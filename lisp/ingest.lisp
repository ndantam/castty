(in-package :castty)

;;;;;;;;;;;;;;;;;;
;;; Processing ;;;
;;;;;;;;;;;;;;;;;;

;; ;; TODO: unwind-protect to cleanup any running processes
;; (defun multi-process (thunks &key (jobs 4))
;;   (labels ((procloop (thunks procs failed)
;;              (cond
;;                ;; Start next process
;;                ((and (not failed)
;;                      thunks
;;                      (< (length procs) jobs))
;;                 (procloop (cdr thunks)
;;                           (cons (funcall (car thunks)) procs)
;;                           failed))
;;                ;; Wait for process
;;                (procs
;;                 (let ((process))
;;                   ;; This is dumb, but SBCL seems to not let us wait()
;;                   ;; for multiple children.  Polling it is.
;;                   (sb-ext:wait-for (setq process
;;                                          (find-if-not #'sb-ext:process-alive-p procs))
;;                                    :timeout 1)
;;                   (if process
;;                       (progn
;;                         (sb-ext:process-wait process)
;;                         (let ((ok (zerop (sb-ext:process-exit-code process))))
;;                           (sb-ext:process-close process)
;;                           (procloop thunks
;;                                     (remove process procs :test #'eq)
;;                                     (or (not ok) failed))))
;;                       ;; repeat
;;                       (procloop thunks procs failed))))

;;                ;; Result
;;                (t
;;                 failed))))

;;     (let ((failed (procloop thunks nil nil)))
;;       (when failed
;;         (error "Multi-processing failed")))))


(defun probe-resolution (file)
  (let ((text (with-output-to-string (s)
                (sb-ext:run-program
                 "sh"
                 (list "-c"
                       (format nil "ffprobe -hide_banner '~A' 2>&1 | grep Video | sed -e 's/.* \\([[:digit:]]\\+\\)x\\([[:digit:]]\\+\\).*/\\1 \\2/'"
                               file))
                 :search t
                 :output s))))
    (multiple-value-bind (width end)
        (parse-integer text :junk-allowed t)
      (list width
            (parse-integer text :start end :junk-allowed t)))))



(defun ingest-audio (recfile &key overwrite)
  (let ((srcfile (src-file (make-pathname :name (pathname-name recfile) :type "flac"))))
    (when-newer (srcfile recfile )
      (ffmpeg (list "-i" recfile
                    "-codec:a" "flac"
                    srcfile)
              :overwrite overwrite
              :wait t))))

(defun ingest-image (recfile &key overwrite)
  (let ((srcfile (src-file (make-pathname :name (pathname-name recfile)
                                          :type (pathname-type recfile)))))
    (when-newer (srcfile recfile )
      (copy-file recfile srcfile
                 :if-to-exists (if overwrite :supersede :error)))))


(defun ingest-video (recfile &key overwrite)
  (let* ((part (part-file-part recfile))
         (srcfile (src-file (part-file :tag "video" :part part :type "mkv"))))
    (when-newer (srcfile recfile)
      (let ((zstd-proc))
                                        ;(check-file srcfile overwrite)
        (unwind-protect
             (progn
               ;; decompress
               (setq zstd-proc
                     (zstd :mode :decompress
                           :input recfile
                           :output :stream
                           :overwrite nil
                           :wait nil))
               ;; encode
               (ffmpeg (list "-i" "-"
                             *video-codec-lossless*
                             srcfile)
                       :overwrite overwrite
                       :input (sb-ext:process-output zstd-proc)
                       :wait t)
               (sb-ext:process-wait zstd-proc)
               (check-zstd-result zstd-proc))
          ;; cleanup
          (process-cleanup zstd-proc))))))


(defun overlay-filter (video)
  (let* ((res (probe-resolution video))
         (aspect-ratio (/ (first res) (second res))))
    (cond
      ((= aspect-ratio 16/9)
       "[0:v]scale=1440:810[vscale];[1:v][vscale]overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2+75[outv]")
       ((= aspect-ratio 8/9)
        "[0:v]scale=720:810[vscale];[1:v][vscale]overlay=(main_w-overlay_w)/2+main_w/4:(main_h-overlay_h)/2+75[outv]")
      (t (error "Cannot handle overlay resolution `~A' of file `~A'"
                res video)))))

(defun ingest-clip (number subnumber)
  (let ((clip (clip-file (part-file :tag "clip" :number number :subnumber subnumber
                                    :type "mkv"))))
    (flet ((h (prerequisites args)
             (when-newer (clip prerequisites)
               (ffmpeg args :wait t)))
           (f (&key tag
                    (number number)
                    (subnumber subnumber)
                    type)
             (let ((file (src-file (part-file :tag tag
                                              :number number
                                              :subnumber subnumber
                                              :type type))))
               (when (probe-file file) file))))
      (let ((video (f :tag "video" :type "mkv"))
            (audio (f :tag "audio" :type "flac"))
            (ss (or (f :tag "video" :type "png")
                    (f :tag "video" :subnumber nil :type "png")))
            (bg (or (f :tag "bg" :type "png")
                    (f :tag "bg" :subnumber nil :type "png")))
            (fps (scene-parameter nil :video-fps)))

        (ensure-directories-exist clip)
        (unless (probe-file clip)
          (cond
            ((and audio video bg)
             (h (list audio video bg)
                (list "-i" video  "-i" bg "-i" audio
                                        ; TODO: parameters for size and offset
                      "-filter_complex" (overlay-filter video)
                      "-map" "[outv]:v:0" "-map" "2:a:0"
                      *video-codec-lossless*
                      "-c:a" "copy"
                      "-r" fps
                      clip)))
            ((and video bg)
             (h (list video bg)
                (list "-i" video  "-i" bg
                                        ; TODO: parameters for size and offset
                      "-filter_complex" (overlay-filter video)
                      "-map" "[outv]:v:0" "-map" "0:a:0"
                      *video-codec-lossless*
                      "-c:a" "copy"
                      "-r" fps
                      clip)))
            ((and audio video)
             (h (list audio video)
                (list "-i" video "-i" audio
                      "-map" "0:v:0" "-map" "1:a:0"
                      "-c:v" "copy" "-c:a" "copy"
                      "-r" fps
                      clip)))
            ((and audio ss)
             (h (list audio ss)
                (list "-i" ss "-i" audio
                      "-map" "0:v:0" "-map" "1:a:0"
                      *video-codec-lossless*
                      "-c:a" "copy"
                      "-r" fps
                      clip)))
            (t (error "Invalid sources for for `~A.~A'" number subnumber))))))))


;; (defun clip ()
;;   (assert *workdir*)
;;   (ensure-directories-exist (clip-file))
;;   (labels ((ls (f) (directory (src-file f))))
;;     (pdolist (audio (ls #P"audio-*.flac"))
;;       (ingest-clip audio))))

(defun clip ()
  (assert *workdir*)
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((ls (f) (directory (src-file f)))
             (h (files)
               (dolist (file files)
                 (multiple-value-bind (tag part number subnumber)
                     (file-parts file)
                   (declare (ignore tag part))
                   (setf (gethash (list number subnumber) hash) t)))))
      (h (ls #P"audio-*.flac"))
      (h (ls #P"video-*.mkv"))
      (h (ls #P"video-*.png")))

    (pdolist (k (hash-table-keys hash))
      (ingest-clip (first k) (second k)))))







  ;; (ensure-directories-exist (clip-file))
  ;;   (pdolist (audio (ls #P"audio-*.flac"))
  ;;     (ingest-clip audio))))


(defun ingest (&key overwrite)
  (check-workdir)
  (ensure-directories-exist (src-file))
  (flet ((ls (f) (directory (rec-file f))))
    (pdolist (f (append (ls #P"*.wav")
                        (ls #P"*.nut.zst")
                        (ls #P"*.png")))
      (cond
        ;; AUDIO
        ((string= "wav" (pathname-type f))
         (ingest-audio f :overwrite overwrite))
        ;; VIDEO
        ((string= "zst" (pathname-type f))
         (ingest-video f :overwrite overwrite))
        ;; IMAGE
        ((string= "png" (pathname-type f))
         (ingest-image f :overwrite overwrite))
        ;; ERROR
        (t (error "Unrecognized media type `~A'" f))))))
