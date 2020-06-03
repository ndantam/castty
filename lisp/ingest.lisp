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


(defun ingest-audio (recfile &key overwrite)
  (let ((srcfile (src-file (make-pathname :name (pathname-name recfile) :type "flac"))))
    (check-file srcfile overwrite)
    (let ((process (ffmpeg (list "-i" recfile
                                 "-codec:a" "flac"
                                 srcfile)
                           :wait t)))
      (check-ffmpeg-result process))))


(defun ingest-video (recfile &key overwrite)
  (let* ((n (pathname-name recfile))
         (e (position #\. n :from-end t))
         (nn (subseq n 0 e))
         (srcfile (src-file (make-pathname :name nn :type "mkv")))
         (zstd-proc)
         (ffmpeg-proc))
    (check-file srcfile overwrite)
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
           (setq ffmpeg-proc
                 (ffmpeg (list "-i" "-"
                               *video-codec-lossless*
                               srcfile)
                         :input (sb-ext:process-output zstd-proc)
                         :wait t))
           (check-ffmpeg-result ffmpeg-proc)
           (sb-ext:process-wait zstd-proc)
           (check-zstd-result zstd-proc))
      ;; cleanup
      (process-cleanup zstd-proc))))


(defun ingest-clip (audio)
  (flet ((h (args)
           (check-ffmpeg-result (ffmpeg args :wait t)))
         (f (thing part type)
           (merge-pathnames (make-pathname :name (format nil "~A-~A" thing part)
                                           :type type)
                            audio)))
  (let* ((part (file-parts audio))
         ;(type (pathname-type srcfile))
         ;; (audio (merge-pathnames (make-pathname :name (format nil "audio-~A" part)
         ;;                                        :type "flac")
         ;;                         srcfile))
         (video (f "video" part "mkv"))
         (ss (f "video" part "png") )
         (bg (f "bg" part "png") )
         (clip (clip-file (make-pathname :name (format nil "clip-~A" part)
                                         :type "mkv"))))
    (unless (probe-file clip)
        (cond
          ((and (probe-file video) (probe-file bg))
           (h (list "-i" video  "-i" bg "-i" audio
                    ; TODO: parameters for size and offset
                    "-filter_complex"
                    "[0:v]scale=1440:810[vscale];[1:v][vscale]overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2+75[outv]"
                    "-map" "[outv]:v:0" "-map" "2:a:0"
                    *video-codec-lossless*
                    "-c:a" "copy"
                    clip)))
          ((probe-file video)
           (h (list "-i" video "-i" audio
                    "-map" "0:v:0" "-map" "1:a:0"
                    "-c:v" "copy" "-c:a" "copy"
                    clip)))
          ((probe-file ss)
           (h (list "-i" ss "-i" audio
                    "-map" "0:v:0" "-map" "1:a:0"
                    *video-codec-lossless*
                    "-c:a" "copy"
                    clip)))
          (t (error "No video for `~A'" audio)))))))


(defun clip ()
  (assert *workdir*)
  (ensure-directories-exist (clip-file))
  (labels ((ls (f) (directory (src-file f))))
    (pdolist (audio (ls #P"audio-*.flac"))
      (ingest-clip audio))))

(defun ingest (&key overwrite)
  (check-workdir)
  (ensure-directories-exist (src-file))
  (flet ((ls (f) (directory (recdir f))))
    (pdolist (f (append (ls #P"*.wav") (ls #P"*.nut.zst")))
      (cond
        ;; AUDIO
        ((string= "wav" (pathname-type f))
         (ingest-audio f :overwrite overwrite))
        ;; VIDEO
        ((string= "zst" (pathname-type f))
         (ingest-video f :overwrite overwrite))
        ;; ERROR
        (t (error "Unrecognized media type `~A'" f))))))