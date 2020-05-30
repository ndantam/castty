(defpackage castty
  (:use :cl :alexandria)
  (:export))

(in-package :castty)

;;;;;;;;;;;;;;
;;; Scenes ;;;
;;;;;;;;;;;;;;

(defparameter *base-scene*
  '((:audio-device "pulse")
    (:audio-input "default")
    (:audio-pasuspend nil)
    (:audio-filter "highpass=f=80,lowpass=f=1200,loudnorm")

    (:video-fps 15)
    (:video-device "x11grab")
    (:video-size "1920x1080")))

(defvar *scenes* nil)

(defun scene-parameter (scene parameter)
  (flet ((default ()
           (if-let ((elt (assoc parameter *base-scene*)))
             (cdr elt)
             (error "Could not lookup `~A'" parameter))))
    (if-let ((scene-alist (assoc scene  *scenes*)))
      (if-let ((el (assoc parameter  (cdr scene-alist))))
        (cdr el)
        (default))
      (default))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files and Directories ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *workdir* nil)

(defun check-workdir ()
  (assert (probe-file *workdir*)))

(defun subdir (pathname subdirectory)
  (let ((pathname (pathname pathname)))
    ;; ensure not a file
    (assert (null (pathname-name pathname)))
    (make-pathname :directory (append (pathname-directory pathname)
                                      (ensure-list subdirectory))
                   :defaults pathname)))


(defvar *configdir* (subdir (user-homedir-pathname)
                            '(".config" "castty")))

(defun load-scenes (&optional file)
  (let ((file (or file
                  (merge-pathnames (make-pathname :name "scene-alist"
                                                  :type "lisp")
                                   *configdir*))))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (labels ((h (rest)
                   (if-let ((scene (read s nil nil)))
                     (h (cons scene rest))
                     rest)))
        (setf *scenes* (h nil)))))))

(defun recdir (&optional file)
  (if file
      (merge-pathnames file (subdir *workdir* "rec"))
      (subdir *workdir* "rec")))

(defun %record-file (file)
  (merge-pathnames file (recdir)))

(defun src-file (&optional file)
  (if file
      (merge-pathnames file (subdir *workdir* "src"))
      (subdir *workdir* "src")))


(defun record-fifo ()
  (make-pathname :name "video"
                 :type "fifo"
                 :directory (pathname-directory *workdir*)))

(defun record-file (what part type)
  (%record-file (make-pathname :name (format nil "~A-~A" what part)
                               :type type)))


(defun ensure-string (thing)
  (etypecase thing
    (string thing)
    (pathname (namestring thing))
    (symbol (string thing))
    (fixnum (format nil "~D" thing))))

(defun record-part (number)
  (ensure-string number))

(defun check-file (file &optional overwrite)
  (when (probe-file file)
    (if overwrite
        (delete-file file)
        (error "Refusing to overwrite `~A'." file))))

;;;;;;;;;;;;;;;;;
;;; Recording ;;;
;;;;;;;;;;;;;;;;;


(defun merge-args (&rest args)
  (labels ((%merge-args (args)
             (when args
               (merge-arg1 (first args)
                           (%merge-args (rest args)))))
           (merge-arg1 (first rest)
             (if (listp first)
                 (reduce #'merge-arg1 first
                         :initial-value rest
                         :from-end t)
                 (cons (ensure-string first)
                       rest))))
    (%merge-args args)))

(defun process-cleanup (process)
  (when process
    (when (sb-ext:process-alive-p process)
      (sb-ext:process-kill process 9)
      (sb-ext:process-wait process))
    (sb-ext:process-close process)))

(defun zstd (&key
               (mode :compress)
               input
               output
               overwrite
               wait
               fast)
  (let ((args (merge-args (when fast "--fast")
                          (ecase mode
                            (:compress "--compress")
                            (:decompress "--decompress"))
                          "-")))
    (sb-ext:run-program "zstd" args
                        :wait wait
                        :input input
                        :if-input-does-not-exist  :error
                        :output output
                        :if-output-exists (if overwrite :supersede :error)
                        :search t
                        :error *error-output*)))

(defun check-zstd-result (process)
  (assert (not (sb-ext:process-alive-p process)))
  (let ((r (sb-ext:process-exit-code process)))
  (unless (zerop r)
    (error "Zstd returned `~A'" r))))


(defun ffmpeg (args &key
                      wait
                      pasuspend
                      input
                      (output *standard-output*)
                      (if-output-exists :error))
  (let ((args (apply #'merge-args
                     "-hide_banner" "-nostats"
                     args)))
    (multiple-value-bind (program args)
        (if pasuspend
            (values "pasuspender" (list* "--" "ffmpeg" args))
            (values "ffmpeg" args))
      (sb-ext:run-program program args
                          :output output
                          :wait wait
                          :input input
                          :error *error-output*
                          :if-output-exists if-output-exists
                          :search t))))

(defun check-ffmpeg-result (process)
  (assert (not (sb-ext:process-alive-p process)))
  (let ((r (sb-ext:process-exit-code process)))
  (unless (or (zerop r) (= r 255))
    (error "FFmpeg returned `~A'" r))))

(defun record-audio (&key file scene)
  (ffmpeg (list "-f" (scene-parameter scene :audio-device)
                 "-i" (scene-parameter scene :audio-input)
                 "-ac" "1"
                 "-codec:a" "pcm_s16le"
                 file)
           :pasuspend (scene-parameter scene :audio-input)))


(defun record-video (&key scene draw-mouse output)
  (ffmpeg (list "-f" (scene-parameter scene :video-device)
                 "-video_size" (scene-parameter scene :video-size)
                 "-i" (scene-parameter scene :video-input)
                 "-draw_mouse" (if draw-mouse "1" "0")
                 "-framerate" (scene-parameter scene :video-fps)
                 "-codec:v" "rawvideo"
                 "-f" "nut"
                 "-" )
           :output output
           :if-output-exists :append))


(defun record (&key
                 (scene :default)
                 (number)
                 (audio t)
                 (video t)
                 overwrite
                 (draw-mouse nil))
  (check-workdir)
  (load-scenes)
  (let ((video-file (record-file "video"
                                 (format nil "~A.nut"
                                         (record-part number))
                                 "zst"))
        (audio-file (record-file "audio" (record-part number) "wav"))
        (proc-zstd)
        (proc-ffmpeg))
    (unwind-protect
         (progn
           ;; Checks
           ;; ------
           (when video (check-file video-file overwrite))
           (when audio (check-file audio-file overwrite))

           ;; Setup
           ;; -----
           ;; start zstd
           (when video
             (format t "Starting zstd...~%")
             (setq proc-zstd
                   (zstd :mode :compress
                         :input :stream
                         :output video-file)))

           ;; Recording
           ;; ---------
           ;; video
           (when video
             (format t "Starting video...~%")
             (push (record-video :scene scene
                                 :draw-mouse draw-mouse
                                 :output (sb-ext:process-input proc-zstd))
                   proc-ffmpeg))

           ;; audio
           (when audio
             (format t "Starting audio...~%")
             (push (record-audio :file audio-file :scene scene)
                   proc-ffmpeg))


           ;; Normal Cleanup
           ;; --------------
           (format t "Waiting...~%")
           (dolist (p proc-ffmpeg)
             (sb-ext:process-wait p)
             (check-ffmpeg-result p))
           ;; Our current process has Zstd's stdin open.  Must close for
           ;; Zstd to exit when the FFMPEG process exits.
           (when proc-zstd
             (close (sb-ext:process-input proc-zstd))
             (sb-ext:process-wait proc-zstd)
             (check-zstd-result proc-zstd))

           ;; no value
           (values))

      ;; Cleanup, Dammit
      ;; ---------------
      (progn
        (map nil #'process-cleanup proc-ffmpeg)
        (process-cleanup proc-zstd)))))

;;;;;;;;;;;;;;;;;;
;;; Processing ;;;
;;;;;;;;;;;;;;;;;;

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
                               "-codec:v" "libx264" "-qp" "0"
                               srcfile)
                         :input (sb-ext:process-output zstd-proc)
                         :wait t))
           (check-ffmpeg-result ffmpeg-proc)
           (sb-ext:process-wait zstd-proc)
           (check-zstd-result zstd-proc))
      ;; cleanup
      (process-cleanup zstd-proc))))


(defun ingest (&key overwrite)
  (check-workdir)
  (ensure-directories-exist (src-file))
  (flet ((ls (f) (directory (recdir f))))
    ;; AUDIO
    (map nil (lambda (f)
               (ingest-audio f :overwrite overwrite))
         (ls #P"*.wav"))
    ;; VIDEO
    (map nil (lambda (f)
               (ingest-video f :overwrite overwrite))
         (ls #P"*.nut.zst"))))
