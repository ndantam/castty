(defpackage castty
  (:use :cl :alexandria)
  (:export))

(in-package :castty)

;;;;;;;;;;;;;;
;;; Scenes ;;;
;;;;;;;;;;;;;;

(defvar *audio-device* "pulse")
(defvar *audio-input* "default")
(defvar *audio-pasuspend* nil)
(defvar *audio-filter* "highpass=f=80,lowpass=f=1200,loudnorm")

(defvar *video-fps* 15)
(defvar *video-device* "x11grab")
(defvar *video-size* "1920x1080")

(defun scene-parameter (scene parameter)
  (if (null scene)
      (ecase parameter
        (:audio-device *audio-device*)
        (:audio-input *audio-input*)
        (:audio-pasuspend *audio-pasuspend*)
        (:audio-filter *audio-filter*)
        (:video-fps *video-fps*)
        (:video-device *video-device*)
        (:video-size *video-size*)
        (:video-input *video-input*))
      (error 'unimplemented)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files and Directories ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *workdir* nil)

(defun subdir (pathname subdirectory)
  (let ((pathname (pathname pathname)))
    ;; ensure not a file
    (assert (null (pathname-name pathname)))
    (make-pathname :directory (append (pathname-directory pathname)
                                      (ensure-list subdirectory))
                   :defaults pathname)))

(defun %record-file (file)
  (merge-pathnames file (subdir *workdir* "rec")))


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

(defun %ffmpeg (args &key
                       (output *standard-output*)
                       (if-output-exists :error))

  (sb-ext:run-program "ffmpeg"
                      (apply #'merge-args
                             "-hide_banner" "-nostats"
                             args)
                      :output output
                      :wait nil
                      :error *error-output*
                      :if-output-exists if-output-exists
                      :search t))

(defun ffmpeg (&rest args)
  (%ffmpeg args))


(defun record-audio (&key file scene)
  ;; TODO: pasuspend
  (ffmpeg "-f" (scene-parameter scene :audio-device)
          "-i" (scene-parameter scene :audio-input)
          "-ac" "1"
          "-codec:a" "pcm_s16le"
          file))


(defun record-video (&key scene draw-mouse output)
  (%ffmpeg (list "-f" (scene-parameter scene :video-device)
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
                 scene
                 (number)
                 (audio t)
                 (video t)
                 (force)
                 (draw-mouse nil))

  (let ((wait-processes)
        (video-file (record-file "video"
                                 (format nil "~A.nut"
                                         (record-part number))
                                 "zst"))
        (audio-file (record-file "audio" (record-part number) "wav"))
        (proc-zstd))
    (unwind-protect
         (progn
           ;; Checks
           ;; ------
           (flet ((check-file (file)
                    (when (probe-file file))
                      (if force
                          (sb-posix:unlink file)
                          (error "Refusing to overwrite `~A'." file))))
             (when video (check-file video-file))
             (when audio (check-file audio-file)))

           ;; Setup
           ;; -----
           ;; start zstd
           (when video
             (format t "Starting zstd...~%")
             (setq proc-zstd
                   (sb-ext:run-program "zstd"
                                       (merge-args
                                        "--fast" "-")
                                       :wait nil
                                       :input :stream
                                       :output video-file
                                       :search t
                                       :error *error-output*))
             (push proc-zstd wait-processes))

           ;; Recording
           ;; ---------
           ;; video
           (when video
             (format t "Starting video...~%")
             (push (record-video :scene scene
                                 :draw-mouse draw-mouse
                                 :output (sb-ext:process-input proc-zstd))
                   wait-processes))

           ;; audio
           (when audio
             (format t "Starting audio...~%")
             (push (record-audio :file audio-file :scene scene)
                   wait-processes))
           ;; no value
           (values))

      ;; Cleanup
      ;; -------
      (progn
        ;; Our current process has Zstd's stdin open.  Must close for
        ;; Zstd to exit when the FFMPEG process exits.
        (when proc-zstd
          (close (sb-ext:process-input proc-zstd)))
        ;; Wait for processes to finish
        (format t "Waiting...~%")
        (dolist (p wait-processes)
          (sb-ext:process-wait p))
        ;; Remove fifo
        (when (probe-file (record-fifo))
          (sb-posix:unlink (record-fifo)))))))
