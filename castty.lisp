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
(defvar *video-input* ":0.0")

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

(defun ffmpeg (&rest args)
  (sb-ext:run-program "ffmpeg"
                      (apply #'merge-args
                             "-hide_banner" "-nostats"
                             ;"-loglevel" "info"
                             args)
                      :output *standard-output*
                      ;:output "/dev/pts/15"
                      ;if-output-exists :append
                      :error *error-output*
                      :search t))


(defun record-audio (&key number scene)
  ;; TODO: pasuspend
  (ffmpeg "-f" (scene-parameter scene :audio-device)
        "-i" (scene-parameter scene :audio-input)
        "-ac" "1"
        "-codec:a" "pcm_s16le"
        (record-file "audio" (record-part number) "wav")))




(defun record (&key
                 (number)
                 (audio t)
                 (video t)
                 (draw-mouse nil))

  (let ((wait-pids))

    ;; Setup


    ;; Recording


    ;; Cleanup
  ))
