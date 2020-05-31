(in-package :castty)

;;;;;;;;;;;;;;
;;; Scenes ;;;
;;;;;;;;;;;;;;

(defparameter *video-codec-lossless* (list "-c:v" "libx264" "-qp" "0"))

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
