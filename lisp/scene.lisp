(in-package :castty)

;;;;;;;;;;;;;;
;;; Scenes ;;;
;;;;;;;;;;;;;;

(defparameter *video-codec-lossless* (list "-c:v" "libx264" "-qp" "0"))

(defparameter *base-scene*
  '((:audio-device . "pulse")
    (:audio-input . "default")
    (:audio-record-codec . "pcm_s24le")
    (:audio-record-sample-rate . nil)
    (:audio-pasuspend nil)
    (:audio-filter . "highpass=f=100,lowpass=f=10000,loudnorm")

    (:video-fps . 30)
    (:video-device . "x11grab")
    (:video-size . "1920x1080")))

(defvar *scenes* nil)

(defun scene-parameter-env (parameter)
  "Lookup parameter in environment"
  (uiop/os:getenv (concatenate 'string
                               "CASTTY_PARAM_"
                               (string-upcase (substitute #\_ #\-
                                                          (string parameter))))))

(defun scene-parameter (scene parameter)
  (let ((scene (or scene :default)))
    (labels ((lookup-env (continuation)
               (if-let ((env (scene-parameter-env parameter)))
                 env
                 (funcall continuation)))
             (lookup-scene (scene continuation)
               (if-let ((el (assoc parameter  scene )))
                 (cdr el)
                 (funcall continuation)))
             (lookup-sym (symbol continuation)
               (if-let ((scene-alist (assoc symbol  *scenes*)))
                 (lookup-scene (cdr scene-alist) continuation)
                 (funcall continuation)))
             (default ()
               (lookup-sym :default  #'base))
             (base ()
               (lookup-scene *base-scene*
                             (lambda  () (error "Could not lookup `~A'" parameter)))))
      (lookup-env (lambda () (lookup-sym scene #'default))))))
