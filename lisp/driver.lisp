(in-package :castty)


(defun driver ()
  (load-scenes)
  (setq *workdir* (uiop/os:getcwd)) ; must setq to be valid in other threads
  (let ((command (string-upcase (uiop/os:getenv "CASTTY_COMMAND")))
        (scene (when-let ((scene-name (string-upcase (uiop/os:getenv "CASTTY_SCENE"))))
                 (intern scene-name :keyword))))
    (format t "workdir: ~A~&" *workdir*)
    (format t "command: ~A~&" command)
    (format t "scene: ~A~&" scene)
    (cond
      ((string= "POST" command)
       (check-workdir)
       (post))
      ((string= "CLEAN" command)
       (check-workdir)
       (clean))
      ((string= "TRANSCODE" command)
       (transcode (uiop/os:getenv "CASTTY_INPUT") :overwrite t :scene scene))
      (t
       (format *error-output* "~&ERROR: Unknown command `~A'~&" command)))))
