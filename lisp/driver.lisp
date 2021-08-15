(in-package :castty)


(defun driver ()
  (setq *workdir* (uiop/os:getcwd)) ; must setq to be valid in other threads
  (let ((command (string-upcase (uiop/os:getenv "CASTTY_COMMAND"))))
    (check-workdir)
    ;;(format t "workdir: ~A~&" *workdir*)
    ;;(format t "command: ~A~&" command)
    (cond
      ((string= "POST" command)
       (post)
       )
      (t
       (format *error-output* "~&ERROR: Unknown command `~A'~&" command)))))
