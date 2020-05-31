(in-package :castty)

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


;; Quirk: reading from slime's standard input may do bad things and/or
;; hang
(defun ffmpeg (args &key
                      wait
                      pasuspend
                      input
                      (print-command t)
                      ;(output *standard-output*)
                      output
                      (if-output-exists :error))
  (let ((args (apply #'merge-args
                     "-hide_banner" "-nostats"
                     "-loglevel" "warning"
                     args)))
    (multiple-value-bind (program args)
        (if pasuspend
            (values "pasuspender" (list* "--" "ffmpeg" args))
            (values "ffmpeg" args))
      (when print-command
        (format print-command
                "~&~A ~{~A~^ ~}~@[ &~]~%" program args (not wait)))
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
