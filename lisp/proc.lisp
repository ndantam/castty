(in-package :castty)

(defun ensure-string (thing)
  (etypecase thing
    (string thing)
    (pathname (namestring thing))
    (symbol (string thing))
    (fixnum (format nil "~D" thing))))

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

(defun check-process-result (process &key name)
  (assert (not (sb-ext:process-alive-p process)))
  (let ((r (sb-ext:process-exit-code process)))
    (unless (zerop r)
      (error "~A returned `~A'" (or name "Process") r))))

(defun check-zstd-result (process)
  (check-process-result process :name "Zstd"))

(defun check-ffmpeg-result (process)
  (assert (not (sb-ext:process-alive-p process)))
  (let ((r (sb-ext:process-exit-code process)))
    (unless (or (zerop r) (= r 255))
      (error "FFmpeg returned `~A'" r))))


(defun run-program (program args
                    &key
                      wait
                      input
                      (if-input-does-not-exist :error)
                      output
                      (if-output-exists :error)
                      (print-command t))
  (when print-command
    (format print-command
            "~&~A ~{~A~^ ~}~@[ &~]~%" program args (not wait)))
  (sb-ext:run-program program args
                      :input input
                      :if-input-does-not-exist if-input-does-not-exist
                      :output output
                      :if-output-exists if-output-exists
                      :wait wait
                      :error *error-output*
                      :search t))

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
    (run-program "zstd" args
                 :wait wait
                 :input input
                 :output output
                 :if-output-exists (if overwrite :supersede :error))))

;; Quirk: reading from slime's standard input may do bad things and/or
;; hang
(defun ffmpeg (args &key
                      wait
                      pasuspend
                      input
                      (print-command t)
                      ;(output *standard-output*)
                      overwrite
                      output
                      (if-output-exists :error))
  (let ((args (apply #'merge-args
                     "-hide_banner" "-nostats"
                     "-loglevel" "warning"
                     (when overwrite "-y")
                     args)))
    (multiple-value-bind (program args)
        (if pasuspend
            (values "pasuspender" (list* "--" "ffmpeg" args))
            (values "ffmpeg" args))

      (let ((proc (run-program program args
                               :output output
                               :wait wait
                               :input input
                               :print-command print-command
                               :if-output-exists if-output-exists)))
        (if wait
            (check-process-result proc :name "FFmpeg")
            proc)))))
