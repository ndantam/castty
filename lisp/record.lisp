(in-package :castty)

;;;;;;;;;;;;;;;;;
;;; Recording ;;;
;;;;;;;;;;;;;;;;;

(defun record-audio (&key file scene overwrite)
  (ffmpeg (list "-f" (scene-parameter scene :audio-device)
                (when-let ((sample-rate (scene-parameter scene :audio-record-sample-rate)))
                  (list "-sample_rate" sample-rate))
                "-i" (scene-parameter scene :audio-input)
                "-ac" "1"
                "-codec:a" (scene-parameter scene :audio-record-codec)
                file)
          :wait nil
          :overwrite overwrite
          :pasuspend (scene-parameter scene :audio-pasuspend)))


(defun record-video (&key scene draw-mouse output overwrite)
  (ffmpeg (list "-f" (scene-parameter scene :video-device)
                "-video_size" (scene-parameter scene :video-size)
                "-framerate" (scene-parameter scene :video-fps)
                "-draw_mouse" (if draw-mouse "1" "0")
                "-i" (scene-parameter scene :video-input)
                "-codec:v" "rawvideo"
                "-f" "nut"
                "-" )
          :wait nil
          :output output
          :overwrite overwrite
          :if-output-exists :append))

(defun record-screenshot (&key file scene (wait t) overwrite)
  (ffmpeg (list "-f" (scene-parameter scene :video-device)
                "-video_size" (scene-parameter scene :video-size)
                "-framerate" 30
                "-draw_mouse" 0
                "-i" (scene-parameter scene :video-input)
                "-vframes" 1
                file)
          :overwrite overwrite
          :wait wait))


(defun %record (&key scene video-file audio-file image-file draw-mouse overwrite)
  (let  ((proc-zstd)
         (proc-ffmpeg)
         (proc-kill))
    (unwind-protect
         (progn
           ;; Setup
           ;; -----
           ;; start zstd
           (when video-file
             (format t "Starting zstd...~%")
             (setq proc-zstd
                   (zstd :mode :compress
                         :input :stream
                         :output video-file)))

           ;; Recording
           ;; ---------
           (when image-file
             (format t "Capturing screenshot...~%")
             (push (record-screenshot :scene scene
                                      :file image-file
                                      :wait nil
                                      :overwrite overwrite)
                   proc-ffmpeg))

           ;; video
           (when video-file
             (format t "Starting video...~%")
             (let ((p (record-video :scene scene
                                    :draw-mouse draw-mouse
                                    :output (sb-ext:process-input proc-zstd)
                                    :overwrite overwrite)))
               (push p proc-ffmpeg)
               (push p proc-kill)))



           ;; audio
           (when audio-file
             (format t "Starting audio...~%")
             (let ((p (record-audio :file audio-file
                                 :scene scene
                                 :overwrite overwrite)))
               (push p proc-ffmpeg)
               (push p proc-kill)))

           ;; Write PIDs
           ;; ----------
           (with-open-file (s *pidfile* :direction :output
                              :if-exists :error :if-does-not-exist :create)
             (dolist (p proc-kill)
               (format s "~D~%" (sb-ext:process-pid p))))


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

(defun record (&key
                 (scene :default)
                 number
                 (audio t)
                 (video t)
                 overwrite
                 (draw-mouse nil))
  (declare (type (or null (eql t))
                 audio overwrite draw-mouse)
           (type (or null (eql t)
                     (eql :background)
                     (eql :bg)
                     (eql :screenshot)
                     (eql :ss))
                 video))

  (check-workdir)
  (load-scenes)
  (ensure-directories-exist (rec-file))
  (when (probe-file *pidfile*)
    (error "PID file exists.  Is recording still active?"))
  (let* ((subnumber  (etypecase number
                       (number nil)
                       (list (second number))))
         (number (etypecase number
                   (number number)
                   (list (car number))))
         (video-file (when (eq video t)
                      (rec-file (part-file :tag "video"
                                           :number number
                                           :subnumber subnumber
                                           :junk "nut"
                                           :type "zst"))))
        (image-file (when (and video (not (eq video t)))
                      (rec-file (part-file :tag (cond ((find video '(:background :bg))
                                                       "bg")
                                                      ((find video '(:ss :screenshot))
                                                       "video")
                                                      (t (error "Unknown video type: ~A"
                                                                video)))
                                           :number number
                                           :subnumber subnumber
                                           :type "png"))))

         (audio-file (when audio (rec-file (part-file :tag "audio"
                                                      :number number
                                                      :subnumber subnumber
                                                      :type "wav")))))
    (when audio-file (check-file audio-file overwrite))
    (when image-file (check-file image-file overwrite))
    (when video-file (check-file video-file overwrite))
    (%record :scene scene
             :video-file video-file
             :audio-file (when audio audio-file)
             :image-file image-file
             :overwrite overwrite
             :draw-mouse draw-mouse)))
