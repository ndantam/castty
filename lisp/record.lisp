(in-package :castty)

;;;;;;;;;;;;;;;;;
;;; Recording ;;;
;;;;;;;;;;;;;;;;;

(defun record-audio (&key file scene)
  (ffmpeg (list "-f" (scene-parameter scene :audio-device)
                "-i" (scene-parameter scene :audio-input)
                "-ac" "1"
                "-codec:a" "pcm_s16le"
                file)
          :wait nil
          :pasuspend (scene-parameter scene :audio-pasuspend)))


(defun record-video (&key scene draw-mouse output)
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
          :if-output-exists :append))


(defun record (&key
                 (scene :default)
                 number
                 subnumber
                 (audio t)
                 (video t)
                 overwrite
                 (draw-mouse nil))
  (check-workdir)
  (load-scenes)
  (let ((video-file (rec-file (part-file :tag "video"
                                         :number number
                                         :subnumber subnumber
                                         :junk "nut"
                                         :type "zst")))
        (audio-file (rec-file (part-file :tag "audio"
                                         :number number
                                         :subnumber subnumber
                                         :type "wav")))
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
