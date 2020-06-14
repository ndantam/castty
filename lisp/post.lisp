(in-package :castty)



(defun post (&key scene)
  (ensure-directories-exist (out-file))
  ;; collect inputs
  (let ((clips (sort-files (uiop/filesystem:directory-files (clip-file))))
        (file-inputs (merge-pathnames "inputs.txt" *workdir*))
        (file-cat (out-file "cat.mkv"))
        (file-filtered (out-file "filtered.flac"))
        (file-hiq (out-file "hi-quality.mkv"))
        (file-aac (out-file "compressed.m4a"))
        (file-compressed (out-file "compressed.mp4")))

    ;; TODO: maybe filter audio per-file

    ;; Concatenate
    ;; -----------
    (when-newer (file-cat clips)
      ;; check
      (dolist (clip clips)
        (multiple-value-bind (tag part n sn) (file-parts clip)
          (declare (ignore part n sn))
          (assert (string= tag "clip"))))

      ;; get inputs
      (with-open-file (s file-inputs :direction :output
                         :if-exists :supersede  :if-does-not-exist :create)
        (dolist (clip clips)
          (format s "file 'clip/~A.~A'~%"
                  (pathname-name clip) (pathname-type clip))))


      (ffmpeg (list "-f" "concat" "-i" file-inputs
                    *video-codec-lossless*
                    "-framerate" (scene-parameter scene :video-fps)
                    "-codec:a" "flac"
                    "-ac" 1
                    "-max_muxing_queue_size" 4096
                    file-cat)
              :overwrite t
              :wait t))

    ;; Filter Audio
    ;; ------------
    (when-newer (file-filtered file-cat)
      (ffmpeg (list "-i" file-cat
                    "-vn"
                    "-codec:a" "flac"
                    "-filter:a" (scene-parameter scene :audio-filter)
                    "-max_muxing_queue_size" 4096
                    file-filtered)
              :overwrite t
              :wait t))

    ;; Combine video and filtered audio
    ;; --------------------------------
    ;; TODO hi-quality
    (when-newer (file-hiq (list file-cat file-filtered))
      (ffmpeg (list "-i" file-cat
                    "-i" file-filtered
                    "-map" "0:v:0" "-map" "1:a:0"
                    "-codec:v" "copy"
                    "-codec:a" "copy"
                    "-max_muxing_queue_size" "4096"
                    file-hiq)
              :overwrite t
              :wait t))


    ;; Compress Audio
    ;; --------------
    (when-newer (file-aac file-filtered)
      (let ((proc-ffmpeg) (proc-fdkaac))
        (unwind-protect
             (progn
               ;; decode
               (setq proc-ffmpeg
                     (ffmpeg (list "-i" file-filtered
                                   "-filter:a" "aresample=48000"
                                   "-codec:a" "pcm_s16le"
                                   "-f" "wav" "-")
                             :output :stream
                             :wait nil))
               ;; encode
               (setq proc-fdkaac
                     (run-program "fdkaac"
                                  (merge-args "--silent"
                                              "--profile" 2
                                              "--bitrate-mode" 4
                                              "-"
                                              "-o" file-aac)
                                  :wait nil
                                  :input (sb-ext:process-output proc-ffmpeg)))
               ;; wait for FFmpeg
               (sb-ext:process-wait proc-ffmpeg)
               (check-process-result proc-ffmpeg)
               (close (sb-ext:process-output proc-ffmpeg))
               (sb-ext:process-close proc-ffmpeg)
               ;; wait for fdkaac
               (sb-ext:process-wait proc-fdkaac)
               (sb-ext:process-close proc-fdkaac))


          ;; cleanup
          (process-cleanup proc-ffmpeg)
          (process-cleanup proc-fdkaac))))

    ;; Compress video and audio
    ;; ------------------------
    (when-newer (file-compressed (list file-cat file-aac))
      (ffmpeg (list "-i" file-cat
                    "-i" file-aac
                    "-map" "0:v:0" "-map" "1:a:0"
                    "-codec:v" "libx264" "-crf" 23
                    "-codec:a" "copy"
                    "-max_muxing_queue_size" "4096"
                    file-compressed)
              :overwrite t
              :wait t))))
