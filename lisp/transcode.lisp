(in-package :castty)



(defun transcode (input-file &key output-pathname scene overwrite)
  ;; collect inputs
  (let* ((output-pathname (or output-pathname
                              (make-pathname :directory (pathname-directory input-file))))
         (file-filtered (merge-pathnames (make-pathname :name "filtered"
                                                        :type "flac")
                                         output-pathname))
         (file-aac (merge-pathnames (make-pathname :name "compressed"
                                                   :type "m4a")
                                    output-pathname))
         (file-hiq (merge-pathnames (make-pathname :name "hi-quality"
                                                   :type "mkv")
                                    output-pathname))
         (file-compressed (merge-pathnames (make-pathname :name "compressed"
                                                          :type "mp4")
                                           output-pathname)))

    (ensure-directories-exist output-pathname)
    ;; TODO: maybe filter audio per-file
    ;; TODO: Variable Frame Rate (-vsync vfr)

    ;; Filter Audio
    ;; ------------
    (when-newer (file-filtered input-file)
      (ffmpeg (list "-i" input-file
                    "-vn"
                    "-codec:a" "flac"
                    "-filter:a" (scene-parameter scene :audio-filter)
                    "-max_muxing_queue_size" 4096
                    file-filtered)
              :overwrite overwrite
              :wait t))

    ;; Combine video and filtered audio
    ;; --------------------------------
    (when-newer (file-hiq (list input-file file-filtered))
      (ffmpeg (list "-i" input-file
                    "-i" file-filtered
                    "-map" "0:v:0" "-map" "1:a:0"
                    "-codec:v" "copy"
                    "-codec:a" "copy"
                    "-max_muxing_queue_size" "4096"
                    file-hiq)
              :overwrite overwrite
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
    (when-newer (file-compressed (list input-file file-aac))
      (ffmpeg (list "-i" input-file
                    "-i" file-aac
                    "-map" "0:v:0" "-map" "1:a:0"
                    "-codec:v" "libx264" "-crf" 23 "-pix_fmt" "yuv420p"
                    "-codec:a" "copy"
                    "-max_muxing_queue_size" "4096"
                    file-compressed)
              :overwrite overwrite
              :wait t))))
