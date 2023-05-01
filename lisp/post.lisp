(in-package :castty)



(defun %post (&key scene)
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
    ;; TODO: Variable Frame Rate (-vsync vfr)

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

    ;; Filter and transcode
    ;; ---------------------
    (transcode file-cat :scene scene :overwrite t)))


(defun post (&key (ingest t) (clip t) (post t) overwrite scene)
  (when ingest
    (ingest :overwrite overwrite))
  (when clip
    (clip :overwrite overwrite))
  (when post
    (%post :scene scene)))
