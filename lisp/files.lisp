(in-package :castty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files and Directories ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *workdir* nil)

(defun check-workdir ()
  (let ((workdir (etypecase *workdir*
                   (pathname *workdir*)
                   (string (pathname *workdir*)))))
   (assert (null (pathname-name workdir)))
   (assert (null (pathname-type workdir)))
  ;(assert (probe-file *workdir*))
   (ensure-directories-exist *workdir*)
   (values)))

(defun subdir (pathname subdirectory)
  (let ((pathname (pathname pathname)))
    ;; ensure not a file
    (assert (null (pathname-name pathname)))
    (make-pathname :directory (append (pathname-directory pathname)
                                      (ensure-list subdirectory))
                   :defaults pathname)))


(defvar *configdir* (subdir (user-homedir-pathname)
                            '(".config" "castty")))

(defun load-scenes (&optional file)
  (let ((file (or file
                  (merge-pathnames (make-pathname :name "scene-alist"
                                                  :type "lisp")
                                   *configdir*))))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (labels ((h (rest)
                   (if-let ((scene (read s nil nil)))
                     (h (cons scene rest))
                     rest)))
        (setf *scenes* (h nil)))))))

(defun work-subdir (subdir &optional file)
  (if file
      (merge-pathnames file (subdir *workdir* subdir))
      (subdir *workdir* subdir)))

(defun rec-file (&optional file)
  (work-subdir "rec" file))
(defun src-file (&optional file)
  (work-subdir "src" file))
(defun clip-file (&optional file)
  (work-subdir "clip" file))
(defun out-file (&optional file)
  (work-subdir "out" file))
(defun tmp-file (&optional file)
  (work-subdir "tmp" file))

;;; TAG-NUMBER[-SUBNUMBER][.JUNK].TYPE

(defun part-file (&key tag part number subnumber type defaults junk)
  (declare (type (or string non-negative-integer null) number subnumber)
           (type string tag type)
           (type (or string null) part junk)
           (type (or pathname null) defaults))
  (let* ((name
          (cond
            ((and part
                  (null number)
                  (null subnumber))
             (format nil "~A-~A" tag part))
            ((and number subnumber)
             (format nil "~A-~A-~A" tag number subnumber))
            (number
             (format nil "~A-~A" tag number))
            (t
             (error "Can't create part-file from arguments."))))
         (pathname (make-pathname :name (if junk
                                            (format nil "~A.~A" name junk)
                                            name)
                                  :type type)))
    (if defaults
        (merge-pathnames pathname defaults)
        pathname)))

(defun file-parts (pathname)
  (let* ((name (pathname-name pathname))
         (end (position #\. name))
         (ss (position #\- name :from-end t :end end))
         (s (position #\- name :end  ss  :from-end t))
         ;(part (subseq name (1+ s)))
         )
    (multiple-value-bind (part tag n sn)
        (if s
            (values (subseq name (1+ s) end)
                    (subseq name 0 s)
                    (subseq name (1+ s) ss)
                    (subseq name (1+ ss) end))
            (values (subseq name (1+ ss) end)
                    (subseq name 0 ss)
                    (subseq name (1+ ss) end)
                    nil))
      (values tag
              part
              (parse-integer n)
              (when sn (parse-integer sn))))))


(defun part-file-part (pathname)
  (multiple-value-bind (tag part n sn) (file-parts pathname)
    (declare (ignore tag n sn))
    part))

(defun check-file (file &optional overwrite)
  (when (probe-file file)
    (if overwrite
        (delete-file file)
        (error "Refusing to overwrite `~A'." file))))


(defun clean ()
  (labels ((validate (thing)
             (and (eq :absolute
                      (car (pathname-directory thing)))))
           (clean-path (path)
             (when (probe-file path)
               (uiop/filesystem:delete-directory-tree path :validate #'validate))))

    (clean-path (clip-file))
    (clean-path (out-file))
    (values)))



(defun sort-files (thing)
  (let ((thing (etypecase thing
                 (pathname (uiop/filesystem:directory-files thing))
                 (list (copy-list thing)))))
    (sort thing
          (lambda (a b)
            (multiple-value-bind (a-tag a-part a-x a-y) (file-parts a)
              (declare (ignore a-tag))
              (multiple-value-bind (b-tag b-part b-x b-y) (file-parts b)
                (declare (ignore b-tag))
                (cond
                  ((and a-x b-x (not (eql a-x b-x)))
                   (< a-x b-x))
                  ((and a-y b-y (not (eql a-y b-y)))
                   (assert (and a-x b-x))
                   (< a-y b-y))
                  (t
                   (string< a-part b-part)))))))))

(defun check-file-parts (files)
  (dolist (f files)
    (multiple-value-bind (tag part n s) (file-parts f)
      (declare (ignore part tag))
      (check-type n non-negative-integer)
      (check-type s (or non-negative-integer null)))))

(defun insert-src (number)
  (let ((files (reverse (sort-files (src-file "*.*")))))
    (check-file-parts files)

    ;; rename
    (dolist (f files)
      (multiple-value-bind (tag part n s) (file-parts f)
        (declare (ignore part))
        (when (>= n number)
          (let ((g (src-file (part-file :tag tag
                                        :number (1+ n)
                                        :subnumber s
                                        :type (pathname-type f)))))
            (check-file g nil)
            (rename-file f g)))))

    ;; end
    (values)))

(defun remove-src (number)
  (let ((files (sort-files (src-file "*.*"))))
    (check-file-parts files)

    ;; rename
    (dolist (f files)
      (multiple-value-bind (tag part n s) (file-parts f)
        (declare (ignore part))
        (when (> n number)
          (let ((g (src-file (part-file :tag tag
                                        :number (1- n)
                                        :subnumber s
                                        :type (pathname-type f)))))
            (check-file g nil)
            (rename-file f g)))))

    ;; end
    (values)))


(defun some-newer (target prerequisites)
  (or (not (probe-file target))
      (let ((mtime (file-write-date target)))
        (some (lambda (p)
                (< mtime (file-write-date p)))
          (ensure-list prerequisites)))))

(defmacro when-newer ((target prerequisites) &body body)
  `(when (some-newer ,target ,prerequisites)
    ,@body))
