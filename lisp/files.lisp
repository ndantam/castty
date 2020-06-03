(in-package :castty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files and Directories ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *workdir* nil)

(defun check-workdir ()
  (assert (probe-file *workdir*)))

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

(defun recdir (&optional file)
  (if file
      (merge-pathnames file (subdir *workdir* "rec"))
      (subdir *workdir* "rec")))

(defun part-file (&key tag number subnumber type)
  (declare (type (or string non-negative-integer null) subnumber)
           (type string tag type)
           (type (or string non-negative-integer number)))
  (make-pathname :name (if subnumber
                           (format nil "~A-~A-~A" tag number subnumber)
                           (format nil "~A-~A" tag number))
                 :type type))

(defun file-parts (pathname)
  (let* ((name (pathname-name pathname))
         (ss (position #\- name :from-end t))
         (s (position #\- name :end  ss  :from-end t))
         ;(part (subseq name (1+ s)))
         )
    (multiple-value-bind (part tag n sn)
        (if s
            (values (subseq name (1+ s) )
                    (subseq name 0  s)
                    (subseq name (1+ s) ss)
                    (subseq name (1+ ss)))
            (values (subseq name (1+ ss))
                    (subseq name 0 ss)
                    (subseq name (1+ ss))
                    nil))
      (values part
              tag
              (parse-integer n)
              (when sn (parse-integer sn))))))


(defun %record-file (file)
  (merge-pathnames file (recdir)))

(defun src-file (&optional file)
  (if file
      (merge-pathnames file (subdir *workdir* "src"))
      (subdir *workdir* "src")))

(defun clip-file (&optional file)
  (if file
      (merge-pathnames file (subdir *workdir* "clip"))
      (subdir *workdir* "clip")))

(defun record-file (what part type)
  (%record-file (make-pathname :name (format nil "~A-~A" what part)
                               :type type)))


(defun ensure-string (thing)
  (etypecase thing
    (string thing)
    (pathname (namestring thing))
    (symbol (string thing))
    (fixnum (format nil "~D" thing))))

(defun record-part (number)
  (ensure-string number))

(defun check-file (file &optional overwrite)
  (when (probe-file file)
    (if overwrite
        (delete-file file)
        (error "Refusing to overwrite `~A'." file))))


(defun clean ()
  (labels ((validate (thing)
             (and (eq :absolute
                      (car (pathname-directory thing))))))

    (when (probe-file (clip-file))
      (uiop/filesystem:delete-directory-tree (clip-file)
                                             :validate #'validate))))




    ;; (values s ss)))

    ;; (if ss
    ;;     (values part
    ;;             (parse-integer (subseq part 0 ss))
    ;;             (parse-integer (subseq part (1+ ss))))
    ;;     (values part (parse-integer part) nil))))



(defun sort-files (thing)
  (let ((thing (etypecase thing
                 (pathname (uiop/filesystem:directory-files thing))
                 (list (copy-list thing)))))
    (sort thing
          (lambda (a b)
            (multiple-value-bind (a-part a-tag a-x a-y) (file-parts a)
              (declare (ignore a-tag))
              (multiple-value-bind (b-part b-tag b-x b-y) (file-parts b)
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
    (multiple-value-bind (part tag n s) (file-parts f)
      (declare (ignore part tag))
      (check-type n non-negative-integer)
      (check-type s (or non-negative-integer null)))))

(defun insert-src (number)
  (let ((files (reverse (sort-files (src-file "*.*")))))
    (check-file-parts files)

    ;; rename
    (dolist (f files)
      (multiple-value-bind (part tag n s) (file-parts f)
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
      (multiple-value-bind (part tag n s) (file-parts f)
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
