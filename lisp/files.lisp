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
