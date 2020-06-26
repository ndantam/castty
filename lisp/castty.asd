(asdf:defsystem castty
  :description "Video maker"
  :depends-on ("alexandria" "anthill")
  :components ((:file "package")
               (:file "scene" :depends-on ("package"))
               (:file "proc" :depends-on ("package"))
               (:file "files" :depends-on ("scene"))
               (:file "record" :depends-on ("files" "proc"))
               (:file "ingest" :depends-on ("files" "proc"))
               (:file "post" :depends-on ("files" "proc"))))
