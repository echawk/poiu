(asdf:defsystem "poiu-test-target"
  :depends-on ("poiu")
  :components
  ((:file "package")
   (:file "file-a" :depends-on ("package"))
   (:file "file-b" :depends-on ("package"))
   (:file "file-c" :depends-on ("package"))
   (:file "file-d" :depends-on ("package"))
   (:file "final" :depends-on ("file-a" "file-b" "file-c" "file-d"))))
