;;; A system with dependencies missing between its files, which nonetheless builds
;;; sequentially because of the order of its components. POIU must build it correctly.
(asdf:defsystem "poiu-missing-dependency-target"
  :components
  ((:file "package")
   (:file "macros" :depends-on ("package"))
   ;; Missing dependency on "macros", whose macro TWICE it uses.
   (:file "use-macro" :depends-on ("package"))
   (:file "specials" :depends-on ("package"))
   ;; Missing dependency on "specials", whose special variable *FACTOR* it binds.
   (:file "use-special" :depends-on ("package"))
   (:file "package-2" :depends-on ("package"))
   ;; Missing dependency on "package-2", whose package it reads symbols from.
   (:file "use-package" :depends-on ("package"))))
