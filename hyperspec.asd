(defsystem "hyperspec"
  :author "TANI Kojiro"
  :depends-on (#:drakma #:plump #:str #:cl-ppcre #:unix-opts)
  :serial t
  :components ((:file "src/package")
               (:file "src/symbols-map")
               (:file "src/main")
               (:file "src/command-line")))
