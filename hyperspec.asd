(defsystem "hyperspec"
  :author "TANI Kojiro"
  :depends-on (#:drakma #:plump #:str #:cl-ppcre)
  :serial t
  :components ((:file "src/package")
               (:file "src/symbols-map")
               (:file "src/main")))
