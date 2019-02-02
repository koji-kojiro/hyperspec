(defsystem "hyperspec"
  :author "TANI Kojiro"
  :depends-on (#:drakma #:plump #:str)
  :serial t
  :components ((:file "src/package")
               (:file "src/symbols-map")
               (:file "src/main")))
