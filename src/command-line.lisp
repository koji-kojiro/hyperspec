(in-package :hyperspec)

(opts:define-opts
  (:name :help
   :description "Print this help and exit."
   :short #\h
   :long "help")
  (:name :url
   :description "Print url instead of document."
   :short #\u
   :long "url")
  (:name :nocolor
   :description "Do not use ANSI escapes."
   :short #\n
   :long "nocolor"))

(defun help ()
  (opts:describe :args "symbol" :usage-of "clhs")
  (opts:exit 1))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it ,@body)))

(defun main (argv)
  (multiple-value-bind (options args)
    (handler-case
      (if argv (opts:get-opts argv) (opts:get-opts))
      (error () (help)))
    (unless args (help))
    (when (cdr args) (help))
    (when-option (options :help) (help))
    (handler-case
      (progn
        (when-option (options :url)
          (format t "~a~%" (hs:get-url (car args)))
          (opts:exit))
        (hs:show (car args) :use-color (null (getf options :nocolor))))
      (condition (c)
        (format t "Error: ~a~%" c)
        (opts:exit 1)))))


