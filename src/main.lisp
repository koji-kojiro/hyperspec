(in-package :hyperspec)

(defvar *cache* nil)

(defparameter *clhs-base-url*
  "http://www.lispworks.com/documentation/HyperSpec/")

(defun clhs-url (path)
  (format nil "~a~a" *clhs-base-url* path))

(defun retrieve-url (url)
  (multiple-value-bind (body status) (drakma:http-request url)
    (if (= status 200)
        body
        (error "Connection failed with code ~a: ~s" status url))))

(defun extract-text (html)
  (str:unlines
    (remove-if #'str:blank?
      (str:lines
        (with-output-to-string (s)
          (plump:traverse
            (plump:parse html) 
            #'(lambda (n)
                (let ((p (plump:parent n)))
                  (if (and (plump:child-node-p p)
                           (string-equal "PRE" (plump:tag-name p)))
                      (format s "~a~%"
                        (str:unlines
                          (str:add-prefix
                            (remove-if
                              #'str:blank?
                              (str:lines (plump:text n)))
                            "|")))
                      (format s "~a" (plump:text n)))))
            :test #'plump:text-node-p))))))

(defun emphasize (title content use-color)
  (if use-color
      (format t "~c[35;1m~a~c[0m~@[~a~]~%"
              #\ESC title #\ESC content)
      (format t "[~a]~a~%" title content))) 

(defun pprint-text (text use-color)
  (let ((m) (lines (str:lines text)))
    (emphasize (car lines) "" use-color)
    (loop :for line :in (butlast (cddr lines))
          :unless (or (str:contains? "X3J13" line)
                      (str:contains? "<TT>" line))
          :when (car (setf m
                           (multiple-value-list
                             (ppcre:scan "^[A-Z][a-zA-Z ]+:" line))))
          :do (progn
                (terpri)
                (emphasize (subseq line (car m) (cadr m))
                           (subseq line (cadr m))
                           use-color))
          :else
          :do (format t 
                (if (str:starts-with? "|" line) " ~a~%" "~a~%")
                (str:trim line)))
    (format t "~%~a~2%" (last lines))))

(defun lookup (symbol)
  (let ((value (assoc symbol *cache* :test #'string-equal)))
    (when value (return-from lookup (cadr value))))
  (let ((value (assoc symbol *symbols-map* :test #'string-equal)))
    (when value
      (destructuring-bind (name . url) value
        (let* ((url (clhs-url url))
               (html (retrieve-url url)))
          (cadar
            (push
              `(,name ((url ,url)
                       (html ,html)
                       (text ,(extract-text html))))
              *cache*)))))))

(defmacro get-data (symbol key)
  `(cadr (assoc ',key (lookup ,symbol) :test #'string-equal)))
 
(defun get-html (symbol)
  (get-data symbol html))

(defun get-url (symbol)
  (get-data symbol url))

(defun show (symbol &key use-color)
  (let ((text (get-data symbol text)))
    (if text
        (pprint-text text use-color)
        (format t "No documentation found on ~a.~%" symbol))))
