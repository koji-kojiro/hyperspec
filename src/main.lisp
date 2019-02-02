(in-package :hyperspec)

(defclass document ()
  ((url
    :initarg :url
    :reader url)
   (html
    :initarg :html
    :reader html)
   (text
    :initarg :text
    :reader text)))

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
              (format s "~a" (plump:text n)))
          :test #'plump:text-node-p))))))

(defun pprint-text (text)
  (let ((m) (lines (mapcar #'str:trim (str:lines text))))
    (format t "~c[35;1m~a~c[0m~%" #\ESC (car lines) #\ESC)
    (loop :for line :in (butlast (cddr lines))
          :unless (str:contains? "X3J13" line)
          :when (car (setf m
                           (multiple-value-list
                             (ppcre:scan "^[A-Z][a-zA-Z ]+:" line))))
          :do (format t "~%~c[35;1m~a~c[0m~@[~a~]~%"
                      #\ESC (subseq line (car m) (cadr m))
                      #\ESC (subseq line (cadr m)))
          :else :do (format t "~a~%" line))
    (format t "~%~a~2%" (last lines))))

(defun lookup (symbol)
  (loop :for (name value) :in *cache*
        :when (string-equal name symbol)
        :do (return-from lookup value))
  (let ((value (assoc symbol *symbols-map* :test #'string-equal)))
    (when value
      (destructuring-bind (name . url) value
        (let* ((url (clhs-url url))
               (html (retrieve-url url))
               (doc (make-instance 'document
                      :url url
                      :html html
                      :text (extract-text html))))
          (push (list name doc) *cache*)
          doc)))))

(defmacro get-data (symbol accessor)
  (let ((doc (gensym)))
    `(let ((,doc (lookup ,symbol)))
       (when ,doc (,accessor ,doc)))))

(defun get-html (symbol)
  (get-data symbol html))

(defun get-url (symbol)
  (get-data symbol url))

(defun show (symbol)
  (let ((text (get-data symbol text)))
    (if text
        (pprint-text text)
        (format t "No documentation found on ~a.~%" symbol))))
