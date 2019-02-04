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

(defun edit-distance (str1 str2)
  (let ((n (length str1))
        (m (length str2)))
    (let ((col (make-array (1+ m) :element-type 'integer))
          (prev-col (make-array (1+ m) :element-type 'integer)))
      (dotimes (i (1+ m))
        (setf (svref prev-col i) i))
      (dotimes (i n)
        (setf (svref col 0) (1+ i))
        (dotimes (j m)
          (setf (svref col (1+ j))
                (min (1+ (svref col j))
                     (1+ (svref prev-col (1+ j)))
                     (+ (svref prev-col j)
                     (if (char-equal (schar str1 i)
                                     (schar str2 j))
                         0 1)))))
        (rotatef col prev-col))
      (svref prev-col m))))

(defun list-possible-choice (symbol n)
  (let ((target (string symbol)))
    (mapcar 
      #'car
      (subseq
        (sort
          (mapcar
            #'(lambda (elm)
                (list (car elm)
                      (edit-distance (string (car elm)) target)))
            *symbols-map*)
          #'< :key #'cadr)
        0 n))))

(defmacro get-data (symbol key)
  `(cadr (assoc ',key (lookup ,symbol) :test #'string-equal)))
 
(defun get-html (symbol)
  (get-data symbol html))

(defun get-url (symbol)
  (get-data symbol url))

(defun show (symbol &key use-color)
  (let ((text (get-data symbol text)))
    (if text
        (or (pprint-text text use-color) t)
        (progn 
          (format t "No documentation found on ~a.~2%" symbol)
          (format t "Did you mean:~%~{  ~a~}~2%"
                  (list-possible-choice symbol 4))))))
