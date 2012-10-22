(defpackage h264.common
  (:use :common-lisp)
  (:export octet
           octets

           with-package
           ))
(in-package :h264.common)

(deftype octet () '(unsigned-byte 8))
(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))

(defun find-external-symbols (package-name)
  (let ((package (find-package package-name)))
    (assert (packagep package) () "~a is not a valid package name" package-name)
    (with-package-iterator (generator package :external) 
      (loop FOR (more? symbol) = (multiple-value-list (generator))
            WHILE more?
            COLLECT symbol))))

(defmacro with-package ((package) &body body)
  (let* ((symbols (find-external-symbols package))
         (name->symbol (mapcar (lambda (s) (list (symbol-name s) s)) symbols)))

    ;; XXX: 置き換え不完全 (quoteとかmacroとか考慮していない)
    (labels ((recur (exp)
               (typecase exp
                 (cons (destructuring-bind (car . cdr) exp
                         (cons (typecase car
                                 (symbol (let ((pair (assoc (symbol-name car) name->symbol :test #'string=)))
                                           (if (null pair)
                                               car 
                                             (second pair))))
                                 (otherwise (recur car)))
                               (mapcar #'recur cdr))))
                 (otherwise exp))))
      `(locally ,@(mapcar #'recur body)))))
