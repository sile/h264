(in-package :h264.parser)

;; XXX: 暫定ファイル

(deftype $u (&optional (bit-length 32)) `(unsigned-byte ,bit-length))
(deftype $ue () '(unsigned-byte 32))
(deftype $se () '(signed-byte 32))
(deftype $list (type) `(vector ,type))

(deftype $vui-parameters () 'list)

(defmacro empty (type)
  `(make-array 0 :element-type ',type))

(defvar *default-bit-stream*) ; XXX: delete
(defun $bit (bit-length &optional (in *default-bit-stream*))
  (parse-bit in bit-length))

(defun $u (bit-length &optional (in *default-bit-stream*))
  (parse-bit in bit-length))

(defun $ue (&optional (in *default-bit-stream*))
  (parse-ue in))

(defun $se (&optional (in *default-bit-stream*))
  (parse-se in))

(defmacro $list (type &key count (parse-type type))
  `(repeat ,count ,type ,(if (listp parse-type) parse-type (list parse-type))))

(defmacro repeat (n type fn)
  (let ((ary (gensym))
        (i   (gensym)))
    `(let ((,ary (make-array ,n :element-type ',type)))
       (dotimes (,i ,n ,ary)
         (setf (aref ,ary ,i) ,fn)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *focused-class* t)
  (defun symb (&rest args)
    (intern (format nil "~{~A~}" args))))

(defmacro $parse (slot-name class-name extra-args)
  (let* ((slots (sb-mop:class-slots (find-class class-name)))
         (type (sb-mop:slot-definition-type
                (find slot-name slots :key #'sb-mop:slot-definition-name)))
         (parse-exp (if (listp type) type (list type))))
    `(setf ,slot-name ,(append parse-exp extra-args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun $tmp (slot-name class-name extra-args)
  `($parse ,slot-name ,class-name ,extra-args))
)

(defmacro with-parse-context ((in class-name) &body body)
  `(with-make-instance (,class-name)
     (macrolet (($ (slot-name &rest extra-args)
                  ($tmp slot-name ',class-name extra-args)))
       (let ((*default-bit-stream* ,in))
         ,@body))))


