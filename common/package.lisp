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
