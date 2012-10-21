(defpackage h264.bit-stream
  (:use :common-lisp :h264.common)
  (:shadow :common-lisp read)
  (:export bit-stream

           make-input-stream
           with-input-stream 
           with-input-from-octets

           read
           peek
           byte-aligned?
           eos?
           ))
(in-package :h264.bit-stream)

(deftype endian () '(member :big :little))
