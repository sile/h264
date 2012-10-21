(in-package :h264.bit-stream)

(defstruct bit-stream
  (source t :type stream)
  (octets t :type list)
  (pos    t :type (integer 1 8)))

(defun make-input-stream (source)
  (make-bit-stream :source source
                   :octets '()
                   :pos 8))

(defmacro with-input-stream ((in source) &body body)
  `(let ((,in (make-input-stream ,source)))
     ,@body))

(defmacro with-input-from-octets ((in octets) &body body)
  (let ((source (gensym)))
    `(flexi-streams:with-input-from-sequence (,source (the octets ,octets))
       (let ((,in (make-input-stream ,source)))
         ,@body))))

(defun eos? (in)
  (declare (bit-stream in))
  (with-slots (source octets) in
    (and (null octets)
         (not (listen source)))))

(defun read-impl (octets pos bit-length)
  (if (<= bit-length pos)
      (let ((new-pos (- pos bit-length)))
        (values (ldb (byte bit-length new-pos) (car octets))
                (if (zerop new-pos) (cdr octets) octets)
                (if (zerop new-pos) 8 new-pos)))
    (multiple-value-bind (value new-octets new-pos)
                         (read-impl (cdr octets) 8 (- bit-length pos))
      (values (+ (ash (ldb (byte pos 0) (car octets))
                      (- bit-length pos))
                 value)
              new-octets
              new-pos))))

(defun read (in bit-length)
  (declare (bit-stream in)
           (fixnum bit-length))

  (with-slots (source octets pos) in
    (loop REPEAT (- (ceiling (+ bit-length pos) 8) (length octets))
          DO (setf octets (append octets (list (read-byte source nil 0)))))
    
    (multiple-value-bind (value new-octets new-pos)
                         (read-impl octets pos bit-length)
      (setf octets new-octets
            pos new-pos)
      value)))