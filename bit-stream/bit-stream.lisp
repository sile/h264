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
    (and (not (listen source))
         (null octets))))

(defun byte-aligned? (in)
  (declare (bit-stream in))
  (with-slots (pos) in
    (= pos 8)))

(defun read-impl (octets pos bit-length)
  (cond ((zerop bit-length) 
         (values 0 octets pos))
        
        ((<= bit-length pos)
         (let ((new-pos (- pos bit-length)))
           (values (ldb (byte bit-length new-pos) (or (car octets) 0))
                   (if (zerop new-pos) (cdr octets) octets)
                   (if (zerop new-pos) 8 new-pos))))

        (t
         (multiple-value-bind (value new-octets new-pos)
                              (read-impl (cdr octets) 8 (- bit-length pos))
           (values (+ (ash (ldb (byte pos 0) (or (car octets) 0))
                           (- bit-length pos))
                      value)
                   new-octets
                   new-pos)))))

(defun fill-octets (in bit-length)
  (with-slots (source octets pos) in
    (loop REPEAT (- (ceiling (+ bit-length (- 8 pos)) 8) (length octets))
          WHILE (listen source)
          DO (setf octets (append octets (list (read-byte source)))))))

(defun read (in bit-length)
  (declare (bit-stream in)
           (fixnum bit-length))

  (fill-octets in bit-length)
  (with-slots (octets pos) in
    (multiple-value-bind (value new-octets new-pos)
                         (read-impl octets pos bit-length)
      (setf octets new-octets
            pos new-pos)
      value)))

(defun peek (in bit-length)
  (declare (bit-stream in)
           (fixnum bit-length))

  (fill-octets in bit-length)
  (with-slots (octets pos) in
    (values (read-impl octets pos bit-length))))
  