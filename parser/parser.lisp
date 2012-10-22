(in-package :h264.parser)

(defconstant +START_CODE_PREFIX_ONE_3BYTES+ #x000001)
(defconstant +LEADING_ZERO_8BITS+ #x00)

(defun seek-to-next-nal-unit (in)
  (loop UNTIL (= (h264.bit-stream:peek in 24) +START_CODE_PREFIX_ONE_3BYTES+)
        DO (assert (= (h264.bit-stream:read in 8) +LEADING_ZERO_8BITS+)
                   ()
                   "leading-zero-8bits must be 0"))
  (h264.bit-stream:read in 24) ; start-code-prefix
  (values))

(defun read-nal-unit-bytes (in)
  (seek-to-next-nal-unit in)
  (prog1
      (loop FOR 3byte = (h264.bit-stream:peek in 24)
            UNTIL (or (= 3byte #x000000)
                      (= 3byte #x000001))
            COLLECT (h264.bit-stream:read in 8) INTO bytes
            FINALLY (return (coerce bytes 'octets)))

    ;; discard trailing bytes
    (loop FOR 3byte = (h264.bit-stream:peek in 24)
          UNTIL (= 3byte #x000000)
          DO (assert (= (h264.bit-stream:read in 8) 0)
                     ()
                     "trailing-zero-8bits must be 0")
          FINALLY
          (h264.bit-stream:read in 24))))

(defun parse-nal-unit-header-svc-extension (in)
  (declare (ignore in))
  (error "Not Implemented"))

(defun parse-nal-unit-header-mvc-extension (in)
  (declare (ignore in))
  (error "Not Implemented"))

;; 7.3.1: NAL unit syntax
(defun parse-nal-unit (in)
  (with-package (:h264.bit-stream)
    (let ((forbidden-zero-bit (read in 1))
          (nal-ref-idc        (read in 2))
          (nal-unit-type      (read in 5))
          (rbsp-bytes '())
          )
      (assert (= 0 forbidden-zero-bit) ()
              "forbidden-zero-bit must be 0: ~a" forbidden-zero-bit)
      
      (when (or (= nal-unit-type 14)
                (= nal-unit-type 20))
        (let ((svc-extension-flag (read in 1)))
          (if (= svc-extension-flag  1)
              (parse-nal-unit-header-svc-extension in) ; specified in Annex G
            (parse-nal-unit-header-mvc-extension in))) ; specified in Annex H
        )
      
      (loop UNTIL (eos? in)
            FOR 3bytes = (peek in 24)
            DO 
            (if (= 3bytes #x000003)
                ;; emulation-prevention-three-byte
                (progn (push (read in 8) rbsp-bytes)
                       (push (read in 8) rbsp-bytes)
                       (read in 8))
              (push (read in 8) rbsp-bytes)))
      (setf rbsp-bytes (coerce (nreverse rbsp-bytes) 'octets))

      (values nal-ref-idc
              nal-unit-type
              rbsp-bytes)
      )))

(defun parse-seq-parameter-set-data (in)
  (with-package (:h264.bit-stream)
    (let ((profile-idc (read in 8))
          (constraint-set0-flag (read in 1))
          (constraint-set1-flag (read in 1))
          (constraint-set2-flag (read in 1))
          (constraint-set3-flag (read in 1))
          (constraint-set4-flag (read in 1))
          (constraint-set5-flag (read in 1))
          (reserved-zero-2bits  (read in 2))
          (level-idc            (read in 8))
          )
      (values profile-idc
              constraint-set0-flag
              constraint-set1-flag
              constraint-set2-flag
              constraint-set3-flag
              constraint-set4-flag
              constraint-set5-flag
              reserved-zero-2bits
              level-idc
              (loop FOR b = (read in 8)
                    UNTIL (eos? in)
                    COLLECT b)
              ))))

(defun parse-sequence-parameter-set (in)
  ;;; seq-parameter-set-data
  (parse-seq-parameter-set-data in)

  ;;; rbsp-trailing-bits
  )

(defun parse (in)
  (h264.bit-stream:with-input-stream (in in)
    (let ((nal-unit-bytes (read-nal-unit-bytes in)))
      (h264.bit-stream:with-input-from-octets (in2 nal-unit-bytes)
        (multiple-value-bind (nal-ref-idc nal-unit-type rbsp-bytes)
                             (parse-nal-unit in2)
          (declare (ignore nal-ref-idc))
          (h264.bit-stream:with-input-from-octets (in3 rbsp-bytes)
            (ecase nal-unit-type
              (7 (parse-sequence-parameter-set in3))
              )))))))

(defun parse-file (filepath)
  (with-open-file (in filepath :element-type 'octet)
    (parse in)))
