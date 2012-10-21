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

