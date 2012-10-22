(in-package :h264.parser)

(defun parse-exp-golomb-code (in)
  (with-package (:h264.bit-stream)
    (let* ((leading-zero-bit-count
            (loop WHILE (= 0 (read in 1))
                  SUM 1))
           (code-num
            (+ (1- (expt 2 leading-zero-bit-count))
               (read in leading-zero-bit-count))))
      code-num)))

;; ue(v)
(defun parse-ue (in)
  (parse-exp-golomb-code in))

      
      