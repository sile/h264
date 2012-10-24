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

;; Annex.E
(defconstant +EXTENDED_SAR+ 255); Table E-1: Meaning of sample aspect ratio indicator
(defun parse-vui-parameters (in)
  (with-package (:h264.bit-stream)
    (let* ((aspect-ratio-info-present-flag (read in 1))
           (aspect-ratio-idc 
            (when (= aspect-ratio-info-present-flag 1)
              (read in 8)))
           (sar-width 
            (when (= aspect-ratio-idc +EXTENDED_SAR+)
              (read in 16)))
           (sar-height
            (when (= aspect-ratio-idc +EXTENDED_SAR+)
              (read in 16)))
           (overscan-info-present-flag (read in 1))
           (overscan-appropriate-flag 
            (when (= overscan-info-present-flag 1)
              (read in 1)))
           (video-signal-type-present-flag (read in 1))
           (video-format
            (when (= video-signal-type-present-flag 1)
              (read in 3)))
           (video-full-range-flag
            (when (= video-signal-type-present-flag 1)
              (read in 1)))
           (colour-description-present-flag
            (when (= video-signal-type-present-flag 1)
              (read in 1)))
           (colour-primaries 
            (when (eql colour-description-present-flag 1)
              (read in 8)))
           (transfer-characteristics 
            (when (eql colour-description-present-flag 1)
              (read in 8)))
           (matrix-coefficients
            (when (eql colour-description-present-flag 1)
              (read in 8)))
           (chroma-loc-info-present-flag (read in 1))
           (chroma-sample-loc-type-top-field
            (when (= chroma-loc-info-present-flag 1)
              (parse-ue in)))
           (chroma-sample-loc-type-bottom-field
            (when (= chroma-loc-info-present-flag 1)
              (parse-ue in)))
            
           (timing-info-present-flag (read in 1))
           (num-units-in-tick
            (when (= timing-info-present-flag 1)
              (read in 32)))
           (time-scale
            (when (= timing-info-present-flag 1)
              (read in 32)))
           (fixed-frame-rate-flag
            (when (= timing-info-present-flag 1)
              (read in 1)))
           
           (nal-hrd-parameters-present-flag (read in 1))
           (nal-hrd-parameters
            (when (= nal-hrd-parameters-present-flag 1)
              (error "Not Implemented")))
           (vcl-hrd-parameters-present-flag (read in 1))
           (vcl-hrd-parameters
            (when (= vcl-hrd-parameters-present-flag 1)
              (error "Not Implemented")))
           (low-delay-hrd-flag
            (when (or (= nal-hrd-parameters-present-flag 1)
                      (= vcl-hrd-parameters-present-flag 1))
              (read in 1)))
           (pic-struct-present-flag (read in 1))
           (bitstream-restriction-flag (read in 1))
           (motion-vectors-over-pic-boundaries-flag
            (when (= bitstream-restriction-flag 1)
              (read in 1)))
           (max-bytes-per-pic-denom
            (when (= bitstream-restriction-flag 1)
              (parse-ue in)))
           (max-bits-per-mb-denom
            (when (= bitstream-restriction-flag 1)
              (parse-ue in)))
           (log2-max-mv-length-horizontal
            (when (= bitstream-restriction-flag 1)
              (parse-ue in)))
           (log2-max-mv-length-vertical
            (when (= bitstream-restriction-flag 1)
              (parse-ue in)))
           (max-num-reorder-frames
            (when (= bitstream-restriction-flag 1)
              (parse-ue in)))
           (max-dec-frame-buffering
            (when (= bitstream-restriction-flag 1)
              (parse-ue in)))
           )
      (list aspect-ratio-info-present-flag
            aspect-ratio-idc
            sar-width
            sar-height

            overscan-info-present-flag
            overscan-appropriate-flag
            video-signal-type-present-flag
            video-format
            video-full-range-flag
            colour-description-present-flag
            colour-primaries
            transfer-characteristics
            matrix-coefficients
            
            chroma-loc-info-present-flag
            chroma-sample-loc-type-top-field
            chroma-sample-loc-type-bottom-field
            timing-info-present-flag
            num-units-in-tick
            time-scale
            fixed-frame-rate-flag

            nal-hrd-parameters-present-flag
            nal-hrd-parameters
            vcl-hrd-parameters-present-flag
            vcl-hrd-parameters
            low-delay-hrd-flag
            pic-struct-present-flag
            bitstream-restriction-flag
            motion-vectors-over-pic-boundaries-flag
            max-bytes-per-pic-denom
            max-bits-per-mb-denom
            log2-max-mv-length-horizontal
            log2-max-mv-length-vertical
            max-num-reorder-frames
            max-dec-frame-buffering
            )
      )))

(defstruct seq-parameter-set-data
  profile-idc
  constraint-set0-flag
  constraint-set1-flag
  constraint-set2-flag
  constraint-set3-flag
  constraint-set4-flag
  constraint-set5-flag
  reserved-zero-2bits
  level-idc
  seq-parameter-set-id

  log2-max-frame-num-minus4
  pic-order-cnt-type
  log2-max-pic-order-cnt-lsb-minus4
  delta-pic-order-always-zero-flag
  offset-for-non-ref-pic
  offset-for-top-to-bottom-field
  num-ref-frames-in-pic-order-cnt-cycle
  offsets-for-ref-frame

  max-num-ref-frames
  gaps-in-frame-num-value-allowed-flag
  pic-width-in-mbs-minus1
  pic-height-in-map-units-minus1
  frame-mbs-only-flag
  mb-adaptive-frame-field-flag
  direct-8x8-inference-flag
  frame-cropping-flag
  frame-crop-left-offset
  frame-crop-right-offset
  frame-crop-top-offset
  frame-crop-bottom-offset
  vui-parameters-present-flag
  vui-parameters)

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
          (seq-parameter-set-id (parse-ue in))
          )

      (when (member profile-idc '(100 110 122 244 44 83 86 118 128))
        (error "Not Implemented"))
      
      (let* ((log2-max-frame-num-minus4 (parse-ue in))
             (pic-order-cnt-type (parse-ue in))
             (log2-max-pic-order-cnt-lsb-minus4
              (when (= pic-order-cnt-type 0) (parse-ue in)))
             (delta-pic-order-always-zero-flag 
              (when (= pic-order-cnt-type 1) (read in 1)))
             (offset-for-non-ref-pic  
              (when (= pic-order-cnt-type 1) (parse-se in)))
             (offset-for-top-to-bottom-field 
              (when (= pic-order-cnt-type 1) (parse-se in)))
             (num-ref-frames-in-pic-order-cnt-cycle 
              (when (= pic-order-cnt-type 1) (parse-ue in)))
             (offsets-for-ref-frame
              (when (= pic-order-cnt-type 1)
                (loop REPEAT num-ref-frames-in-pic-order-cnt-cycle
                      COLLECT (parse-se in))))
             (max-num-ref-frames (parse-ue in))
             (gaps-in-frame-num-value-allowed-flag (read in 1))
             (pic-width-in-mbs-minus1 (parse-ue in))
             (pic-height-in-map-units-minus1 (parse-ue in))
             (frame-mbs-only-flag (read in 1))
             (mb-adaptive-frame-field-flag
              (when (= frame-mbs-only-flag 0) (read in 1)))
             (direct-8x8-inference-flag (read in 1))
             (frame-cropping-flag (read in 1))
             (frame-crop-left-offset
              (when (= frame-cropping-flag 1) (parse-ue in)))
             (frame-crop-right-offset
              (when (= frame-cropping-flag 1) (parse-ue in)))
             (frame-crop-top-offset
              (when (= frame-cropping-flag 1) (parse-ue in)))
             (frame-crop-bottom-offset
              (when (= frame-cropping-flag 1) (parse-ue in)))
             (vui-parameters-present-flag (read in 1))
             (vui-parameters
              (when (= vui-parameters-present-flag 1)
                (parse-vui-parameters in)))
             )

        (make-seq-parameter-set-data
         :profile-idc profile-idc
         :constraint-set0-flag constraint-set0-flag
         :constraint-set1-flag constraint-set1-flag
         :constraint-set2-flag constraint-set2-flag
         :constraint-set3-flag constraint-set3-flag
         :constraint-set4-flag constraint-set4-flag
         :constraint-set5-flag constraint-set5-flag
         :reserved-zero-2bits reserved-zero-2bits
         :level-idc level-idc
         :seq-parameter-set-id seq-parameter-set-id

         :log2-max-frame-num-minus4 log2-max-frame-num-minus4
         :pic-order-cnt-type pic-order-cnt-type
         :log2-max-pic-order-cnt-lsb-minus4 log2-max-pic-order-cnt-lsb-minus4
         :delta-pic-order-always-zero-flag delta-pic-order-always-zero-flag
         :offset-for-non-ref-pic offset-for-non-ref-pic
         :offset-for-top-to-bottom-field offset-for-top-to-bottom-field
         :num-ref-frames-in-pic-order-cnt-cycle num-ref-frames-in-pic-order-cnt-cycle
         :offsets-for-ref-frame offsets-for-ref-frame

         :max-num-ref-frames max-num-ref-frames
         :gaps-in-frame-num-value-allowed-flag gaps-in-frame-num-value-allowed-flag
         :pic-width-in-mbs-minus1 pic-width-in-mbs-minus1
         :pic-height-in-map-units-minus1 pic-height-in-map-units-minus1
         :frame-mbs-only-flag frame-mbs-only-flag
         :mb-adaptive-frame-field-flag mb-adaptive-frame-field-flag
         :direct-8x8-inference-flag direct-8x8-inference-flag
         :frame-cropping-flag frame-cropping-flag
         :frame-crop-left-offset frame-crop-left-offset
         :frame-crop-right-offset frame-crop-right-offset
         :frame-crop-top-offset frame-crop-top-offset
         :frame-crop-bottom-offset frame-crop-bottom-offset
         :vui-parameters-present-flag vui-parameters-present-flag
         :vui-parameters vui-parameters)))))

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
