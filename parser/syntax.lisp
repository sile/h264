(in-package :h264.parser)

;;; TODO: h264.syntax的なパッケージを用意してそこに移動？
 
;;; VUI-parameters
;; Annex.E
(defconstant +EXTENDED_SAR+ 255); Table E-1: Meaning of sample aspect ratio indicator

(defstruct vui-parameters
  (aspect-ratio-info-present-flag          0 :type ($u 1)  :read-only t)
  (aspect-ratio-idc                        0 :type ($u 8)  :read-only t)
  (sar-width                               0 :type ($u 16) :read-only t)
  (sar-height                              0 :type ($u 16) :read-only t)
  (overscan-info-present-flag              0 :type ($u 1)  :read-only t)
  (overscan-appropriate-flag               0 :type ($u 1)  :read-only t)
  (video-signal-type-present-flag          0 :type ($u 1)  :read-only t)
  (video-format                            0 :type ($u 3)  :read-only t)
  (video-full-range-flag                   0 :type ($u 1)  :read-only t)
  (colour-description-present-flag         0 :type ($u 1)  :read-only t)
  (colour-primaries                        0 :type ($u 8)  :read-only t)
  (transfer-characteristics                0 :type ($u 8)  :read-only t)
  (matrix-coefficients                     0 :type ($u 8)  :read-only t)
  (chroma-loc-info-present-flag            0 :type ($u 1)  :read-only t)
  (chroma-sample-loc-type-top-field        0 :type $ue     :read-only t)
  (chroma-sample-loc-type-bottom-field     0 :type $ue     :read-only t)
  (timing-info-present-flag                0 :type ($u 1)  :read-only t)
  (num-units-in-tick                       0 :type ($u 32) :read-only t)
  (time-scale                              0 :type ($u 32) :read-only t)
  (fixed-frame-rate-flag                   0 :type ($u 1)  :read-only t)
  (nal-hrd-parameters-present-flag         0 :type ($u 1)  :read-only t)
  (nal-hrd-parameters                      t :type t       :read-only t) ; TODO:
  (vcl-hrd-parameters-present-flag         0 :type ($u 1)  :read-only t)
  (vcl-hrd-parameters                      t :type t       :read-only t) ; TODO:
  (low-delay-hrd-flag                      0 :type ($u 1)  :read-only t)
  (pic-struct-present-flag                 0 :type ($u 1)  :read-only t)
  (bitstream-restriction-flag              0 :type ($u 1)  :read-only t)
  (motion-vectors-over-pic-boundaries-flag 0 :type ($u 1)  :read-only t)
  (max-bytes-per-pic-denom                 0 :type $ue     :read-only t)
  (max-bits-per-mb-denom                   0 :type $ue     :read-only t)
  (log2-max-mv-length-horizontal           0 :type $ue     :read-only t)
  (log2-max-mv-length-vertical             0 :type $ue     :read-only t)
  (max-num-reorder-frames                  0 :type $ue     :read-only t)
  (max-dec-frame-buffering                 0 :type $ue     :read-only t))

(deftype $vui-parameters () '(or null vui-parameters))

(defun parse-vui-parameters (in)
  (with-parse-context (in vui-parameters)
    ($ aspect-ratio-info-present-flag)
    (when (= aspect-ratio-info-present-flag 1)
      ($ aspect-ratio-idc)
      
      (when (= aspect-ratio-idc +EXTENDED_SAR+)
        ($ sar-width)
        ($ sar-height)))

    ($ overscan-info-present-flag)
    (when (= overscan-info-present-flag 1)
      ($ overscan-appropriate-flag))
    
    ($ video-signal-type-present-flag)
    (when (= video-signal-type-present-flag 1)
      ($ video-format)
      ($ video-full-range-flag)
      ($ colour-description-present-flag)

      (when (= colour-description-present-flag 1)
        ($ colour-primaries)
        ($ transfer-characteristics)
        ($ matrix-coefficients)))
    
    ($ chroma-loc-info-present-flag)
    (when (= chroma-loc-info-present-flag 1)
      ($ chroma-sample-loc-type-top-field)
      ($ chroma-sample-loc-type-bottom-field))
            
    ($ timing-info-present-flag)
    (when (= timing-info-present-flag 1)
      ($ num-units-in-tick)
      ($ time-scale)
      ($ fixed-frame-rate-flag))

    ($ nal-hrd-parameters-present-flag)
    (when (= nal-hrd-parameters-present-flag 1)
      ;; ($ nal-hrd-parameters)
      (error "Not Implemented"))

    ($ vcl-hrd-parameters-present-flag)
    (when (= vcl-hrd-parameters-present-flag 1)
      ;; ($ vcl-hrd-parameters)
      (error "Not Implemented"))

    (when (or (= nal-hrd-parameters-present-flag 1)
              (= vcl-hrd-parameters-present-flag 1))    
      ($ low-delay-hrd-flag))

    ($ pic-struct-present-flag)
    ($ bitstream-restriction-flag)
    (when (= bitstream-restriction-flag 1)
      ($ motion-vectors-over-pic-boundaries-flag)
      ($ max-bytes-per-pic-denom)
      ($ max-bits-per-mb-denom)
      ($ log2-max-mv-length-horizontal)
      ($ log2-max-mv-length-vertical)
      ($ max-num-reorder-frames)
      ($ max-dec-frame-buffering))))


;;; seq-parameter-set-data
(defstruct seq-parameter-set-data
  (profile-idc                           0 :type ($u 8) :read-only t)
  (constraint-set0-flag                  0 :type ($u 1) :read-only t)
  (constraint-set1-flag                  0 :type ($u 1) :read-only t)
  (constraint-set2-flag                  0 :type ($u 1) :read-only t)
  (constraint-set3-flag                  0 :type ($u 1) :read-only t)
  (constraint-set4-flag                  0 :type ($u 1) :read-only t)
  (constraint-set5-flag                  0 :type ($u 1) :read-only t)
  (reserved-zero-2bits                   0 :type ($u 2) :read-only t)
  (level-idc                             0 :type ($u 8) :read-only t)
  (seq-parameter-set-id                  0 :type $ue    :read-only t)
  (log2-max-frame-num-minus4             0 :type $ue    :read-only t)
  (pic-order-cnt-type                    0 :type $ue    :read-only t)
  (log2-max-pic-order-cnt-lsb-minus4     0 :type $ue    :read-only t)
  (delta-pic-order-always-zero-flag      0 :type ($u 1) :read-only t)
  (offset-for-non-ref-pic                0 :type $se    :read-only t)
  (offset-for-top-to-bottom-field        0 :type $se    :read-only t)
  (num-ref-frames-in-pic-order-cnt-cycle 0 :type $ue         :read-only t)
  (offset-for-ref-frame-list   (empty $se) :type ($list $se) :read-only t)
  (max-num-ref-frames                    0 :type $ue    :read-only t)
  (gaps-in-frame-num-value-allowed-flag  0 :type ($u 1) :read-only t)
  (pic-width-in-mbs-minus1               0 :type $ue    :read-only t)
  (pic-height-in-map-units-minus1        0 :type $ue    :read-only t)
  (frame-mbs-only-flag                   0 :type ($u 1) :read-only t)
  (mb-adaptive-frame-field-flag          0 :type ($u 1) :read-only t)
  (direct-8x8-inference-flag             0 :type ($u 1) :read-only t)
  (frame-cropping-flag                   0 :type $ue    :read-only t)
  (frame-crop-left-offset                0 :type $ue    :read-only t)
  (frame-crop-right-offset               0 :type $ue    :read-only t)
  (frame-crop-top-offset                 0 :type $ue    :read-only t)
  (frame-crop-bottom-offset              0 :type $ue    :read-only t)
  (vui-parameters-present-flag           0 :type ($u 1) :read-only t)
  (vui-parameters                      nil :type $vui-parameters :read-only t))

(defun parse-seq-parameter-set-data (in)
  (with-parse-context (in seq-parameter-set-data) 
    ($ profile-idc)
    ($ constraint-set0-flag)
    ($ constraint-set1-flag)
    ($ constraint-set2-flag)
    ($ constraint-set3-flag)
    ($ constraint-set4-flag)
    ($ constraint-set5-flag)
    ($ reserved-zero-2bits)
    (assert (= reserved-zero-2bits 0) () "reserved-zero-2bits must be 0: %a" reserved-zero-2bits)

    ($ level-idc)
    ($ seq-parameter-set-id)

    (when (member profile-idc '(100 110 122 244 44 83 86 118 128))
      (error "Not Implemented"))

    ($ log2-max-frame-num-minus4)
    ($ pic-order-cnt-type)

    (ecase pic-order-cnt-type
      (0 ($ log2-max-pic-order-cnt-lsb-minus4))
      (1 ($ delta-pic-order-always-zero-flag)
         ($ offset-for-non-ref-pic)
         ($ offset-for-top-to-bottom-field)
         ($ num-ref-frames-in-pic-order-cnt-cycle)
         ($ offset-for-ref-frame-list :count num-ref-frames-in-pic-order-cnt-cycle)))

    ($ max-num-ref-frames)
    ($ gaps-in-frame-num-value-allowed-flag)
    ($ pic-width-in-mbs-minus1)
    ($ pic-height-in-map-units-minus1)
    ($ frame-mbs-only-flag)
    
    (when (= frame-mbs-only-flag 0)
      ($ mb-adaptive-frame-field-flag))
    
    ($ direct-8x8-inference-flag)
    ($ frame-cropping-flag)

    (when frame-cropping-flag
      ($ frame-crop-left-offset)
      ($ frame-crop-right-offset)
      ($ frame-crop-top-offset)
      ($ frame-crop-bottom-offset))

    ($ vui-parameters-present-flag)
    (when (= vui-parameters-present-flag 1)
      ($ vui-parameters))))