(in-package :h264.parser)

(defvar *nal-ref-idc*) ; XXX:

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

(defun $vui-parameters ()
  (parse-vui-parameters *default-bit-stream*))

;;; seq-parameter-set-data
(defvar *seq-parameter-set-data*)
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

  ;; TODO:
  (chroma-format-idc 0 :type $ue)
  (separate-colour-plane-flag 0 :type ($u 1))
  (bit-depth-luma-minus8 0 :type ($u 1))
  (bit-depth-chroma-minus8 0 :type $ue)
  (qpprime-y-zero-transform-bypass-flag 0 :type ($u 1))
  (seq-scaling-matrix-present-flag 0 :type ($u 1))
  (seq-scaling-list-present-flag t :type t) ; TODO:
  
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
  (setf *seq-parameter-set-data*
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
      ($ vui-parameters)))))

(defmacro defsyntax (name &rest slots)
  `(defstruct ,name
     ,@(mapcar (lambda (s) (append s '(:read-only t))) 
               slots)))

;;; pic-parameter-set
(defvar *pic-parameter-set*)
(defsyntax pic-parameter-set
  (pic-parameter-set-id                         0 :type $ue)
  (seq-parameter-set-id                         0 :type $ue)
  (entropy-coding-mode-flag                     0 :type ($u 1))
  (bottom-field-pic-order-in-frame-present-flag 0 :type ($u 1))
  (num-slice-groups-minus1                      0 :type $ue)
  (slice-group-map-type                         0 :type $ue)
  (run-length-minus1-list             (empty $ue) :type ($list $ue))
  (top-left-list                      (empty $ue) :type ($list $ue))
  (bottom-right-list                  (empty $ue) :type ($list $ue))
  (slice-group-change-direction-flag            0 :type ($u 1))
  (slice-group-change-rate-minus1               0 :type $ue)
  (pic-size-in-map-units-minus1                 0 :type $ue)
  (slice-group-id-list                  (empty $u):type ($list $u)) ; v = (log2 num-slice-groups-minus1)
  (num-ref-idx-10-default-active-minus1         0 :type $ue)
  (num-ref-idx-11-default-active-minus1         0 :type $ue)
  (weighted-pred-flag                           0 :type ($u 1))
  (weighted-bipred-idc                          0 :type ($u 2))
  (pic-init-qp-minus26                          0 :type $se)  ; relative to 26
  (pic-init-qs-minus26                          0 :type $se)  ; relative to 26
  (chroma-qp-index-offset                       0 :type $se)
  (deblocking-filter-control-present-flag       0 :type ($u 1))
  (constrained-intra-pred-flag                  0 :type ($u 1))
  (redundant-pic-cnt-present-flag               0 :type ($u 1))
  (transform-8x8-mode-flag                      0 :type ($u 1))
  (pic-scaling-matrix-present-flag              0 :type ($u 1))
  (pic-scaling-list-present-flag-list (empty ($u 1)) :type ($list ($u 1)))
  (scaling-list-list                          nil :type null) ; TODO:
  (second-chroma-qp-index-offset                0 :type $se))

(defun parse-pic-parameter-set (in)
  (setf *pic-parameter-set*
  (with-parse-context (in pic-parameter-set)
    ($ pic-parameter-set-id)
    ($ seq-parameter-set-id)
    ($ entropy-coding-mode-flag)
    ($ bottom-field-pic-order-in-frame-present-flag)
    ($ num-slice-groups-minus1)
    (when (> num-slice-groups-minus1 0)
      ($ slice-group-map-type)
      (case slice-group-map-type
        ((0)
         ($ run-length-minus1-list :count (1+ num-slice-groups-minus1)))

        ((2)
         (loop WITH tmp1 = '() ; XXX:
               WITH tmp2 = '()
               REPEAT num-slice-groups-minus1
           DO (push ($ue in) tmp1) ; top-left
              (push ($ue in) tmp2) ; bottom-right
           FINALLY
           (setf top-left-list     (coerce (nreverse tmp1) '(vector $ue))
                 bottom-right-list (coerce (nreverse tmp2) '(vector $ue)))))

        ((3 4 5)
         ($ slice-group-change-direction-flag)
         ($ slice-group-change-rate-minus1))
        
        ((6)
         ($ pic-size-in-map-units-minus1)
         ($ slice-group-id-list :count (1+ pic-size-in-map-units-minus1)
                                :parse-type `($u ,(ceiling (log (1+ num-slice-groups-minus1) 2)))))
        ))
    
    ($ num-ref-idx-10-default-active-minus1)
    ($ num-ref-idx-11-default-active-minus1)
    ($ weighted-pred-flag)
    ($ weighted-bipred-idc)
    ($ pic-init-qp-minus26)
    ($ pic-init-qs-minus26)
    ($ chroma-qp-index-offset)
    ($ deblocking-filter-control-present-flag)
    ($ constrained-intra-pred-flag)
    ($ redundant-pic-cnt-present-flag)
    
    (when (h264.bit-stream:more-rbsp-data? in)
      ($ transform-8x8-mode-flag)
      ($ pic-scaling-matrix-present-flag)
      (error "Not Implemented")
      )
    )))

;;; user-data-registered
(defsyntax user-data-unregistered
  (uuid-iso-ie-11578          0 :type ($u 128))
  (payload       (empty ($u 8)) :type ($list ($u 8))))

(defun parse-sei-payload-user-data-unregistered (in size)
  (with-parse-context (in user-data-unregistered)
    ($ uuid-iso-ie-11578)
    ($ payload :count (- size (/ 128 8)))))

;;; supplemental enhancement information (SEI)
(defsyntax sei-message
  (payload-type              0 :type ($u 32)) ; XXX: type
  (payload-size              0 :type ($u 32))
  (payload                   t :type t)       ; XXX: type
  )

(deftype $sei-message () 'sei-message)

(defun parse-sei-message (in)
  (with-parse-context (in sei-message)
    (with-package (:h264.bit-stream)
      (let ((type (loop FOR byte = (read in 8)
                        SUM byte
                        WHILE (= byte #xFF)))
            (size (loop FOR byte = (read in 8)
                        SUM byte
                        WHILE (= byte #xFF))))
        (setf payload-type type
              payload-size size)
        (let ((payload-bytes (loop REPEAT size
                                   COLLECT (read in 8) INTO list
                                   FINALLY (return (coerce list 'octets)))))
          (with-input-from-octets (in2 payload-bytes)
            (setf payload
                  (ecase payload-type
                    (5 (parse-sei-payload-user-data-unregistered in2 payload-size))
                    ))
        ))))))

(defun $sei-message ()
  (parse-sei-message *default-bit-stream*))

(defvar *sei*)
(defsyntax sei
  (sei-message-list nil #|(empty $sei-message)|# :type (or null ($list $sei-message)))) ; XXX:
                    
(deftype $sei () 'sei)

(defun parse-sei (in)
  (setf *sei*
  (with-parse-context (in sei)
    ;; XXX: 暫定
    (setf sei-message-list
          (loop WHILE (h264.bit-stream:more-rbsp-data? in)
                COLLECT ($sei-message) INTO list
                FINALLY (return (coerce list '($list $sei-message)))))
    )))
(defun $sei ()
  (parse-sei *default-bit-stream*))


;;
;; TODO: いろいろ
(defsyntax ref-pic-list-modification
  (ref-pic-list-modification-flag-l0 0 :type ($u 1))
  (modification-of-pic-nums-idc      0 :type $ue)
  (abs-diff-pic-num-minus1           0 :type $ue)
  (long-term-pic-num                 0 :type $ue)
  
  (ref-pic-list-modification-flag-l1 0 :type ($u 1)))

(defun parse-rbsp-slice-trailing-bits (in)
  (parse-rbsp-trailing-bits in)
  ;; TODO: entropy-coding-mode-flagのハンドリング
  (assert (= 0 (slot-value *pic-parameter-set* 'entropy-coding-mode-flag)) () "Not Implemented")
  (assert (h264.bit-stream:eos? in) () "not eos"))

;;;
(defun parse-ref-pic-list-modification (in)
  in)
  

;;; slice-header
(defsyntax slice-header
  (first-mb-in-slice                0 :type $ue)
  (slice-type                       0 :type $ue)
  (pic-parameter-set-id             0 :type $ue)
  (colour-plane-id                  0 :type ($u 2))
  (frame-num                        0 :type $u) ; v? TODO
  (field-pic-flag                   0 :type ($u 1))
  (bottom-field-flag                0 :Type ($u 1))
  (idr-pic-id                       0 :type $ue)
  (pic-order-cnt-lsb                0 :type $u); v? TODO
  (delta-pic-order-cnt-bottom       0 :type $se)
  (delta-pic-order-cnt0             0 :type $se)
  (delta-pic-order-cnt1             0 :type $se)
  (redundant-pic-cnt                0 :type $ue)
  (direct-spatial-mv-pred-flag      0 :type ($u 1))
  (num-ref-idx-active-override-flag 0 :type ($u 1))
  (num-ref-idx-10-active-minus1     0 :type $ue)
  (num-ref-idx-11-active-minus1     0 :type $ue)
  (ref-pic-list-mvc-modification    t :type t)
  (ref-pic-list-modification         t :type t)
  (pred-weight-table                t :type t)
  (dec-ref-pic-marking              t :type t)
  (cabac-init-idc                   0 :type $ue)
  (slice-qp-delta                   0 :type $se)
  (sp-for-switch-flag               0 :type ($u 1))
  (slice-qs-delta                   0 :type $se)
  (disable-deblock-filter-idc       0 :type $ue)
  (slice-alpha-c0-offset-div2       0 :type $se)
  (slice-beta-offset-div2           0 :type $se)
  (slice-group-change-cycle         2 :type $u)) ; v? TODO

(defun parse-slice-header (in &key idr? nal-unit-type &aux slice-type-name)
  (with-parse-context (in slice-header)
    ($ first-mb-in-slice)
    ($ slice-type)
    ($ pic-parameter-set-id)
    
    (when (= 1 (slot-value *seq-parameter-set-data* 'separate-colour-plane-flag))
      ($ colour-plane-id))
    
    ($ frame-num (ceiling (log (+ 4 (slot-value *seq-parameter-set-data* 'log2-max-frame-num-minus4)) 
                               2)))
    
    (when (= 0 (slot-value *seq-parameter-set-data* 'frame-mbs-only-flag))
      ($ field-pic-flag)
      (when (= field-pic-flag 1)
        ($ bottom-field-flag)))
    
    (when idr?
      ($ idr-pic-id))

    (when (= 0 (slot-value *seq-parameter-set-data* 'pic-order-cnt-type))
      ($ pic-order-cnt-lsb (+ 4 (slot-value *seq-parameter-set-data* 'log2-max-pic-order-cnt-lsb-minus4)))
      (when (and (= 1 (slot-value *pic-parameter-set* 'bottom-field-pic-order-in-frame-present-flag))
                 (= 0 field-pic-flag))
        ($ delta-pic-order-cnt-bottom)))

    (when (and (= 1 (slot-value *seq-parameter-set-data* 'pic-order-cnt-type))
               (= 0 (slot-value *seq-parameter-set-data* 'delta-pic-order-always-zero-flag)))
      ($ delta-pic-order-cnt0)
      (when (and (= 1 (slot-value *pic-parameter-set* 'bottom-field-pic-order-in-frame-present-flag))
                 (= 0 field-pic-flag))
        ($ delta-pic-order-cnt1)))

    (when (= 1 (slot-value *pic-parameter-set* 'redundant-pic-cnt-present-flag))
      ($ redundant-pic-cnt))

    (setf slice-type-name (ecase slice-type
                            ((0 5) :P)
                            ((1 6) :B)
                            ((2 7) :I)
                            ((3 8) :SP)
                            ((4 9) :SI)))
    
    (when (eq slice-type-name :B)
      ($ direct-spatial-mv-pred-flag))

    (when (member slice-type-name '(:P :SP :B))
      ($ num-ref-idx-active-override-flag)
      (when (= 1 num-ref-idx-active-override-flag)
        ($ num-ref-idx-10-active-minus1)
        (when (eq slice-type-name :B)
          ($ num-ref-idx-11-active-minus1))))

    (if (= nal-unit-type 20)
        (error "Not Implemented: ref-pic-list-mvc-modification")
      (setf ref-pic-list-modification (parse-ref-pic-list-modification in)))

    (when (or (and (= 1 (slot-value *pic-parameter-set* 'weighted-pred-flag))
                   (member slice-type-name '(:P :SP)))
              (and (= 1 (slot-value *pic-parameter-set* 'weighted-bipred-idc))
                   (eq slice-type-name :B)))
      (error "Not Implemented: pred-wieght-table"))

    (when (/= 0 *nal-ref-idc*)
      (error "Not Implemented: dec-ref-pic-marking"))

    (when (and (= 1 (slot-value *pic-parameter-set* 'entropy-coding-mode-flag))
               (not (member slice-type-name '(:I :SI))))
      ($ cabac-init-idc))

    ($ slice-qp-delta)

    (when (member slice-type-name '(:SP :SI))
      (when (eq slice-type-name :SP)
        ($ sp-for-switch-flag))
      ($ slice-qs-delta))

    (when (= 1 (slot-value *pic-parameter-set* 'deblocking-filter-control-present-flag))
      ($ disable-deblock-filter-idc)
      (when (/= 1 disable-deblock-filter-idc)
        ($ slice-alpha-c0-offset-div2)
        ($ slice-beta-offset-div2)))

    (when (and (> (slot-value *pic-parameter-set* 'num-slice-groups-minus1) 0)
               (<= 3 (slot-value *pic-parameter-set* 'slice-group-map-type) 5))
      ($ slice-group-change-cycle
         (ceiling (log (1+ (/ (1+ (slot-value *pic-parameter-set* 'pic-size-in-map-units-minus1))
                              (1+ (slot-value *pic-parameter-set* 'slice-group-change-rate-minus1))))
                       2))
         ))
    ))

(defun parse-slice-data (in)
  (declare (ignore in))
  (error "Not Implemented"))

;;; Coded slice of an IDR picture 
(defsyntax slice-layer-without-partitioning 
  (slice-header t :type t)  ; XXX: type
  (slice-data   t :type t)) ; XXX: type

(defun parse-slice-layer-without-partitioning (in)
  (with-parse-context (in slice-layer-without-partitioning)
    (setf slice-header (parse-slice-header in :idr? t :nal-unit-type 5)
          slice-data   :todo #+C(parse-slice-data in))
    (parse-rbsp-slice-trailing-bits in)))
