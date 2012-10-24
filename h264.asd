(require :asdf)

(defsystem h264
  :name "h264"
  
  :serial t
  :depends-on (:flexi-streams) ; :sb-mop)
  :components ((:file "package")
               
               (:file "common/package")

               (:file "bit-stream/package")
               (:file "bit-stream/bit-stream")

               (:file "parser/package")
               (:file "parser/exp-golomb-code")
               (:file "parser/parser")

               (:file "h264")))
