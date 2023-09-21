(asdf:defsystem #:layers
  :version "0.1"
  :default-component-class "cl-source-file.lsp"
  :description "generative music library"
  :author "Leon Focker"
  :license "GNU General Public License v3.0"
  :depends-on (:cl-pcg) ;(:clm :slippery-chicken)
  :serial t
  :components ((:file "src/all")))

;; EOF layers.asd
