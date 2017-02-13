;;;; ql-utils.asd

(asdf:defsystem #:ql-utils
  :description "A set of auxulary utilities around QuickLisp"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-html-parse
               #:alexandria
               #:drakma)
  :serial t
  :components ((:file "ql-utils")))
