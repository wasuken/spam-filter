;; (require \'asdf)
 
 (in-package :cl-user)
 (defpackage spam-filter-asd
 (:use :cl :asdf))
 (in-package :spam-filter-asd)
 
 (defsystem :spam-filter
 :version "1.0.0"
 :author "wasu"
 :license "MIT"
 :components ((:file "package")
 (:module "src" :components ((:file "spam-filter")))))

