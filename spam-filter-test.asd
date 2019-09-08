;; (require \'asdf)
 
 (in-package :cl-user)
 (defpackage spam-filter-test-asd
 (:use :cl :asdf))
 (in-package :spam-filter-test-asd)
 
 (defsystem spam-filter-test
 :depends-on (:spam-filter)
 :version "1.0.0"
 :author "wasu"
 :license "MIT"
 :components ((:module "t" :components ((:file "spam-filter-test"))))
 :perform (test-op :after (op c)
 (funcall (intern #.(string :run) :prove) c)))

