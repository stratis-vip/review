(defsystem :review 
  :description "A small testing library"
  :author "Stratis Christodoulou <stratis.vip@gmail.com"
  :version 1.1.1
  
  :depends-on ()
  :pathname "src"
  :serial t
  
  :components ((:file "package")
               (:file "review"))

  :in-order-to ((test-op (test-op "review/tests"))))

(defsystem :review/tests 
  :description "Test suite for review"

  :depends-on (:review)
  
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  
   :perform (test-op (op c)
                    (uiop:symbol-call :review :run-tests
                                      ;:show-only-errors t 
                                      ;:color t
				      )))
