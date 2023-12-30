(defpackage simian.rails-tests
  (:nicknames #:rails-tests)
  (:use #:cl #:lisp-unit2
	#:foundation #:unparser #:interrogation #:specification
	#:ror)
  (:export
   #:load-tests
   #:test-application))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
