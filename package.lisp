;;;===============================================================================
;;;
;;;   Ruby on Rails application generator for a Software Simian application model
;;;
;;;===============================================================================

(defpackage simian.rails-generator
  (:use #:cl #:cl-inflector
        #:soft-sim #:utilities #:entity #:attribute #:foundation #:interrogation #:construction #:unparser
        #:implementation #:model #:view #:controller #:database #:dependencies #:authentication
        #:rails-unparser #:rails-config #:app-tests #:app-tasks)
  (:nicknames #:ror #:rails)
  (:export #:generate))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
