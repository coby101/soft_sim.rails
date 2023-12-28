
(defpackage #:simian.rails-generator.config
  (:use
   #:cl #:cl-inflector
   #:foundation
   #:configuration
   #:unparser
   #:interface
   #:entity
   #:attribute
   #:implementation
   #:interrogation
   #:utilities
   #:authentication
   #:rails-unparser
   )
  (:nicknames #:rails-config #:app-config)
  (:export
   #:*locale*
   #:*show-button-text*
   #:*delete-conf*
   #:configure-inflections
   #:implemented-actions
   #:generated.yml
   #:routes.rb
   #:unparse-routes
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
