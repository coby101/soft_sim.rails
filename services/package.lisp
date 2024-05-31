
(defpackage #:simian.rails-generator.services
  (:use
   #:cl
   #:utilities #:interrogation #:entity #:attribute #:relationship #:foundation #:interface #:formula #:implementation #:authentication
   #:unparser
   #:rails-unparser
   )
  (:nicknames #:services)
  (:export
   #:generate-service-files
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
