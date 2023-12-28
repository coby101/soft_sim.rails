
(defpackage #:simian.rails-generator.database
  (:use
   #:cl #:interface #:utilities #:attribute #:entity #:relationship #:formula #:foundation #:unparser #:interrogation
   #:rails-unparser #:implementation)
  (:nicknames #:database)
  (:export
   #:schema.rb
   #:seeds.rb
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
