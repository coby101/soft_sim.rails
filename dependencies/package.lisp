
(defpackage #:simian.rails-generator.dependencies
  (:use
   #:cl
   #:implementation
   #:unparser
   #:rails-unparser
   )
  (:nicknames #:dependencies)
  (:export
   #:*css-components*
   #:google-address-js
   #:install-dependencies
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
