
(defpackage #:simian.rails-generator.app-tests
  (:use
   :cl
   ;; soft-sim packages
   :foundation :unparser :configuration :interface :relationship :entity :attribute :interrogation :utilities
   ;; other rails packages
   :authentication :implementation :rails-unparser
   )
  (:nicknames :app-tests :rails-tests)
  (:export
   #:unparse-factory
   #:write-factory-file
   #:write-factories
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
