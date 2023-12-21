
(defpackage :simian.rails-generator.controller
  (:use
   :cl 
   ;; soft-sim packages
   :unparser :interface :utilities :attribute :entity :relationship :formula :foundation :interrogation
   ;; other rails packages
   :authentication :implementation :rails-unparser :rails-config
   )
  (:nicknames :controller)
  (:export
   #:controller-class-definition
   #:write-controller-files
   #:generate-controllers
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
