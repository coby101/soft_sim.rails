
(defpackage :simian.rails-generator.model
  (:use
   :cl
   :utilities :interrogation :entity :attribute :relationship :foundation :interface :formula :implementation :authentication
   :unparser
   :rails-unparser
   )
  (:nicknames :model)
  (:export
   #:model-entities
   #:generate-model-files
   #:write-model-class
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
