
(defpackage :simian.rails-generator.app-tasks
  (:use
   :cl
   ;; soft-sim packages
   :foundation :unparser :configuration :interface :relationship :entity :attribute :interrogation :utilities
   ;; other rails packages
   :authentication :implementation :rails-unparser
   )
  (:nicknames :app-tasks :rails-tasks)
  (:export
   #:write-data-load-tasks
 ;  #:WIP
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
