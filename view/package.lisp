
(defpackage :simian.rails-generator.view
  (:use
   :cl :unparser :configuration :interface :interrogation :utilities :attribute
   :entity :relationship :implementation :foundation
   :rails-unparser :rails-config :authentication :dependencies)
  (:nicknames :view)
  (:export
   #:generate-views
   #:generate-view
   #:generate-partials
   #:write-view-files
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
