
(in-package #:rails)

(setf *simple-table-layouts?* t)

(defun show-non-scoped-top-level-models()
  (remove-if #'tenant-scoped-entity? (remove-duplicates (mapcar #'entity (mapcar #'root (views *application*))))))

(defun test-it-all ()
  (ql:quickload :rails-generator)
  (soft-sim.tests:load-all-unparsers)
  (soft-sim.tests:load-tests)
  (rails-tests:load-tests)
  (soft-sim.tests:print-all-tests)
  (rails:generate (soft-sim.tests:load-demo))
  )

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
