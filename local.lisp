
(in-package ror)

(setf ruby:*include-rails* t)
(setf *simple-table-layouts?* t)

(defun show-non-scoped-top-level-models()
  (remove-if #'tenant-scoped-entity? (remove-duplicates (mapcar #'entity (mapcar #'root (views *application*))))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
