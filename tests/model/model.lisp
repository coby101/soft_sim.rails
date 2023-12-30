(in-package #:model-tests)

(define-test meta-data-tests
    (:tags '(model))
  (let ((*application* (model-test-application)))
    (assert-equal (division-status-meta-data) (model::entity-meta-data (find-entity :divisionstatus)))
    (assert-equal (division-status-meta-data-hash) (ruby:unparse-hash (model::entity-meta-data (find-entity :divisionstatus))))
    (assert-equal (division-status-meta-data-method) (with-nesting (with-output-to-string (result) (model::meta_data (find-entity :divisionstatus) result))))
    ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
