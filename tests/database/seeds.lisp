(in-package :database-tests)

(define-test seed-data-tests
    (:tags '(database))
  (let ((*application* (database-test-application)))
    (assert-equal (phone-type-data-import) (with-output-to-string (stream) (database::seed-data-import-statement (find-entity :phonetype) stream)))
    ))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
