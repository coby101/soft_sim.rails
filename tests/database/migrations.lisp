(in-package :database-tests)

(define-test migration-tests
    (:tags '(database))
  (let ((*application* (database-test-application)))
    (assert-equal "add_column :employees, :nick_name, :string" (database::add_column (find-field :nickname :employee) nil))
    (assert-equal (add-description-to-company-migration) (database::add-attribute-migration (find-field :description :company) nil))
    ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
