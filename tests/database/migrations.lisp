(in-package :database-tests)

(define-test migration-tests
    (:tags '(database))
  (let ((*application* (database-test-application)))
    (assert-equal "add_column :employees, :nick_name, :string" (database::add_column (find-field :nickname :employee) nil))
    (assert-equal (add-description-to-company-migration) (database::add-attribute-migration (find-field :description :company) nil))
    (assert-equal (list "t.references :operational_manager, null: false, index: true, default: 0, foreign_key: { to_table: :employees }"
                        "t.belongs_to :company, null: false, index: true, default: 0, foreign_key: { to_table: :companies }")
                  (mapcar #'rails-unparser::unparse-table-association (database::roles-for-create-table (find-entity :division))))
    (assert-equal (add-division-migration) (database::add-entity-migration (find-entity :division) nil))
    (assert-equal (add-phonetype-migration) (database::add-entity-migration (find-entity :phonetype) nil))
    ))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
