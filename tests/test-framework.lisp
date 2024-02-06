(in-package #:rails-tests)

(defun load-tests ()
  (load (asdf:system-relative-pathname 'rails-generator "tests/package.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/test-framework.lisp"))

  (load (asdf:system-relative-pathname 'rails-generator "tests/model/package.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/model/expected-results.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/model/model.lisp"))

  (load (asdf:system-relative-pathname 'rails-generator "tests/controller/package.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/controller/controller.lisp"))

  (load (asdf:system-relative-pathname 'rails-generator "tests/view/package.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/view/view.lisp"))

  (load (asdf:system-relative-pathname 'rails-generator "tests/unparsing/package.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/unparsing/attribute-references.lisp"))

  (load (asdf:system-relative-pathname 'rails-generator "tests/database/package.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/database/expected-results.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/database/migrations.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/database/schema.lisp"))
  (load (asdf:system-relative-pathname 'rails-generator "tests/database/seeds.lisp"))
)

(defun test-application ()
  (let ((*application* nil))
    (define-project rails-tests
      :org         "Rails Generator"
      :name        "Rails Generator Test Application"
      :gui        ("Rails" :version "7.1.1")
      :description "")

    (define-lookup-table ("PhoneType") :keep-history? nil
      :documentation  "a set of standard labels for phone contact numbers"
      :seed-data ((Name Description)
		          ("Home" "Primary home phone number")
		          ("Work" "Primary work phone number")
		          ("Mobile" "Personal mobile phone number")
		          ("Work Mobile" "Company provided mobile phone number")))

    (define-entity ("Employee")
      :documentation
      "personal and company related data for individuals working for the organization"
      :attributes ((:entity-code)
		           :name-fields
		           :address-fields
		           :birthdate)
      :default-layouts
      (:summary (Code :casual-full-name :short-address)
       :details ((Code Nickname GivenName FamilyName)
                 :full-address)
       :context ((Code FirstLastName)))
      :repeated-attributes
      ((("Phone" "Ph. Num" "Phone Number")
        :components
        (("PhoneType" :default "main" :domain (Phonetype . Name)
                      :constraints (($not-null) ($unique)))
         (("Number")  :type phone :nullable? nil)
         (("Comment") :type memo :documentation
	      "Any additional free form comments or notes")))))

    (define-aggregation :parent-dependent
      :name "CorporateStructure"
      :parent (("Company")
               :attributes
               ((:entity-code) (:entity-name) :description
                (:entity-type ("retail" "engineering" "holding") "construction"))
               :default-layouts
               (:summary (Code Name Type Description)
                :details ((Name Code Type)
                          (Description)
                          (Divisions Staff))
                :add-fields ((Code Name Type) (Description))
                :context ((Code Name Type)))
               :documentation "the top level in the heirarchy of business structure")
      :child (("Division")
              :attributes
              ((:entity-code)
               (:entity-name)
               :description
               ("Status" :default "active" :nullable? nil :domain
                         ("active" "dormant" "archived")))
              :default-layouts
              (:summary (Code Name (OperationalManager . FirstLastName) Description)
               :details ((Name Code)
                         ((OperationalManager . FirstLastName) Status)
                         (Description))
               :edit-fields ((Code Name OperationalManager) (Description))
               :add-fields ((Code Name) (OperationalManager Status) (Description))
               :context ((Code Name (OperationalManager . FirstLastName))))
              :documentation
              "physically or operationally distinct business units"))

    (define-relationship
        ((Company (0 1)) ("hires" "are employed by") (Employee (0 *)))
        :name ("CompanyStaff")
        :lhs-properties (:name "Employer" :dependency :independent)
        :rhs-properties (:name ("Staff" :plural "Staff") :dependency :independent))

    (define-relationship
        ((Employee (1 1)) ("is the operational manager of" "are managed by") (Division (0 *)))
        :name ("OperationalManager")
        :lhs-properties (:dependency :independent :name "OperationalManager")
        :rhs-properties (:dependency :changeable))

    (define-recursive-relationship Employee (0 1) ("supervises" "are supervised by") (0 *)
      :name ("StaffReport" :short "Reporting Structure" :long "Staff Reporting Structure")
      :lhs-properties (:name "Manager")
      :rhs-properties (:name "Subordinates" :dependency :independent))
    ;; as we are not using load-project. take care of post-processing
    (soft-sim::run-integrity-checks)
    (soft-sim::post-parse-project nil)
    ;; as we are not using rails:generate. take care of rails-specific post-parsing
    (ror::conventionalize-app)
    *application*))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
