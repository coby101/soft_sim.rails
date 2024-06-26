(in-package #:unparsing-tests)

(undefine-test unparse-attribute-reference-tests
    (:tags '(unparsing attribute-references))
  (let ((*application* (make-instance 'application)))
    (define-entity ("Parent")
      :attributes ((:entity-name) :email
                   ("ChildrenLabel" :type label :formula (Other Value ($eql Index Children))))
      :states (("LotsOfKids" ($>= Children 4))
               ("IsSpecial" ($eql Name (Other Value ($eql Index 0))))))
    (define-entity ("Child")
      :attributes ((:entity-name) :birthdate
                   ("EmailContact" :type email :formula (MyParent . Email)))
      :states (("ParentHasEmail" ($not-null (MyParent . Email)))))
    (define-entity ("Other")
      :attributes (("Index" :type integer)
                   ("Value" :type label)))
    (define-relationship ((Parent (1 1) "has") (Child (0 *) "have"))
                         :name ("Family")
                         :lhs-properties (:dependency :independent :name "MyParent")
                         :rhs-properties (:dependency :dependent))
    (construction::post-parse-project)
    (construction::resolve-entity-state-specifications)
    (assert-equal 2 (unparse-attribute-reference 2 nil))
    (assert-equal "two" (unparse-attribute-reference "two" nil))
    (assert-equal (get-operator :=) (unparse-attribute-reference (get-operator :=) nil))
    (assert-equal (find-entity :child) (unparse-attribute-reference (find-entity :child) nil))
    ;; attributes (with context) are handled with or without an obj-var argument
    (assert-signal 'error (unparse-attribute-reference (find-field :birthdate :child) nil))
    (assert-equal (list (get-operator :literal) "birth_date") (unparse-attribute-reference (find-field :birthdate :child) (find-entity :child)))
    (assert-equal (list (get-operator :literal) "foo.birth_date") (unparse-attribute-reference (find-field :birthdate :child) (find-entity :child) "foo"))

    ;; entity-state expressions
    (assert-equal (list (get-operator :literal) "foo.birth_date") (unparse-attribute-reference (find-state :child :parenthasemail) (find-entity :child)))
))
;    (assert-equal (find-entity :child) (unparse-attribute-reference (find-entity :child) nil))
;    (assert-equal '(1 2 3) (unparse-attribute-references '(1 2 3) nil)
;    (assert-equal '(1 2 3) (unparse-attribute-references '(1 2 3) nil)
;    (assert-equal '(1 2 3) (unparse-attribute-references '(1 2 3) nil)
;    (assert-equal '(1 2 3) (unparse-attribute-references '(1 2 3) nil)
;    (assert-equal '(1 2 3) (unparse-attribute-references '(1 2 3) nil)
;                  (assert-equal '(1 2 3) (unparse-attribute-references '(1 2 3) nil)
;                                
;    manage employer companytype
;    employer status
;    (assert-equal (list (get-operator :literal) "manager.employer.company_type" )
;                  (assert-equal (division-status-meta-data-hash) (ruby:unparse-hash (model::entity-meta-data (find-entity :divisionstatus))))
;                  (assert-equal (division-status-meta-data-method) (with-nesting (with-output-to-string (result) (model::meta_data (find-entity :divisionstatus) result))))
;                  ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
