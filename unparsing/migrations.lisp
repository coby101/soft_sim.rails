;;;====================================================================
;;;
;;;   Code for unparsing code in schema definition and migration files
;;;
;;;====================================================================
 
(in-package :rails-unparser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Association code to go in the schema definition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod format-table-association ((my-relationship t) (side t))
  (comment-with-warning
   nil "no format-table-association method written for ~a (~a)"
   (type-of my-relationship) my-relationship))

(defmethod format-table-association ((relationship many-to-many) (side t)) nil)
(defmethod format-table-association ((relationship specialization) (side t)) nil)
(defmethod format-table-association ((relationship t) (side (eql :left))) nil)

(defun write-table-reference (alias name entity &key (indexed? t) nullable? default)
  (format nil "t.~a :~a, null: ~a~a~a, foreign_key: ~a"
          alias name (if nullable? "true" "false") (if indexed? ", index: true" "")
          (if default (format nil ", default: ~a" default) "")
          (if (string-equal name entity)
              "true"
              (format nil "{ to_table: :~a }" entity))))

(defmethod format-table-association ((relationship recursive-relationship) (side (eql :left)))
  (when (eql 1 (multiplicity-max (lhs relationship)))
    (write-table-reference "references"
                           (snake-case (name (lhs relationship)))
                           (schema-name (entity (lhs relationship)))
                           :nullable? t)))

(defmethod format-table-association ((relationship recursive-relationship) (side (eql :right)))
  (when (eql 1 (multiplicity-max (rhs relationship)))
      (write-table-reference "references"
                             (snake-case (name (rhs relationship)))
                             (schema-name (entity (rhs relationship)))
                             :nullable? t)))

(defmethod format-table-association ((relationship one-to-many) (side (eql :right)))
  (let* ((lhs-ent (entity (lhs relationship)))
         (alias (if (or (typep relationship 'aggregation)
                        (member lhs-ent (associates (entity (rhs relationship)))))
                    "belongs_to" "references"))
         (fk-name (name (lhs relationship)))
         (fk (find-field fk-name (entity (rhs relationship))))
         (default (when (sql:unparsable-default? fk) (default-value fk)))
         (specialized-rhs? (typep (entity (rhs relationship)) 'specialized-entity)))
    (write-table-reference alias
         (snake-case fk-name)
         (schema-name lhs-ent)
         :default (and (not specialized-rhs?) default)
         :nullable? (or (equal (multiplicity-min (lhs relationship)) 0)
                        specialized-rhs?))))

(defmethod format-table-association ((relationship xor-relationship) (side (eql :right)))
  (format nil "t.references :~a, polymorphic: true, null: ~a"
          (snake-case (name (lhs relationship)))
          (if (equal (multiplicity-min (lhs relationship)) 0) "true" "false")))

(defmethod my-side ((me relation))
  (if (eq me (lhs (my-relationship me))) :left :right))
(defmethod my-side ((me shared-relation))
  (if (eq me (lhs (car (relationships me)))) :left :right))

(defmethod unparse-table-association ((rel relation))
  (format-table-association (my-relationship rel) (my-side rel)))
(defmethod unparse-table-association ((rel shared-relation))
  (format-table-association (car (relationships rel)) (my-side rel)))

(defmethod unparse-datatype ((type symbol))  nil)
(defmethod unparse-datatype ((type logical-type))
  (or (unparse-datatype (id type))
      (unparse-datatype (data-type type))))

;; https://stackoverflow.com/questions/11889048/is-there-documentation-for-the-rails-column-types
(defmethod unparse-datatype ((sym (eql :string)))     "string")
(defmethod unparse-datatype ((sym (eql :memo)))       "text")
(defmethod unparse-datatype ((sym (eql :long-text)))  "text")
(defmethod unparse-datatype ((sym (eql :short-text))) "string")
(defmethod unparse-datatype ((sym (eql :label)))      "string")
(defmethod unparse-datatype ((sym (eql :name)))       "string")
(defmethod unparse-datatype ((sym (eql :code)))       "string")
(defmethod unparse-datatype ((sym (eql :datetime)))   "timestamp")
(defmethod unparse-datatype ((sym (eql :integer)))    "integer")
(defmethod unparse-datatype ((sym (eql :date)))       "date")
(defmethod unparse-datatype ((sym (eql :boolean)))    "boolean")
(defmethod unparse-datatype ((sym (eql :float)))      "decimal")
(defmethod unparse-datatype ((sym (eql :money)))      "decimal")
(defmethod unparse-datatype ((sym (eql :sequence)))   "integer")
(defmethod unparse-datatype ((sym (eql :float)))      "decimal")
(defmethod unparse-datatype ((sym (eql :float)))      "decimal")
(defmethod unparse-datatype ((sym (eql :float)))      "decimal")

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
