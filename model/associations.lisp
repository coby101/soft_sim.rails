;;;===========================================================================
;;;
;;; code associated with generating the model validations
;;; 
;;;===========================================================================

(in-package #:model)

(defmethod format-model-association ((rel specialization) (side t)) nil)
(defmethod format-model-association ((my-relationship t) (side t))
  (comment-with-warning
   nil "no format-model-association method written ~%for ~a (~a)"
   (type-of my-relationship) my-relationship))

(defmethod format-model-association ((relationship recursive-relationship) (side t))
  (let* ((my-relation (if (eql side :right) (lhs relationship) (rhs relationship)))
         (relation-name (name my-relation))
         (model-name (name (entity my-relation)))
         (mult-max (or (multiplicity-max my-relation) 2))
         (association-options '())
         (association-type nil))
    (if (> mult-max 1)
        (progn
          (setf association-type "has_many")
          (push (list "foreign_key"
                      (unparse (strcat (instance-name (name (my-relation my-relation))) "_id") :rails))
                association-options))
        (progn
          (when (eql :independent (dependency (rhs relationship)))
            (push (list "optional" "true") association-options))
          (setf association-type "belongs_to")))
    (unless (string= relation-name model-name)
      (push (list "class_name" (format nil "~s" model-name)) association-options))
    (apply #'unparse-model-association association-type
           (instance-name my-relation) association-options)))

(defmethod format-model-association ((rel one-to-many) (side (eql :right)))
  (let* ((lhs (lhs rel))
         (relation-name (name lhs))
         (model-name (name (entity lhs)))
         (association-options '()))
    (when (eql :independent (dependency (rhs rel)))
      (push (list "optional" "true") association-options))
    (unless (string= relation-name model-name)
      (push (list "class_name" (format nil "~s" model-name)) association-options))
    (apply #'unparse-model-association
           "belongs_to" (instance-name relation-name) association-options)))

(defmethod format-model-association ((rel xor-relationship) (side (eql :right)))
  (let* ((lhs (lhs rel))
         (relation-name (name lhs))
;         (model-name (name (entity lhs)))
         (association-options '(("polymorphic" "true"))))
    (when (eql :independent (dependency (rhs rel)))
      (push (list "optional" "true") association-options))
    (apply #'unparse-model-association 
           "belongs_to" (instance-name relation-name) association-options)))

(defmethod format-model-association ((rel aggregation) (side (eql :right)))
  (let* ((main-declaration (call-next-method))
         (parent (entity (lhs rel)))
         (ancestors 
          (mapcar #'lhs (remove-if-not #'(lambda (r)
                                            (and (not (typep r 'many-to-many))
                                                 (eq parent (entity (rhs r)))
                                                 (not (member (entity (lhs r)) (parents (entity (rhs rel)))))))
                        (relationships parent)))))
    (format nil "~a~{~%  ~a~}" main-declaration
            (when ancestors
              (loop for anc in ancestors
                 collect (unparse-model-association
                          "has_one" (instance-name anc)
                          (list "through"
                                (format nil ":~a~a" (instance-name parent)
                                        (if (string-equal (name parent) (name (lhs rel)))
                                            ""
                                            (format nil ", source: :~a" (snake-case (name (lhs rel)))))))))))))

(defun format-accepts_nested_attributes_for (rel)
  (if (typep (entity (rhs rel)) 'attribute-table)
      (format nil "~%  accepts_nested_attributes_for :~a, allow_destroy: true, reject_if: :all_blank"
              (model-plural (entity (rhs rel))))
      ""))

(defun format-validates_associated (rel)
  (if (or (typep (entity (rhs rel)) 'attribute-table)
          (typep rel 'aggregation))
      (format nil "~%  validates_associated :~a" (model-plural (entity (rhs rel))))
      ""))

(defun formated-association-shortcuts (rel)
  (let* ((rhs (entity (rhs rel)))
         (descendents
           (remove-if
            #'(lambda (ent)
                (find ent
                      (mapcar #'rhs (remove rel (relationships (entity (lhs rel)))))
                      :key #'entity))
            (aggregate-children rhs))))
    (loop for desc in descendents
          collect (unparse-model-association
                   "has_many" (model-plural desc)
                   (list "through" (strcat ":" (snake-case (plural (rhs rel)))))))))

(defmethod format-model-association ((rel one-to-many) (side (eql :left)))
  (when (navigable? (lhs rel))
    (let ((options '())
          (non-standard-rhs (unless (string= (name (rhs rel)) (name (entity (rhs rel))))
                               (snake-case (plural (rhs rel)))))
          (non-standard-lhs (unless (string= (name (lhs rel)) (name (entity (lhs rel))))
                               (instance-name (lhs rel)))))
      (when non-standard-lhs
        (push (list "foreign_key" (format nil "\"~a_id\"" non-standard-lhs)) options))
      (when non-standard-rhs
        (push (list "class_name" (format nil "~s" (name (entity (rhs rel))))) options))
      (when (or (typep (entity (rhs rel)) 'attribute-table)
                (typep rel 'aggregation))
        (push (list "dependent" ":destroy") options))
      (let ((main-declaration (apply #'unparse-model-association "has_many"
                                     (or non-standard-rhs (model-plural (entity (rhs rel))))
                                     options))
            (assoc-validation (format-validates_associated rel))
            (accept-nesting (format-accepts_nested_attributes_for rel))
            (short-cuts (formated-association-shortcuts rel)))
        (format nil "~a~a~a~{~%  ~a~}" main-declaration assoc-validation accept-nesting short-cuts)))))

(defmethod format-model-association ((rel xor-relationship) (side (eql :left)))
  (when (navigable? (lhs rel))
    (let ((options (list (list "as" (strcat ":" (instance-name (lhs rel))))))
          (non-standard-rhs (unless (string= (name (rhs rel)) (name (entity (rhs rel))))
                              (snake-case (plural (rhs rel))))))
      (apply #'unparse-model-association "has_many"
             (or non-standard-rhs (model-plural (entity (rhs rel))))
             options))))

(defun many-to-many-declaration (child assoc-ent)
  (if (or (user-attributes assoc-ent) (not (has-default-name? assoc-ent)))
      (unparse-model-association "has_many" (model-plural child)
                           (list "through" (strcat ":" (model-plural assoc-ent))))
      (unparse-model-association "has_and_belongs_to_many" (model-plural child)
                           (list "join_table" (unparse (model-plural assoc-ent) :rails)))))

(defmethod format-model-association ((rel many-to-many) (side (eql :right)))
  (let ((assoc-ent (associated-entity rel)))
    (many-to-many-declaration (entity (lhs rel)) assoc-ent)))
(defmethod format-model-association ((rel many-to-many) (side (eql :left)))
  (let ((assoc-ent (associated-entity rel)))
    (many-to-many-declaration (entity (rhs rel)) assoc-ent)))

(defmethod declare-model-association ((rel relation))
  (format-model-association (my-relationship rel) (my-side rel)))
(defmethod declare-model-association ((rel shared-relation))
  (format-model-association (car (relationships rel)) (my-side rel)))

#|

all of this seems completely unused, delete it when emotionally ready..

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Associations for migrations and schema.rb code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unparse-migration-association (type relation &rest options)
  (format nil "~a :~a~{, ~a~}" type relation
          (mapcar #'(lambda(opt)
                      (format nil "~a: ~a" (car opt) (cadr opt)))
                  (remove nil options))))

(defmethod format-migration-association ((relationship specialization) (side t))
  nil)

(defmethod format-migration-association ((relationship recursive-relationship) (side t))
  (let* ((my-relation (if (eql side :right) (lhs relationship) (rhs relationship)))
         (relation-name (name my-relation))
         (model-name (name (entity my-relation)))
         (mult-max (or (multiplicity-max my-relation) 2))
         (association-options '())
         (association-type nil))
    (if (> mult-max 1)
        (progn
          (setf association-type "has_many")
          (push (list "foreign_key"
                      (unparse (strcat (instance-name (name (my-relation my-relation))) "_id") :rails))
                association-options))
        (setf association-type "belongs_to"))
    (unless (string= relation-name model-name)
      (push (list "class_name" (format nil "~s" model-name)) association-options))
    (apply #'unparse-migration-association association-type
           (format nil "~a~p" (instance-name relation-name) mult-max) association-options)))

(defmethod format-migration-association ((relationship one-to-many) (side (eql :right)))
  (let* ((relation-name (name (lhs relationship)))
         (model-name (name (entity (lhs relationship))))
         (association-options '()))
    (unless (string= relation-name model-name)
      (push (list "class_name" (format nil "~s" model-name)) association-options))
    (apply #'unparse-migration-association "belongs_to" (instance-name relation-name) association-options)))

(defmethod format-migration-association ((rel one-to-many) (side (eql :left)))
  (when (navigable? (lhs rel))
    (format nil "has_many :~a~{~a~}" (schema-name (entity (rhs rel)))
            (let* ((rhs (entity (rhs rel)))
                   (descendents (aggregate-children rhs)))
              (when descendents
                (loop for desc in descendents
                   when (null (find desc (mapcar #'rhs
                                                 (remove rel (relationships (entity (lhs rel)))))
                                    :key #'entity))
                   collect (format nil "~%~ahas_many :~a, :through ~a" (make-indent)
                                   (schema-name desc) (schema-name rhs))))))))

(defmethod format-migration-association ((rel one-to-many) (side (eql :left)))
  (when (navigable? (lhs rel))
    (let ((main-declaration (unparse-migration-association "has_many" (schema-name (entity (rhs rel)))))
          (mva-att-validation
           (if (typep (entity (rhs rel)) 'attribute-table)
               (format nil "~%  validates_associated :~a" (schema-name (entity (rhs rel))))
               ""))
          (short-cuts (let* ((rhs (entity (rhs rel)))
                             (descendents (aggregate-children rhs)))
                        (when descendents
                          (loop for desc in descendents
                             when (null (find desc (mapcar #'rhs
                                                           (remove rel (relationships (entity (lhs rel)))))
                                              :key #'entity))
                             collect (unparse-migration-association
                                      "has_many" (schema-name desc)
                                      (list "through" (strcat ":" (schema-name rhs)))))))))
      (format nil "~a~a~{~%  ~a~}" main-declaration mva-att-validation short-cuts))))


(defmethod format-migration-association ((rel aggregation) (side (eql :left)))
  (format nil "~a~%  validates_associated :~a" (call-next-method) (schema-name (entity (rhs rel)))))


(defmethod format-migration-association ((rel many-to-many) (side (eql :right)))
  (let ((assoc-ent (associated-entity rel)))
    (many-to-many-declaration (entity (lhs rel)) assoc-ent)))
(defmethod format-migration-association ((rel many-to-many) (side (eql :left)))
  (let ((assoc-ent (associated-entity rel)))
    (many-to-many-declaration (entity (rhs rel)) assoc-ent)))

(defun many-to-many-declaration (child assoc-ent)
  (if (or (user-attributes assoc-ent) (not (has-default-name? assoc-ent)))
      (unparse-migration-association "has_many" (model-plural child)
                           (list "through" (strcat ":" (model-plural assoc-ent))))
      (unparse-migration-association "has_and_belongs_to_many" (model-plural child)
                           (list "join_table" (unparse (model-plural assoc-ent) :rails)))))


(defmethod format-migration-association ((rel specialization) (side (eql :right)))
  (comment-with-warning
   nil "no SPECIALIZATION format-migration-association method written to generalize ~a"
   (schema-name (entity (lhs rel)))))
(defmethod format-migration-association ((rel specialization) (side (eql :left)))
  (comment-with-warning
   nil "no SPECIALIZATION format-migration-association method written to  specialize ~a"
   (schema-name (entity (lhs rel)))))

|#
;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
