;;;===========================================================================
;;;
;;;   code for generating schema definitions
;;;
;;;===========================================================================

(in-package :database)

(defun database-tables(&optional (app *application*))
  (remove-if #'(lambda(ent)
                 (or nil ;(typep ent 'audit-entity)
                     (typep ent 'specialized-entity)))
             (schema app)))

(defmethod assemble-table-options ((ent associative-entity)) (call-next-method))
(defmethod assemble-table-options ((ent entity))
  (remove nil
    (list
     (when *dev-mode*
       "force: true"))))

(defmethod create_table-arguments ((ent entity))
  (format nil ":~a~{, ~a~}" (schema-name ent)
          (assemble-table-options ent)))

(defmethod database-attributes ((entity entity))
  (append
   (list (primary-key entity))
   (foreign-keys entity)
   (user-attributes entity)
   (cached-inherits entity)
   (cached-calculations entity)
   (summary-attributes entity)))

(defmethod database-attributes ((entity generalized-entity))
  (remove nil
   (append
    (list (find-field :modeltype entity))
    (call-next-method)
    (remove-if #'(lambda(att)
                   (or (typep att 'primary-key)
                       (and (typep att 'cached-inheritance)
                            (eq (my-entity (source att)) entity))))
               (apply #'append (mapcar #'database-attributes (subclasses entity)))))))

;; default is specify no options
(defmethod limit-column-option     ((type t)) nil)
(defmethod scale-column-option     ((type t)) nil)
(defmethod precision-column-option ((type t)) nil)

;; provide some t.column options according to the column's logical type
(defmethod limit-column-option     ((type (eql :code)))  (format nil "limit: 10"))
(defmethod scale-column-option     ((type (eql :money))) (format nil "scale: 3"))
(defmethod precision-column-option ((type (eql :money))) (format nil "precision: 15"))

(defun assemble-column-options (att)
  (let ((type-id (id (logical-type att))))
    (remove nil
            (list
             (when (or (unique? att) (indexed? att))
               (format nil "index: ~a" (if (and (unique? att)
                                                (not (nullable? att))
                                                (not (tenant-scoped-entity? (my-entity att))))
                                           ;; there is no unique constraint on a null field
                                           "{ unique: true }" "true")))
             (when (sql:unparsable-default? att)
               (let ((default (default-value att)))
                 (format nil "default: ~a"
                         (cond
                           ((and (eql (data-type att) :boolean) (not (implement-as-string? att)))
                            (if (member (default-value att)
                                        (list "yes" "on" "true") :test #'string-equal)
                                "true" "false"))
                           ((or (stringp default)
                                (numberp default))
                            (unparse-expression default :sql))
                           (t (format nil "-> { \"~a\" }" (unparse-expression default :sql)))))))
             (unless (nullable? att)
               (format nil "null: false"))
             (limit-column-option type-id)
             (precision-column-option type-id)
             (scale-column-option type-id)))))
;          (format nil ":comment ~s" (description att)))))

;; https://api.rubyonrails.org/classes/ActiveRecord/ConnectionAdapters/SchemaStatements.html#method-i-add_column
(defmethod t.column ((attribute composite-key)     &optional stream) (declare (ignorable stream)) nil)
(defmethod t.column ((attribute foreign-key)       &optional stream) (declare (ignorable stream)) nil)
(defmethod t.column ((attribute primary-key)       &optional stream) (declare (ignorable stream)) nil)
(defmethod t.column ((attribute summary-attribute) &optional stream) (declare (ignorable stream)) nil)
(defmethod t.column ((attribute cached-summary)    &optional stream)
  (let ((options (assemble-column-options attribute)))
    (format stream "t.~a :~a~a" (unparse-datatype (logical-type attribute))
            (schema-name attribute)
            (if options
                (format nil ", ~{~a~^, ~}" options)
                ""))))

(defmethod t.column ((attribute persistent-attribute) &optional stream)
  (let ((options (assemble-column-options attribute)))
    (format stream "t.~a :~a~a"
            (if (implement-as-string? attribute)
                "string"
                (unparse-datatype (logical-type attribute)))
            (schema-name attribute)
            (if options
                (format nil ", ~{~a~^, ~}" options)
                ""))))

(defmethod roles-for-create-table ((ent entity))
  (remove-if #'(lambda(r)
                 (let ((relationship (if (typep r 'shared-relation) (car (relationships r)) (my-relationship r))))
                   (typep relationship 'specialization)))
             (my-roles ent)))

(defmethod roles-for-create-table ((ent generalized-entity))
  (append
   (call-next-method)
   (apply #'append
          (mapcar #'roles-for-create-table (subclasses ent)))))

;; https://api.rubyonrails.org/classes/ActiveRecord/Schema.html
#|
for associative tables "id: false"
"force: true" will drop the table if it exists. good for development
|#
(defmethod create_table ((entity symbol) &optional stream)
  (create_table (find-entity entity) (or stream t)))

;; https://api.rubyonrails.org/classes/ActiveRecord/ConnectionAdapters/SchemaStatements.html#method-i-create_table
(defmethod change_table ((table entity) &optional stream)
  (let ((t.references
          (remove nil (mapcar #'unparse-table-association
                              (roles-for-create-table table)))))
    (when t.references
      (format stream "~achange_table :~a do |t|" (make-indent) (schema-name table))
      (with-nesting
          (let ((fmt-str (format nil "~~{~%~a~~a~~}" (make-indent))))
            (format stream fmt-str t.references)))
      (format stream "~%~aend" (make-indent)))))

(defmethod create_table ((table entity) &optional stream)
  (let ((attributes (sort (copy-list (database-attributes table))
                          #'string-lessp :key #'name)))
    (format stream "~acreate_table(~a) do |t|~%" (make-indent) (create_table-arguments table))
    (with-nesting
      (let ((fmt-str (format nil "~~{~%~a~~a~~}" (make-indent)))
            (assocs (roles-for-create-table table)))
        (format stream fmt-str (remove nil (mapcar #'t.column attributes)))
        (format stream "~%~at.timestamps~%" (make-indent))
        (when assocs
          (comment-out stream " associations are realized in the change_table method")
          (comment-out stream fmt-str (remove nil (mapcar #'unparse-table-association assocs))))))
    (format stream "~%~aend~%" (make-indent))))

(defmethod create_table ((entity specialized-entity) &optional stream)
  (declare (ignorable stream))
  (error "With Rails STI mechanism there should not be a database table for a subclass entity (~a)" entity))

(defmethod change_table ((entity specialized-entity) &optional stream)
  (declare (ignorable stream))
  (error "With Rails STI mechanism there should not be a database table for a subclass entity (~a)" entity))

(defmethod required-extensions ((db database-platform))
  (remove nil
          (list
           (when (string-equal (name db) "PostgreSQL")
             "enable_extension \"plpgsql\""))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
