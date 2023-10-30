;;;===========================================================================
;;; file:   migrations.lisp
;;; auth:   Coby Beck
;;; date:   2020-12-04
;;;
;;;---------------------------------------------------------------------------
;;;   code associated with generating the database
;;;   schema via ActiveRecord::Migration classes
;;;---------------------------------------------------------------------------  
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

;; https://api.rubyonrails.org/classes/ActiveRecord/Schema.html
(defun schema.rb (&optional (app *application*))
  (let ((file (schema-file-path))
        (entities (sort (copy-list (database-tables app))
                        #'string-lessp :key #'name)))
    (with-open-file (schema-file file :direction :output :if-exists :supersede)
      (format-file-notice schema-file "schema.rb")
      (format schema-file "ActiveRecord::Schema.define() do~%")
      (format schema-file "~{~%  ~a~}~%" (required-extensions (db-platform *implementation*)))
      (terpri schema-file)
      (with-nesting
        (dolist (ent entities)
          (create_table ent schema-file)
          (terpri schema-file))
        (dolist (ent entities)
          (terpri schema-file)
          (change_table ent schema-file)
          (terpri schema-file)))
      (format schema-file "~%end~%"))))

(defun seeds.rb (&optional (app *application*))
  (let ((file (seed-file-path))
        (entities (seed-data-order (database-tables app))))
    (with-open-file (seed-file file :direction :output :if-exists :supersede)
      (format-file-notice seed-file "seeds.rb")
      (dolist (ent entities)
        (when (and (seed-data ent)
                   (not (tenant-scoped-entity? ent))
                   (not (eq (tenant-entity) ent)))
          (terpri seed-file)
          (create-seed-data ent seed-file)
          (terpri seed-file))))))

(defun seed-data-order (entities)
  (let* ((app-ent (application-entity))
         (tenant   (tenant-entity))
         (cal (calendar-entities))
         (non-cal (set-difference entities cal))
         (weak (remove-if-not #'(lambda (e) (typep e 'weak-entity)) non-cal))
         (regular (remove app-ent (remove-if #'(lambda (e) (typep e 'weak-entity)) non-cal))))
    (append (when app-ent (list app-ent))
            (when tenant (list tenant))
            (when *load-calendar-framework* cal)
            (sort regular #'string-lessp :key #'name)
            (sort weak #'string-lessp :key #'name))))

(defun active-record-create-class-declaration (name)
  (format nil "class Create~a < ActiveRecord::Migration[6.0]" name))

(defun database-tables(&optional (app *application*))
  (remove-if #'(lambda(ent)
                 (or (typep ent 'audit-entity)
                     (typep ent 'specialized-entity)))
             (schema app)))

(defun write-create-application-class(&optional stream (app *application*))
  (format stream (active-record-create-class-declaration (plural app)))
  (let ((*nesting-level* (+ 2 *nesting-level*)))
    (format stream "~%  def up~%")
    (dolist (table (database-tables app))
      (create_table table stream))
    (format stream "~%  end~%")
    (format stream "~%  def down~%")
    (dolist (table (database-tables app))
      (format stream "~%  drop_table :~a" (schema-name table)))
    (format stream "~%  end~%"))
  (format stream "end~%"))

#|
for associative tables "id: false"
"force: true" will drop the table if it exists. good for development
|#
(defmethod create_table-arguments ((ent entity))
  (format nil ":~a~{, ~a~}" (schema-name ent)
          (assemble-table-options ent)))

(defmethod assemble-table-options ((ent entity))
  (remove nil
    (list
     (when *dev-mode*
       "force: true"))))

(defmethod assemble-table-options ((ent associative-entity))
  (call-next-method))

(defmethod create_table ((entity symbol) &optional stream)
  (create_table (find-entity entity) (or stream t)))

(defmethod create_table ((entity specialized-entity) &optional stream)
  (declare (ignorable stream))
  (error "With Rails STI mechanism there should not be a database ~
          table for a subclass entity (~a)" entity))

(defmethod change_table ((entity specialized-entity) &optional stream)
  (declare (ignorable stream))
  (error "With Rails STI mechanism there should not be a database ~
          table for a subclass entity (~a)" entity))

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

(defmethod database-attributes ((entity entity))
  (append
   (list (primary-key entity))
   (foreign-keys entity) ;; (maybe this is part of table associations?
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

;; https://api.rubyonrails.org/classes/ActiveRecord/ConnectionAdapters/SchemaStatements.html#method-i-add_column
(defmethod t.column ((attribute composite-key) &optional stream)
  (declare (ignorable stream))
  nil)

(defmethod t.column ((attribute primary-key) &optional stream)
  (declare (ignorable stream))
  nil)

(defmethod t.column ((attribute summary-attribute) &optional stream)
  (declare (ignorable stream))
  nil)

(defmethod t.column ((attribute cached-summary) &optional stream)
  (let ((options (assemble-column-options attribute)))
    (format stream "t.~a :~a~a" (ruby:unparse-datatype (logical-type attribute))
            (schema-name attribute)
            (if options
                (format nil ", ~{~a~^, ~}" options)
                ""))))

(defmethod t.column ((attribute persistent-attribute) &optional stream)
  (let ((options (assemble-column-options attribute)))
    (format stream "t.~a :~a~a"
            (if (implement-as-string? attribute)
                "string"
                (ruby:unparse-datatype (logical-type attribute)))
            (schema-name attribute)
            (if options
                (format nil ", ~{~a~^, ~}" options)
                ""))))

(defmethod limit-column-option ((type t)) nil)
(defmethod limit-column-option ((type (eql :code)))
  (format nil "limit: 10"))
(defmethod scale-column-option ((type t)) nil)
(defmethod scale-column-option ((type (eql :money)))
  (format nil "scale: 3"))
(defmethod precision-column-option ((type t)) nil)
(defmethod precision-column-option ((type (eql :money)))
  (format nil "precision: 15"))

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
             (when (sql-parsable-default? att)
               (let ((default (default-value att)))
                 (format nil "default: ~a"
                         (cond
                           ((and (eql (data-type att) :boolean) (not (implement-as-string? att)))
                            (if (member (default-value att)
                                        (list "yes" "on" "true") :test #'string-equal)
                                "true" "false"))
                           ((or (stringp default)
                                (numberp default))
                            (sql:unparse-expression default))
                           (t (format nil "-> { \"~a\" }" (sql:unparse-expression default)))))))
             (unless (nullable? att)
               (format nil "null: false"))
             (limit-column-option type-id)
             (precision-column-option type-id)
             (scale-column-option type-id)))))
;          (format nil ":comment ~s" (description att)))))

(defun sql-parsable-default? (att)
  (let ((default (default-value att)))
    (when default
      (or (stringp default)
          (numberp default)
          (when (typep default 'formula)
            (let ((op (operator-key (car (expression default)))))
                   (member op '($current-timestamp $current-date $next-sequence-value))))))))

(defmethod t.column ((attribute foreign-key) &optional stream)
  (declare (ignorable stream))
  nil)

(defun create-seed-method () "db_import!")

;;; WIP in soft_sim data-set.lisp COLUMNS DATA-ROWS MAKE-DATA-SEED
(defun columns (arg) arg)
(defun data-rows (arg) arg)
(defun make-data-seed (arg1 arg2) (values arg1 arg2))
(defun create-seed-data (ent stream &optional data-seed)
  (let* ((data (or data-seed (make-data-seed ent (ignore-errors (seed-data ent))))))
    (when data
      (format stream "~a.~a([~%~{~a~^,~%~}~%])" (model-name ent) (create-seed-method) 
              (let* ((columns (columns data))
                     (column-count (length columns)))
                (mapcar #'(lambda(row)
                            (format nil "     {~{~a~^, ~}}"
                                    (remove nil
                                       (loop for i from 0 to (1- column-count)
                                             collect (when (nth i row)
                                                       (format nil "~a: ~a" (schema-name (nth i columns))
                                                               (unparse-attribute-value
                                                                (nth i columns)
                                                                (nth i row))))))))
                        (data-rows data))))
      (format stream
              (format nil "~%ActiveRecord::Base.connection.reset_pk_sequence!('~a')~%"
                      (schema-name ent))))))

(defun data-seeds (arg) arg "not implemented in soft_sim yet")
(defun unparse-data-set (data-set &optional (stream t))
  (dolist (data-seed (reverse (data-seeds data-set)))
    (create-seed-data (entity data-seed) stream data-seed)))

(defun create-test-data-file (data-set &optional stream)
  (let ((file (test-data-file-path (snake-case (name data-set))))
        (special-code (get-custom-code :data-file (id data-set))))
    (with-open-file (data-file file :direction :output :if-exists :supersede)
      (format-file-notice data-file "create-test-data-file")
      (terpri data-file)
      (unparse-data-set data-set (or stream data-file))
      (terpri (or stream data-file))
      (when special-code
        (format (or stream data-file) "~a~%" special-code)))))

;;; WIP involving unfinished soft_sim data-set.lisp code
 ;; (defun needs-dbtenant? (entity data)
 ;;  (and data
 ;;       (tenant-scoped-entity? entity)
 ;;       (not (find (id (tenant-key entity)) (columns data) :key #'id))))

(defmethod add_column ((attribute attribute) &optional stream)
  (let ((options (assemble-column-options attribute)))
    (format stream "add_column :~a, :~a, :~a~a"; (make-indent)
            (schema-name (my-entity attribute)) (schema-name attribute)
            (if (implement-as-string? attribute)
                "string"
                (ruby:unparse-datatype (logical-type attribute)))
            (if options
                (format nil ", ~{~a~^, ~}" options)
                ""))))

(defmethod remove_column ((attribute attribute) &optional stream)
  (format stream "remove_column :~a, :~a" (schema-name (my-entity attribute)) (schema-name attribute)))

(defun add-attributes-migration (name atts &optional (stream t))
  (format stream "~%
class ~a < ActiveRecord::Migration[6.1]
  def change~%~{    ~a~%~}  end~%end~%" name (mapcar #'add_column atts)))

(defmethod add-attribute-migration ((att symbol) &optional (entity t))
  (add-attribute-migration (find-field att entity) t))
(defmethod add-attribute-migration ((att attribute) &optional (stream t))
  (format stream "~%
class Add~aTo~a < ActiveRecord::Migration[6.1]
  def up~%    ~a
  end~%
  def down~%    ~a
  end~%end~%" (name att) (plural (my-entity att))
  (with-nesting (with-nesting (add_column att nil)))
  (with-nesting (with-nesting (remove_column att nil)))))

(defmethod add-entity-migration ((entity symbol) &optional (stream t))
  (let ((ent (find-entity entity)))
    (if ent
        (add-entity-migration ent stream)
        (warn "no entity with key ~a found" entity))))

(defmethod add-entity-migration ((entity entity) &optional (stream t))
  (let ((create (with-output-to-string (out)
                  (with-nesting (with-nesting (create_table entity out)))))
        (change (with-output-to-string (out)
                  (with-nesting (with-nesting (change_table entity out)))))
        (import (when (seed-data entity)
                  (with-output-to-string (out)
                    (with-nesting (with-nesting (create-seed-data entity out)))))))
    (format stream "~%
class Create~a < ActiveRecord::Migration[6.1]
  def up~%~a~%~a~%    ~a
  end
  def down
    drop_table :~a
  end
end~%" (plural entity) create change (or import "") (schema-name entity))))

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
