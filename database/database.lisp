;;;===========================================================================
;;;
;;;   code for generating seed data, migrations and schema definitions
;;;
;;;===========================================================================

(in-package :database)

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


'(defun write-structure.sql ()
  (let ((file (db-structure-file-path)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (create-schema app (db-platform *implementation*) out))))

'(defmethod create-schema ((app application) (db database-platform)
                              &optional (stream t))
  (let ((line-break (if *pretty-print* (format nil "~%~%") " ")))
    (dolist (e (sort (copy-list (schema app)) #'string-lessp :key #'name))
      (format stream "~a" line-break)
      (create-table e db stream))
    (format stream "~a" line-break)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
