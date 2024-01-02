;;;===========================================================================
;;;
;;;   code for generating migrations
;;;
;;;===========================================================================

(in-package #:database)

(defmethod add_column ((attribute attribute) &optional stream)
  (let ((options (assemble-column-options attribute)))
    (format stream "add_column :~a, :~a, :~a~a"; (make-indent)
            (schema-name (my-entity attribute)) (schema-name attribute)
            (if (implement-as-string? attribute)
                "string"
                (unparse-datatype (logical-type attribute)))
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
  (format stream "
class Add~aTo~a < ActiveRecord::Migration[~a]
  def up~%    ~a
  end~%
  def down~%    ~a
  end~%end~%" (name att) (plural (my-entity att))
  (let ((platform (gui-platform *implementation*))) (format nil "~a.~a" (major-version platform) (minor-version platform)))
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
                    (with-nesting (with-nesting (seed-data-import-statement entity out)))))))
    (format stream "
class Create~a < ActiveRecord::Migration[~a]
  def up~%~a~%~a~%    ~a
  end
  def down
    drop_table :~a
  end
end~%" (plural entity) (let ((platform (gui-platform *implementation*))) (format nil "~a.~a" (major-version platform) (minor-version platform)))
       create change (or import "") (schema-name entity))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
