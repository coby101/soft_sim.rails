;;;===========================================================================
;;;
;;;   code for generating seed data
;;;
;;;===========================================================================

(in-package #:database)

(defun seed-data-order (entities)
  (let* ((app-ent (application-entity))
         (tenant   (tenant-entity))
         (cal (calendar-entities))
         (non-cal (set-difference entities cal))
         (weak (remove-if-not #'(lambda (e) (typep e 'weak-entity)) non-cal))
         (regular (remove app-ent (remove-if #'(lambda (e) (typep e 'weak-entity)) non-cal))))
    (append (when app-ent (list app-ent))
            (when tenant (list tenant))
            cal
            (sort regular #'string-lessp :key #'name)
            (sort weak #'string-lessp :key #'name))))

;;; WIP in soft_sim data-set.lisp COLUMNS DATA-ROWS MAKE-DATA-SEED
(defun columns (seed-data ent) (mapcar #'(lambda (col) (find-field col ent)) (car seed-data)))
(defun data-rows (seed-data) (cdr seed-data))
(defun make-data-seed (arg1 arg2) (values arg2 arg1))
(defun create-seed-method () "db_import!")

(defun seed-data-import-statement (ent stream &optional data-seed)
  (let ((data (or data-seed (make-data-seed ent (ignore-errors (seed-data ent)))))
        (fmt-str (format nil "~~a.~~a([~~%~~{~a~~a~~^,~~%~~}~~%])" (with-nesting (make-indent)))))
    (when data
      (format stream fmt-str (model-name ent) (create-seed-method)
              (let* ((columns (columns data ent))
                     (column-count (length columns)))
                (mapcar #'(lambda (line) (ruby:unparse-hash line :one-line t))
                        (mapcar #'(lambda (row) (mapcar #'list (mapcar #'schema-name columns) row)) (data-rows data)))))
      (format stream
              (format nil "~%~aActiveRecord::Base.connection.reset_pk_sequence!('~a')~%"
                      (make-indent) (schema-name ent))))))

(defun data-seeds (arg) arg "not implemented in soft_sim yet")
(defun unparse-data-set (data-set &optional (stream t))
  (dolist (data-seed (reverse (data-seeds data-set)))
    (seed-data-import-statement (entity data-seed) stream data-seed)))

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


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
