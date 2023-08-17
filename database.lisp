;;;===========================================================================
;;; file:   database.lisp
;;; auth:   Coby Beck
;;; date:   2021-08-21
;;;
;;;---------------------------------------------------------------------------
;;;   code associated with creating db/structure.sql
;;;---------------------------------------------------------------------------  
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

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
