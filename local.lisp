
(in-package ror)

(setf ruby::*include-rails* t)
(setf *simple-table-layouts?* t)

(defun view-link (view &optional (base "172.105.172.46:7101"))
  (format nil "~%<a href=\"http://~a/~a/~a\">~a</a>~%<br>"
          base (snake-case (name view)) (schema-name (entity (root view)))
          (long-name view))) 

(defun write-view-links (&optional (base "172.105.172.46:7101"))
  (with-open-file (file (merge-pathnames
                         (make-pathname :name "all-views" :type "html")
                         (web-docs::documentation-css-filepath))
                        :direction :output :if-exists :supersede)
    (format file "<h1>All views below:</h1>")
    (dolist (view (views *application*))
      (format file (view-link view base)))))

(defun show-non-scoped-top-level-models()
  (remove-if #'tenant-scoped-entity? (remove-duplicates (mapcar #'entity (mapcar #'root (views *application*))))))

;(defun find-derived-show-items (aspect)
;  (let 

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
