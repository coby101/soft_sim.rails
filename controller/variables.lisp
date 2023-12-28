;;;===========================================================================
;;;
;;;   Code for generating miscellaneous parts of controllers code
;;;
;;;===========================================================================

(in-package #:controller)

(defmethod build-method ((aspect aspect))
  (if (or (not (root? aspect))
          (tenant-scoped-entity? (entity aspect)))
      "build"
      "new"))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :edit)) &key (key-param "id") (recursive? t))
  (instance-variable-assignment aspect :show :recursive? recursive? :key-param key-param))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :delete)) &key (key-param "id") (recursive? t))
  (instance-variable-assignment aspect :show :recursive? recursive? :key-param key-param))

(defmethod set_model-method ((aspect aspect) &optional (stream t))
  (let ((entity (entity aspect)))
    (format stream (ruby:unparse-method
                    (format nil "set_~a" (instance-name entity)) nil
                    (as-literal
                     (instance-variable-assignment aspect :show))))))

(defun applicable-set_method-actions (aspect)
  (declare (ignorable aspect))
  ;; seems we should be selective about this list based on allowed operations
  (let ((methods (list :show :edit :update :destroy)))
    (unparse-array methods :ruby)))

(defun use-set_method? (aspect)
  (and (not (multi-valued-attributes (entity aspect)))
       (root? aspect)))

(defun set_model-filter (aspect)
  (when (use-set_method? aspect)
    (format nil "before_action :set_~a, only: ~a"
            (instance-name (entity aspect))
            (applicable-set_method-actions aspect))))

(defun write-action-filters (aspect)
  ;; door is open here for other filter actions
  (remove nil (list (set_model-filter aspect))))

(defmethod unparse-filter-application ((filter entity-state))
  (snake-case (name filter)))

(defmethod unparse-filter-application ((filter formula))
  (format nil "where(~s)" (unparse-expression (expression filter) :sql)))

(defmethod format-controller-filters ((aspect aspect))
  (let ((filters (filters aspect)))
    (when filters
      (loop for f in filters
         collect (unparse-filter-application f)))))

(defmethod mva-includes ((aspect aspect))
  (let ((mvas (multi-valued-attributes (entity aspect))))
    (if mvas
        (format nil ".includes(~{:~a~^, ~})"
                (mapcar #'(lambda(att) (schema-name (child-entity att))) mvas))
        "")))

(defmethod unparse-controller-model-reference ((aspect aspect))
  (let* ((entity (entity aspect))
         (closest-rel (get-closest-relative entity (view aspect)))
         (relationship (when closest-rel (find-relationship closest-rel entity))))
    (if (root? aspect)
        (if (tenant-scoped-entity? entity)
            (format nil "@~a.~a" (instance-name (path (tenant-key entity))) (schema-name entity))
            (model-name entity))
        (format nil "@~a.~a" (instance-name closest-rel) (schema-name (rhs relationship))))))

(defmethod set-parents ((aspect aspect))
  (if (root? aspect)
       ""
      (let* ((entity (entity aspect))
             (closest-rel (get-closest-relative entity (view aspect)))
             (rel (when closest-rel (find-relationship closest-rel entity))))
        (format nil "~a~%~a"
                (instance-variable-assignment
                 (find-aspect (view aspect) closest-rel) :show :key-param (strcat (instance-name (lhs rel)) "_id"))
                (make-indent)))))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :list)) &key (key-param "id") (recursive? t))
  (declare (ignorable key-param))
  (format nil "~a~a" (if recursive? (set-parents aspect) "")
          (format nil "@~a = ~a~{.~a~}"
                  (snake-case (plural (entity aspect)))
                  (unparse-controller-model-reference aspect)
                  (or (format-controller-filters aspect)
                      (if (root? aspect) (list "all"))))))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :show)) &key (key-param "id") (recursive? t))
  (let* ((entity (entity aspect)))
    (format nil "~a~a" (if recursive? (set-parents aspect) "")
            (format nil "~a = ~a~a.find(params[:~a])" (strcat "@" (instance-name entity))
                    (unparse-controller-model-reference aspect) (mva-includes aspect)
                    key-param))))

;    @company = Company.find(params[:company_id])
;    @division = @company.divisions.build
(defmethod instance-variable-assignment ((aspect aspect) (action (eql :new)) &key (key-param "id") (recursive? t))
  (declare (ignorable key-param))
  (let* ((entity (entity aspect)))
    (format nil "~a~a" (if recursive? (set-parents aspect) "")
            (format nil "~a = ~a.~a" (strcat "@" (instance-name entity))
                    (unparse-controller-model-reference aspect)
                    (build-method aspect)))))

;    @company = Company.find(params[:company_id])
;    @division = @company.divisions.build(division_params)
(defmethod instance-variable-assignment ((aspect aspect) (action (eql :create)) &key (key-param "id") (recursive? t))
  (declare (ignorable key-param))
  (let* ((entity (entity aspect)))
    (format nil "~a~a" (if recursive? (set-parents aspect) "")
            (format nil "@~a = ~a.~a(~a_params)" (instance-name entity)
                    (unparse-controller-model-reference aspect)
                    (build-method aspect) (instance-name entity)))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
