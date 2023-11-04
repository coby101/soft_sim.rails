;;;===========================================================================
;;;
;;;   code largely specific to PCMD Software Solutions application. Eventually this
;;;   will belong to that project's repo and there will be a soft_sim mechanism
;;;   for importing project specific generator code. 
;;;
;;;===========================================================================

(in-package :ror)

(defun write-controller-files (view)
  (dolist (aspect (aspects view))
    (let ((file (controller-file-path aspect)))
      (with-open-file (cont-file file :direction :output :if-exists :supersede)
        (format-file-notice cont-file "core-controller")
        (core-controller-class-definition aspect cont-file)))))

(defun controller-file-path (aspect)
  (merge-pathnames
   (make-pathname
    :name (snake-case (strcat (plural (entity aspect)) "_controller"))
    :type "rb")
   (controller-directory)))

(defmethod core-controller-class-definition ((aspect symbol) &optional stream)
  (core-controller-class-definition (find-aspect aspect stream) t))
(defmethod core-controller-class-definition ((aspect aspect) &optional (stream t))
  (when (eq stream t) (format stream "~%# app/controllers/~a_controller.rb~%" (snake-case (plural (entity aspect)))))
  (let ((actions (implemented-actions aspect)))
    (format stream "~a~%" (controller-class-declaration (format nil "~aController" (camel-case (plural (entity aspect))))))
    (with-nesting
      (format stream "~a~%~%  public~%~%" (before-actions aspect))          
      (when (member :list actions)
        (index-method aspect stream) (terpri stream)
        (list-method aspect stream) (terpri stream))
      (when (member :detail actions)
        (show-method aspect stream) (terpri stream))
      (when (member :new actions)
        (new-method aspect stream) (terpri stream))
      (when (member :edit actions)
        (edit-method aspect stream) (terpri stream))
      (when (member :create actions)
        (create-method aspect stream) (terpri stream))
      (when (member :update actions)
        (update-method aspect stream) (terpri stream))
      (when (member :delete actions)
        (delete-method aspect stream) (terpri stream))
      (format-private-controller-methods aspect stream))
    (format stream "~%end~%")))

(defmethod format-private-controller-methods ((aspect aspect) &optional (stream t))
  (format stream "~%~aprivate~%~%" (make-indent))
  (unless (root? aspect)
    (format stream "~a~a~%" (make-indent) (set_ancestors aspect nil)))
  (when (showable? aspect)
    (format stream "~a~a~%" (make-indent) (set_model-method aspect nil)))
  (when (listable? aspect)
    (format stream "~a~a~%" (make-indent) (set_index_attributes aspect nil))
    (format stream "~a~a~%" (make-indent) (set_models-method aspect nil)))
  (format stream "~a~a~%" (make-indent) (get_route_focus aspect nil))
  (when (updatable? aspect)
    (format stream "~a~a" (make-indent) (model_params-method aspect nil))))

(defmethod get_route_focus ((aspect aspect) &optional (stream t))
  (format stream "~a" (ruby:unparse-method "get_route_focus" () (as-literal "@current_focus = 'cash_flow'"))))

(defmethod index-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a"
          (make-indent)
          (ruby:unparse-method
           "index" ()
           (as-literal (format nil "@~a = ~a" (schema-name (entity aspect)) (sort-and-paginate-assignment aspect))))))

(defmethod list-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a"
          (make-indent)
          (ruby:unparse-method
           "list" ()
           (as-literal (format nil "@~a = ~a" (schema-name (entity aspect)) (apply-search_assignment aspect)))
           (as-literal (format nil "@~a = ~a" (schema-name (entity aspect)) (sort-and-paginate-assignment aspect :with-params? t)))
           (as-literal "render :index"))))

(defmethod delete-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (ruby:unparse-method
           "destroy" nil
           (if (typep (entity aspect) 'attribute-table)
               (as-literal (format nil "@~a.destroy~%    respond_to do |format|~%      format.js"
                                   (instance-name (entity aspect))))
               (as-literal (respond_to aspect :delete)))
           (as-literal "end"))))

(defmethod update-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (ruby:unparse-method
           "update" nil
           (as-literal (respond_to aspect :update))
           (as-literal "end"))))

(defmethod set_index_attributes ((aspect aspect) &optional (stream t))
  (format stream "~a"
          (ruby:unparse-method
           "set_index_attributes" ()
           (as-literal
            (format nil "@index_attributes = [~{~a~^, ~}]"
                    (loop for item in (panel-items (list-panel aspect))
                           collect (unparse-core-element item aspect)))))))

(defun before-actions (aspect)
  (let ((route-focus (format nil "~abefore_action :get_route_focus~%" (make-indent)))
        (set-ancestors (format nil "~abefore_action :~a~%" (make-indent) (set_ancestors-method-name aspect)))
        (set-object (format nil "~abefore_action :~a, only: [:show, :edit, :update, :destroy]~%" (make-indent) (set_model-method-name aspect)))
        (set-collection (format nil "~abefore_action :~a, only: [:list, :index]" (make-indent) (set_models-method-name aspect))))
    (format nil "~a~a~a~a" route-focus (if (root? aspect) "" set-ancestors) set-object set-collection)))

(defun apply-search_assignment (aspect)
  (format nil "apply_search_proc(@~a, params[:q]) if params[:q].present?" (schema-name (entity aspect))))

(defun sort-and-paginate-assignment (aspect &key with-params?)
  (let* ((col1 (car (panel-items (list-panel aspect))))
         (args (if with-params? "params[:column], params[:direction], params[:page]" (format nil "~(:~a~)"  (if (typep col1 'entity) "id" (schema-name col1))))))
    (format nil "sorted_and_paginated(@~a, ~a)" (schema-name (entity aspect)) args)))

(defun set_ancestors-method-name (aspect)
  (declare (ignorable aspect))
  "set_ancestors")

(defun set_model-method-name (aspect) (format nil "set_~a" (instance-name (entity aspect))))
(defun set_models-method-name (aspect) (format nil "set_~a" (schema-name (entity aspect))))

(defun list-possible-ancestors (aspect)
  (let* ((paths (mapcar #'reverse (all-paths-to-root (entity aspect) (view aspect))))
         (max-length (apply #'max (mapcar #'length paths))))
    (remove nil
            (remove-duplicates
             (loop for i from 0 to (1- max-length)
                   nconcing (mapcar #'(lambda(path) (nth i path)) paths))))))

(defun format-ancestor-assignment(entity view)
  (let* ((view-parents (intersection (parents entity) (mapcar #'entity (aspects view))))
         (parent-count (length view-parents))
         (scope-code (cond
                       ((and (null view-parents) (tenant-scoped-entity? entity)) "@tenant.")
                       ((null view-parents) "")
                       ((= parent-count 1) (format nil "@~a&." (instance-name (car view-parents))))
                       (t (format nil "(~{@~a~^ || ~})." (mapcar #'instance-name view-parents))))))
    (as-literal (format nil "@~a = ~a~a&.find(params[:~a_id]) if params[:~:*~a_id]"
                        (instance-name entity) scope-code (schema-name entity) (instance-name entity)))))

(defmethod set_ancestors ((aspect aspect) &optional (stream t))
  (let ((ancestors (list-possible-ancestors aspect)))
    (format stream "~a"
            (apply #'ruby:unparse-method "set_ancestors" ()
                   (append
                      (loop for ancestor in ancestors
                            collect (format-ancestor-assignment ancestor (view aspect)))
                    (list (as-literal (format nil "@ancestors = [~{@~a~^, ~}].compact" (mapcar #'instance-name ancestors)))))))))

(defmethod set_model-method ((aspect aspect) &optional (stream t))
  (format stream (ruby:unparse-method (set_model-method-name aspect) nil
                  (as-literal
                   (with-nesting (main-variable-assignment aspect :show))))))

(defmethod set_models-method ((aspect aspect) &optional (stream t))
  (format stream (ruby:unparse-method (set_models-method-name aspect) nil
                  (as-literal
                   (with-nesting (main-variable-assignment aspect :list))))))

(defun main-variable-assignment(aspect action)
  (let* ((entity (entity aspect))
         (collection (cond
                       ((and (root? aspect) (tenant-scoped-entity? entity)) (format nil "@tenant.~a" (schema-name entity)))
                       ((root? aspect) (format nil "~a" (model-name entity)))
                       (t (format nil "@ancestors.last.~a" (schema-name entity)))))
         (find (if (eql action :show) ".find(params[:id])" "")))
    (format nil "@~a = ~a~a" (if (eql action :show) (instance-name entity) (schema-name entity))
            collection find)))

(defmethod show-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent) (ruby:unparse-method "show" nil)))

(defmethod edit-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent) (ruby:unparse-method "edit" nil)))



;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
