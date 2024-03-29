;;;===========================================================================
;;;
;;;   Code for generating "core" partials. The code produced here depends on
;;;   PCMD specific partials that have not yet been incorporated into this generator
;;;   (see app/views/application/core/*.html.erb partials)
;;;
;;;===========================================================================

(in-package #:view)

'(defun generate-view (view)
  (dolist (aspect (aspects (find-view view)))
    (generate-partials aspect)))

(defun core-action-links (aspect page)
  (substitute :view :detail (action-links aspect page)))

(defun space-open (aspect)
  (declare (ignorable aspect))
  (if t
      ""
      "
<% if @inside_a_space %>
  <% render @nav_side_menu %>
  <div class=\"px-4 py-5 sm:p-6\">
<% end %>
"))

(defun space-close (aspect)
  (declare (ignorable aspect))
  (if t
      ""
      "
<%= '</div>'.html_safe if @inside_a_space %>
"))

(defun render-context (aspect)
  (if (root? aspect)
      ""
      (format nil "
<%= render partial: '~a/context' %>" (schema-name (entity aspect)))))

(defmethod core_show.html.erb ((aspect symbol) &optional stream)
  (core_show.html.erb (find-aspect aspect stream) t))
(defmethod core_show.html.erb ((aspect aspect) &optional stream)
 (let ((file (layout-file-path aspect "show"))
       (actions (core-action-links aspect :detail)))
   (with-open-file (html.erb file :direction :output :if-exists :supersede)
     (when stream (format stream "~%# app/views/~a/~a.html.erb~%" (schema-name (entity aspect)) "show"))
     (format (or stream html.erb) "~a~a
<%= render partial: 'application/core/show', locals: {
      object:     @~a,
      attributes: [~{~a~^,~%                   ~}],
      actions:    ~a~a
      } %>
~a" (space-open aspect)
    (render-context aspect)
    (instance-name (entity aspect))
    (loop for row in (panel-items (detail-panel aspect))
	          collect (format nil "[~{~a~^, ~}]"
				  (loop for item in row collect (unparse-core-element item aspect))))
    (unparse-array actions :ruby)
    (if (root? aspect) "" (format nil ",~%      ancestors:  @ancestors"))
    (space-close aspect)))))

(defmethod core_index.html.erb ((aspect symbol) &optional stream)
  (core_index.html.erb (find-aspect aspect stream) t))
(defmethod core_index.html.erb ((aspect aspect) &optional stream)
 (let ((file (layout-file-path aspect "index"))
       (actions (core-action-links aspect :list)))
   (with-open-file (html.erb file :direction :output :if-exists :supersede)
     (when stream (format stream "~%# app/views/~a/~a.html.erb~%" (schema-name (entity aspect)) "index"))
     (format (or stream html.erb) "~a~a
<%= render partial: 'application/core/index', locals: {
      searchable: true,
      collection: @~a, 
      attributes: @index_attributes,
      actions:    ~a~a
      } %>
~a" (space-open aspect)
    (render-context aspect)
    (schema-name (entity aspect))
    (unparse-array actions :ruby)
    (if (root? aspect) "" (format nil ",~%      ancestors:  @ancestors"))
    (space-close aspect)))))

(defmethod core_context.html.erb ((aspect symbol) &optional stream)
  (core_context.html.erb (find-aspect aspect stream) t))
(defmethod core_context.html.erb ((aspect aspect) &optional stream)
  (unless (root? aspect)
    (let ((file (layout-file-path aspect "_context")))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
	(when stream (format stream "~%# app/views/~a/~a.html.erb~%" (schema-name (entity aspect)) "context"))
	(format (or stream html.erb) "<%= render partial: '~a_space/context' %>~%" (snake-case (name (view aspect))))
	'(dolist (ancestor (list-possible-ancestors aspect))
	  (let* ((parent-aspect (find-aspect (view aspect) ancestor))
		 (panel (context-panel parent-aspect))
		 (actions (core-action-links parent-aspect :context))
		 (context (and panel (panel-items panel))))
	    (format (or stream html.erb) "
<% if @ancestors&.index(@~a) %>
  <%= render partial: 'application/core/context', locals: {
        object:     @~:*~a,
        attributes: [~{~a~^,~%                   ~}],
        actions:    ~a,
        ancestors:  @ancestors[0...@ancestors.index(@~a)]
        } %>
<% end %>
" (instance-name ancestor)
  (loop for row in context
	collect (format nil "[~{~a~^, ~}]"
			(loop for item in row collect (unparse-core-element item parent-aspect :downlink? :counts_only))))
  (unparse-array actions :ruby)
  (instance-name ancestor)))))))
  nil)

(defmethod core_edit.html.erb ((aspect symbol) &optional stream)
  (core_edit.html.erb (find-aspect aspect stream) t))
(defmethod core_edit.html.erb (aspect &optional stream)
  (let ((file (layout-file-path aspect "edit")))
    (unless (typep (entity aspect) 'attribute-table)
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
	(when stream (format stream "~%# app/views/~a/~a.html.erb~%" (schema-name (entity aspect)) "edit"))
	(format (or stream html.erb) "<%= render 'form', ~a: @~:*~a %>~%" (instance-name (entity aspect)))))))

(defmethod core_new.html.erb ((aspect symbol) &optional stream)
  (core_new.html.erb (find-aspect aspect stream) t))
(defmethod core_new.html.erb (aspect &optional stream)
  (let ((file (layout-file-path aspect "new")))
    (unless (typep (entity aspect) 'attribute-table)
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
	(when stream (format stream "~%# app/views/~a/~a.html.erb~%" (schema-name (entity aspect)) "new"))
	(format (or stream html.erb) "<%= render 'form', ~a: @~:*~a %>~%" (instance-name (entity aspect)))))))


(defmethod core_form.html.erb ((aspect symbol) &optional stream)
  (core_form.html.erb (find-aspect aspect stream) t))
;; there is a lot of duplicate code in view.lisp:form.html.erb
(defmethod core_form.html.erb (aspect &optional stream)
  (unless (typep (entity aspect) 'attribute-table)
    (let ((*simple-table-layouts?* nil)
          (file (layout-file-path aspect "_form")))
;;;          (before-elements (get-additional-page-elements aspect :form :before))
;;;          (after-elements (get-additional-page-elements aspect :form :after)))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
	(when stream (format stream "~%# app/views/~a/~a.html.erb~%" (schema-name (entity aspect)) "_form"))
	(format (or stream html.erb) "~a~a
<%= render partial: 'application/core/form', locals: {
      object:     @~a,
      attributes: [~{~a~^,~%                   ~}]~a
      } %>
~a" (space-open aspect)
    (render-context aspect)
    (instance-name (entity aspect))
    (loop for row in (panel-items (edit-panel aspect))
	  collect (format nil "[~{~a~^, ~}]"
			  (loop for item in row collect (unparse-core-element item aspect))))
    (if (root? aspect) "" (format nil ",~%      ancestors:  @ancestors"))
    (space-close aspect))))))

(defmethod generate-partials ((aspect symbol) &optional stream)
  (generate-partials (find-aspect aspect stream) t))
(defmethod generate-partials (aspect &optional stream)
  (core_context.html.erb aspect stream)
  (when (listable? aspect)
    (core_index.html.erb aspect stream))
  (when (showable? aspect)
    (core_show.html.erb aspect stream))
  (when (updatable? aspect)
    (core_edit.html.erb aspect stream))
  (when (creatable? aspect)
    (core_new.html.erb aspect stream))
  (when (updatable? aspect)
    (core_form.html.erb aspect stream)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
