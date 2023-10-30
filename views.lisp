;;;===========================================================================
;;; file:   generators/ror/views.lisp
;;; auth:   Coby Beck
;;; date:   2020-12-04
;;; update: 
;;;---------------------------------------------------------------------------
;;;   Code for generating general and view specific layouts, and
;;;   all aspects of the view component of Rails MVC framework 
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)
(defvar *use-shared-views* t)
(defun generate-views (&optional (app *application*))
  (default-layouts)
  (framework-layouts)
  ;; (when *dev-mode*
  ;;   (dolist (view (mapcar #'create-default-view (schema-entities app)))
  ;;     (write-view-files view)))
  (dolist (view (views app))
    (if *use-shared-views*
        (generate-view view)
        (write-view-files view))))

(defun write-view-files (view)
  (dolist (aspect (aspects view))
    (when (listable? aspect)   (html.erb aspect :index))
    (when (showable? aspect)   (html.erb aspect :show))
    (when (updatable? aspect)
      (unless (typep (entity aspect) 'attribute-table)
        (html.erb aspect :edit)))
    (when (creatable? aspect)
      (unless (typep (entity aspect) 'attribute-table)
        (html.erb aspect :new)))
    ;(when (member :search ops) ??)
    (let ((*simple-table-layouts?* nil))
      (when (updatable? aspect)
        (form.html.erb aspect)))))

(defun html.erb (aspect layout)
  (let ((method (html.erb-method layout))
        (before-elements (get-additional-page-elements aspect layout :before))
        (after-elements (get-additional-page-elements aspect layout :after)))
    (let* ((layout-name (format nil "~(~a~)" layout))
           (file (layout-file-path aspect layout-name)))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
        (format html.erb (unparse-erb nil (format-file-notice nil (strcat layout-name ".html.erb"))))
        (funcall method aspect html.erb :before before-elements :after after-elements)))))

(defun html.erb-method (page)
  (ecase page
    (:index #'write-list-layout)
    (:show #'write-detail-layout)
    (:edit #'write-edit-layout)
    (:new #'write-create-layout)))

(defmethod get-additional-page-elements ((aspect aspect) (page t) (location t))
  (declare (ignorable aspect page location))
  nil)

(defmethod get-additional-page-elements ((aspect aspect) (page (eql :form)) (location (eql :after)))
  (when (includes-full-address? aspect)
    (list (google-address-js aspect))))

(defmethod includes-full-address? ((aspect aspect))
  (or (includes-full-address? (flatten (and (edit-panel aspect) (panel-items (edit-panel aspect)))))
      (includes-full-address? (flatten (and (add-panel aspect) (panel-items (add-panel aspect)))))))

(defmethod includes-full-address? ((atts list))
  (= 4 (length (intersection '(:adress1 :locality :state :postcode :country)
                             (mapcar #'id atts)))))

;; there is a lot of duplicate code in shared-views.lisp::_form.html.erb
(defun form.html.erb (aspect)
  (unless (typep (entity aspect) 'attribute-table)
    (let ((file (layout-file-path aspect "_form"))
          (before-elements (get-additional-page-elements aspect :form :before))
          (after-elements (get-additional-page-elements aspect :form :after)))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
        (format html.erb (unparse-erb nil (format-file-notice nil (file-namestring file))))
        (write-update-form aspect html.erb :before before-elements :after after-elements)))
    (let ((nested-attributes
            (remove-if-not #'(lambda(att) (typep att 'multi-valued-attribute))
                           (flatten (append (when (updatable? aspect)
                                              (panel-items (edit-panel aspect)))
                                            (when (creatable? aspect)
                                              (panel-items (add-panel aspect))))))))
      (when nested-attributes
        (dolist (att nested-attributes)
          (let* ((child-entity (child-entity att))
                 (child-aspect (find-aspect (view aspect) child-entity))
                 (partial-path (layout-file-path aspect (format nil "_~a_fields" (instance-name child-entity))))
                 (destroy-path (merge-pathnames
                                (make-pathname :name "destroy.js" :type "erb")
                                (layout-directory child-aspect))))
            (with-open-file (html.erb partial-path :direction :output :if-exists :supersede)
              (format html.erb (unparse-erb nil (format-file-notice nil (file-namestring partial-path))))
              (write-form-partial child-aspect html.erb))
            (with-open-file (js.erb destroy-path :direction :output :if-exists :supersede)
              (format js.erb (destroy.js.erb child-aspect)))))))))

(defun destroy.js.erb (aspect &optional stream)
  (format stream "~&~a~%let ~a = document.querySelector(\"#~:*~a\");
~:*~a.classList.add(\"fadeout\");
setTimeout(function(){
    ~:*~a.remove();
}, 1000)~%" (js:comment-out nil "this file was generated by function destroy.js.erb")
          (unparse-form-object-id aspect (strcat "@" (instance-name (entity aspect))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  List layout for index.html.erb
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod write-list-layout ((aspect aspect) stream &key before after)
  (declare (ignorable before after))
  (format stream "~%~a~%" (ui-label aspect :list))
  (let ((panel (list-panel aspect))
        (actions (make-action-links aspect :list :icons? t :var (instance-name (entity aspect)))))
    (write-view-context aspect stream)
    (format stream "~a"
            (format-data-listing
             (list-column-headings panel (length actions))
             (list-body aspect actions)
             (strcat (name (view aspect)) "-" (name (entity aspect)))))
    (format stream "~%<br>~%~%~a~%<br>~%"
            (if (creatable? aspect)
                (make-action-link aspect :new :detail :footer? t)
                ""))))

(defmethod list-column-headings ((panel view-panel) action-count &optional id)
  (declare (ignorable id))
  (error "list layouts need a flat-panel specification"))
(defmethod list-column-headings ((panel flat-listing) action-count &optional id)
  (let ((fmt-str (format nil "~a~%~~{~a~~a~%~~}" (html:make-indent)
                         (with-nesting (html:make-indent)))))
    (format nil fmt-str
            (append
             (when (> action-count 0)
               (list (data-list-heading "(actions)" :colspan action-count)))
             (mapcar #'(lambda(item)
                         (let ((colspan (when (typep item 'view-panel)
                                          (length (panel-items item)))))
                           (data-list-heading
                            (unparse-erb t (t.name item)) :colspan colspan :id id)))
                     (panel-items panel))))))

(defmethod list-column-headings ((entity attribute-table) action-count &optional id)
  (declare (ignorable id))
  (let ((fmt-str (format nil "~a~%~~{~a~~a~%~~}" (html:make-indent)
                         (with-nesting (html:make-indent)))))
    (format nil fmt-str
            (append
             (when (> action-count 0)
               (list (data-list-element "(actions)" :colspan action-count)))
             (mapcar #'(lambda(att)
                         (data-list-element (t.name att)))
                     (user-attributes entity))))))

(defmethod list-body ((aspect aspect) actions)
  (with-output-to-string (str)
    (let* ((entity (entity aspect))
           (var (instance-name entity))
           (items (mapcar #'(lambda (item)
                              (unparse-template-element item aspect :obj-var var))
                          (panel-items (list-panel aspect))))
           (obj-ref (if (and (typep entity 'attribute-table)
                             (not (slot-boundp aspect 'view)))
                        (strcat (instance-name (owner entity)) "." (schema-name entity))
                        (schema-name entity))))
      (format str "~%~a~a~%" (html:make-indent)
              (unparse-erb nil (format nil "@~a.each do |~a|" obj-ref var)))
      (format str "~{~a~}~%~a~a"
              (data-list-rows (list (append actions items)))
              (html:make-indent) (unparse-erb nil "end")))))

'(defmethod list-body ((entity attribute-table) actions)
  (with-output-to-string (str)
    (let* ((var (instance-name entity))
           (items (mapcar #'(lambda (item)
                              (unparse-template-element item aspect :obj-var var))
                          (user-attributes entity))))
      (format str "~%~a~a~%" (html:make-indent)
              (unparse-erb nil (format nil "@~a.each do |~a|"
                                       (strcat (instance-name (owner entity)) "." (schema-name entity))
                                       var)))
      (format str "~{~a~}~%~a~a"
              (data-list-rows (list (append actions items)))
              (html:make-indent) (unparse-erb nil "end")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Detail layout for show.html.erb
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-datasheet -> parent-context 
(defmethod write-detail-layout ((aspect aspect) stream &key before after)
  (declare (ignorable before after))
  (format stream "~%~a~%" (ui-label aspect :detail))
  (let* ((entity (entity aspect))
         (panel (or (panel-items (detail-panel aspect)) (list (panel-items (list-panel aspect)))))
;         (context (get-page-context entity (view aspect)))
         (model-var (strcat "@" (instance-name entity))))
    (write-view-context aspect stream)
    (format stream "<br>~%~{~a~%~}<br>" (make-action-links aspect :detail :icons? t :var model-var))
    (format stream "~%~a"
            (format-datasheet nil ;; format-datasheet should not handle the context
;             (write-view-context aspect nil)
             (unparse-detail-rows (or panel '(("There are no fields to show"))) aspect model-var)))
    (format stream "<br>~%~{~a~%~}<br>" (make-action-links aspect :detail :icons? t :footer? t))))

(defun unparse-detail-rows (panel aspect &optional var)
  (loop for row in panel
        collect
        (mapcar #'(lambda(item)
                    (if (typep item 'multi-valued-attribute)
                        (unparse-template-element item aspect :obj-var var :labeled? t)
                        (unparse-template-element item aspect :obj-var var :labeled? t)))
                row)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  write form layout for use in new.html.erb and edit.html.erb
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod write-update-form ((aspect aspect) stream &key before after)
  (declare (ignorable after))
  (let* ((entity (entity aspect))
         (model-var (instance-name entity))
         (submit-button
           (html:div (unparse-erb t (format nil "form.submit class: ~s" *form-submit-button-class*)))))
    (when before (format stream "~{~a~%~}" before))
    (format stream "~%~a~%~a~a~%~a"
            (unparse-erb t (format nil "~a do |form|" (form_with aspect model-var)))
            (make-indent)
            (unparse-erb nil (format nil "if ~a.errors.any?" model-var))
            ;(with-nesting (format-error-trap model-var))
            (unparse-erb nil "end"))
    (format stream "~%~a~a~%  ~a~%~a~%~a~%" (make-indent)
            submit-button
            (format-update-form (or (panel-items (edit-panel aspect)) '(("There are no fields to show"))))
            submit-button
            (unparse-erb nil "end"))
    (when after (format stream "~{~a~%~}" after))))

(defmethod nested-fields? ((aspect aspect))
  (typep (entity aspect) 'attribute-table))

(defmethod write-form-partial ((aspect aspect) &optional (stream t))
  (unless (nested-fields? aspect)
    (error "calling write-form-partial on a non-nested field aspect"))
  (let ((id (unparse-form-object-id aspect)))
    (format stream "~%~a~%~a~a~%~a~%~a"
            (html:open-tag "div" :class "nested-fields" :id id) (html:make-indent)
            (unparse-erb t "form.hidden_field :_destroy")
            (format-update-form (list (append (mapcar #'unparse-form-element (panel-items (list-panel aspect)))
                                              (list (unparse-erb t (nested-fields-remove-link aspect))))))
;            (html:tag "div" )
;            (html:tag "div" (unparse-erb t "link_to \"Remove\", '#', class: \"remove_fields btn\""))
            (html:close-tag "div"))))

(defun nested-fields-remove-link (aspect)
  (unless (nested-fields? aspect)
    (error "calling write-form-partial on a non-nested field aspect"))
  (format nil "link_to \"Remove\", \"~a~a/~a/#{form.object.id}\", data: ~
               { method: :delete, remote: true, confirm: ('~a' unless form.object.id.nil?) }, class: ~s"
          (unparse-namespace aspect :route)
          (unparse-url-heirarchy (entity aspect) (view aspect) :obj-ref :form-object :include-self? nil)
          (schema-name (entity aspect))
          (escape-characters *delete-conf* #\')
          *delete-nested-attribute-class*))
'(defun nested-fields-remove-link (aspect)
  (unless (nested-fields? aspect)
    (error "calling write-form-partial on a non-nested field aspect"))
  (format nil "remove_child_button \"Remove\""))

(defun format-error-trap (model-var)
  (html:div
   (list (html:heading 2
           (unparse-erb t
              (strcat (format nil "pluralize(~a.errors.count, ~a)" model-var (t.error))
                      " + ' ' + " (t.prevented-saving) " + ':'")))
         (html:ltag nil
            (format nil "~%       ~a~%~a~%       ~a"
                    (unparse-erb nil (format nil "~a.errors.each do |error|" model-var))
                    (let ((*nesting-level* (+ 3 *nesting-level*)))
                      (html:tag "li" (unparse-erb t "error.full_message")))
                    (unparse-erb nil "end"))
            :style *error-list-style*))
   :id "error_explanation" :style *user-error-feedback-style*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  render forms for new.html.erb and edit.html.erb
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod write-create-layout ((aspect aspect) stream &key before after)
  (declare (ignorable before after))
  (let ((entity (entity aspect)))
    (terpri stream)
    (format stream "~a"
          (unparse-erb t
              (format nil "model_error_messages(@~a, ~a)"
                               (instance-name entity) (t.long-name entity))))
    (format stream "~%~a~%" (ui-label aspect :new))
    (format stream "~a~%~a~%~{~a~%~}~%<br><br>~%~a~%~%<br>~%~{~a~%~}~%"
            (if *use-shared-views* ""
                (write-view-context aspect nil))
            "<br>"
            (make-action-links aspect :new)
            (unparse-erb t (format nil "render 'form', ~a: @~:*~a"
                                   (instance-name (entity aspect))))
            (make-action-links aspect :new :footer? t))))

(defmethod write-view-context ((aspect aspect) &optional (stream t))
  (let* ((entity (entity aspect))
         (context-panels (get-page-context entity (view aspect))))
    (if context-panels
      (format stream "~%~a~%"
              (format-data-context
               (loop for panel in context-panels
                     collect (unparse-context-panel panel))))
      "")))

(defmethod unparse-context-panel ((panel view-panel))
  (let* ((aspect (container panel))
         (entity (entity aspect))
         (var (strcat "@" (instance-name entity)))
         (actions (make-action-links aspect :context :var var :icons? t))
         (panel-layout (panel-items panel))
         (panel-width (apply #'max (mapcar #'length panel-layout)))
         (first-item? t))
    (loop for row in panel-layout
          collect
          (format nil "  ~{~a~%  ~}"
                  (let ((row-length (length row)))
                    (mapcar #'(lambda(item)
                                (if (typep item 'multi-valued-attribute)
                                    (unparse-template-element item aspect :obj-var var :labeled? t)
                                    (datasheet-item (unparse-template-element item aspect :obj-var var :labeled? t)
                                                    :colspan (if first-item? 1
                                                                 (calculate-item-width item row-length panel-width))
                                                    :rowspan (when first-item?
                                                               (setf first-item? nil)
                                                               (length panel-layout)))))
                            (if first-item?
                                (cons (format nil "~{~a~^<br>~}" actions) row)
                                row)))))))

(defmethod write-edit-layout ((aspect aspect) stream &key before after)
  (declare (ignorable before after))
  (let* ((entity (entity aspect))
         (model-var (strcat "@" (instance-name entity))))
    (format stream "~%~a"
          (unparse-erb t
              (format nil "model_error_messages(@~a, ~a)"
                               (instance-name entity) (t.long-name entity))))
    (format stream "~%~a~%~a~%~%~{~a~%~}~%<br>~%~a~%<br>~{~a~%~}"
            (if *use-shared-views* "" (write-view-context aspect nil))
            (html:heading 1 (unparse-erb t "t('.title')"))
            (make-action-links aspect :edit :var model-var)
            (unparse-erb t (format nil "render 'form', ~a: ~a" (instance-name entity) model-var))
            (make-action-links aspect :edit :var model-var :footer? t))))

;;; https://api.rubyonrails.org/classes/ActionView/Helpers/FormHelper.html#method-i-form_with
;;;form_with(model: [:dev, @cactus])
;;;form_with(model: [:mng_comp, @company, division]
(defun form_with(aspect model-var)
  (let ((other-args (form_with-args aspect)))
    (format nil "form_with(model: ~a~a)" (form-path aspect model-var) (or other-args ""))))

(defun form-path (aspect model-var)
  (format nil "[~a~a~a]"
            (unparse-namespace aspect :array)
            (if (root? aspect) ""
                (format nil "~{@~a, ~}" (mapcar #'instance-name (reverse (path-to-root aspect (view aspect))))))
            model-var))

(defun form_with-args (aspect)
  (when (includes-full-address? aspect)
    ", data: { controller: \"places\" }"))

(defun path-method-call (aspect action &optional var)
  (let ((method-name (path-method-name aspect action t))
        (controller-path (reverse (path-to-root (entity aspect) (view aspect)))))
    (ecase action
      ((:new :list) (strcat method-name
                            (if (root? aspect) ""
                                (format nil "(~{~a~^, ~})"
                                        (append (mapcar #'(lambda(ent)
                                                            (strcat "@" (instance-name ent)))
                                                        (butlast controller-path))
                                                (list (or var (strcat "@" (instance-name (car (last controller-path)))))))))))
      ((:edit :detail) (format nil "~a(~{~a~^, ~})" method-name
                               (append (mapcar #'(lambda (ent)
                                                   (strcat "@" (instance-name ent)))
                                               controller-path)
                                       (when var (list var)))))
      (:delete (format nil "~a~a, method: :delete, data: { confirm: ~a, text: ~a }"
                       method-name
                       (format nil "(~{~a~^, ~})"
                               (append (mapcar #'(lambda (ent)
                                                   (strcat "@" (instance-name ent)))
                                               controller-path)
                                       (list (or var (strcat "@" (instance-name (entity aspect)))))))
                       (t.confirmation) (t.delete-warning))))))

(defun action-label (entity action &key (brief? t))
  (ecase action
    (:new (if brief? (t.new) (strcat (t.new) " + ' ' + " (t.long-name entity))))
    (:list (if brief? (t.list) (strcat (t.show) " + ' ' + " (t.long-plural entity))))
    (:edit (t.edit))
    (:detail (if brief? (t.show) (strcat (t.show) " + ' ' + " (t.long-name entity))))
    (:delete (if brief? (t.delete) (strcat (t.delete) " + ' ' + " (t.long-name entity))))))


(defun action-links (aspect page)
  (let ((showable? (showable? aspect))
        (listable? (listable? aspect))
        (updatable? (updatable? aspect))
        (creatable? (creatable? aspect)))
    (remove nil
            (ecase page
              (:detail  (list (when listable? :list)
                              (when updatable? :edit)
                              (when creatable? :new)
                              (when creatable? :delete)))
              (:new     (list (when listable? :list)))
              (:edit    (list (when listable? :list)
                              (when showable? :detail)))
              (:list    (list (when showable? :detail)
                              (when updatable? :edit)
                              (when creatable? :delete)))
              (:context (list (when showable? :detail)
                              (when listable? :list)
                              (when updatable? :edit)))))))

(defun make-action-links (aspect page &key var footer? icons?)
  (remove nil
      (append
       (mapcar #'(lambda (l)
                   (make-action-link aspect l page
                      :var (unless (member page (list :edit :detail)) var)
                      :footer? footer?
                      :icon (when icons? (choose-icon page l))))
               (action-links aspect page))
                 (child-action-links aspect page :footer? footer?))))

(defun choose-icon (page action)
  (declare (ignorable page))
  (case action
    (:delete *svg-delete-icon*)
    (:detail *svg-view-icon*)
    (:list *svg-list-icon*)
    (:new *svg-add-icon*)
    (:edit *svg-edit-icon*)
    (otherwise nil)))

(defun make-action-link (aspect operation page &key var footer? label icon)
  (declare (ignorable footer?))
  (let ((css-classes *link-button-class*))
    (setf css-classes
          (unless (eql page :list)
            (strcat css-classes (ecase operation
                                  (:edit " crud-op-update")
                                  (:delete " crud-op-destroy")
                                  (:new " crud-op-create")
                                  (:detail " crud-op-show")
                                  (:list " crud-op-index")))))
    (unparse-template-link aspect operation :label label :var var :css-class css-classes :icon icon)))

(defun child-action-links (aspect page &key footer?)
  (let ((children (remove-if #'(lambda(ch)
                                 (typep (entity ch) 'attribute-table))
                             (child-aspects aspect))))
    (loop for child in children
          nconcing
          (list
           (when (and (listable? child) (eql page :detail))
             (make-action-link child :list page :footer? footer?
                  :label (action-label (entity child) :list :brief? nil)))
           (when (and (creatable? child) (eql page :detail))
             (make-action-link child :new page :footer? footer?
                  :label (action-label (entity child) :new :brief? nil)))))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
