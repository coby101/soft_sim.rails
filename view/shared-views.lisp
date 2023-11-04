;;;===========================================================================
;;;
;;;   Code for generating views that use "shared" partials. The code produced
;;;    here depends onPCMD specific partials that have not yet been incorporated
;;;    into this generator (see app/views/shared/*.html.erb partials)
;;;
;;;               DEPRECATED!
;;;
;;;===========================================================================

(in-package ror)

(defun generate-view (view)
  (dolist (aspect (aspects (find-view view)))
    (_context.html.erb aspect)
    (when (listable? aspect)
      (_index_actions.html.erb aspect)
      (index.html.erb aspect)
      (_entity.html.erb aspect))
    (when (showable? aspect)
      (_show_actions.html.erb aspect)
      (show.html.erb aspect))
    (when (updatable? aspect)
      (edit.html.erb aspect))
    (when (creatable? aspect)
      (new.html.erb aspect))
   ;(when (member :search ops) ??)
    (when (updatable? aspect)
      (_form.html.erb aspect))))

(defun _index_actions.html.erb (aspect &optional stream)
  (unless (typep (entity aspect) 'attribute-table)
    (let ((file (layout-file-path aspect "_index_actions")))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
        (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "_index_actions.html.erb")))
        (when (creatable? aspect)
          (format (or stream html.erb) "~%<p class=\"pcmdcrud-show-icon-holder\">~%~a~%</p>"
                  (make-action-link aspect :new :detail :footer? t)))))))

(defun _show_actions.html.erb (aspect &optional stream)
  (let ((file (layout-file-path aspect "_show_actions")))
    (with-open-file (html.erb file :direction :output :if-exists :supersede)
      (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "_show_actions.html.erb")))
      (format (or stream html.erb) "~%<div class=\"pcmdcrud-show-icon-holder\">~%~{~a~%~}</div>"
              (make-action-links aspect :detail :icons? t :footer? t)))))

(defmethod show.html.erb ((aspect symbol) &optional stream)
  (show.html.erb (find-aspect aspect stream) t))
(defmethod show.html.erb ((aspect aspect) &optional stream)
 (let ((file (layout-file-path aspect "show"))
       (actions (action-links aspect :detail)))
   (with-open-file (html.erb file :direction :output :if-exists :supersede)
     (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "show.html.erb")))
     (format (or stream html.erb) (ui-label aspect :detail))
        (unless (root? aspect)
          (format (or stream html.erb) "~%  <%= render partial: '~a/~a/context' %>~%"
                  (snake-case (name (view aspect))) (schema-name (next-level aspect))))
     (when actions (format (or stream html.erb) "~%  <%= render partial: 'show_actions' %>~%"))

     (format (or stream html.erb) "
<div class=\"pcmdcrud-show-content-holder\">
  <dl class=\"divide-y\">~{~a~}
  </dl>
</div>"  (mapcar #'(lambda(item)
                     (unparse-detail-element item aspect))
                 (apply #'append (or (panel-items (detail-panel aspect)) (list (panel-items (list-panel aspect))))))))))
;     (when actions (format (or stream html.erb) "~%  <%= render partial: 'show_actions' %>~%")))))

(defmethod unparse-detail-element ((item multi-valued-attribute) (aspect aspect))
  (let ((child-entity (child-entity item)))
    (format nil "
    <%= render partial: 'shared/table', locals: {
         header_title: ~a,
         subheader: true,
         header_columns: [~{~a~^, ~}],
         collection: @~a.~a
   } %>" (t.short-plural child-entity)
   (mapcar #'(lambda(f)
               (format nil "t('~a.~a.index.~a')" (snake-case (name (view aspect)))
                       (schema-name child-entity) (schema-name f)))
           (panel-items (list-panel (find-aspect (view aspect) child-entity))))
   (instance-name (entity aspect)) (schema-name child-entity))))

(defmethod unparse-detail-element ((item t) (aspect aspect))
  (with-output-to-string (str)
    (format str "~%    <div class=\"pcmdcrud-show-content-item-holder\">")
    (format str "~%      <dt class=\"pcmdcrud-show-content-item-key\"><%= t('.~a') %></dt>" (schema-name item))
    (format str "~%      <dd class=\"pcmdcrud-show-content-item-value\">")
    (format str "~%        ~a" (unparse-best-in-place item aspect))
    (format str "~%      </dd>~%    </div>")))

(defvar *bip-ok-button-class* "mt-1 mr-1 shadow bg-gray-200 cursor-pointer py-1 px-2 rounded hover:bg-gray-300")
(defvar *bip-cancel-button-class* "mt-1 shadow bg-gray-200 cursor-pointer py-1 px-2 rounded hover:bg-gray-300")

(defmethod unparse-best-in-place ((item t) (aspect aspect))
  (unparse-template-element item aspect))
(defmethod unparse-best-in-place ((item summary-attribute) (aspect aspect))
  (unparse-template-element item aspect))

(defmethod unparse-best-in-place ((item attribute) (aspect aspect))
  (if (editable? item aspect)
      (html:tag "span"
         (unparse-erb t
              (format nil "best_in_place ~a, :~a, ~a"
                      (form-path aspect (strcat "@" (instance-name (entity aspect))))
                      (schema-name item) (best-in-place-args item aspect)))
         :class "flex-grow")
      (unparse-template-element item aspect)))

(defmethod best-in-place-args ((item attribute) (aspect aspect))
  (if (or (typep (domain item) 'enumeration) (typep item 'foreign-key))
      (format nil "as: :select, collection: ~a" (best-in-place-collection-args item aspect))
      (format nil "ok_button: '&#x2713;'.html_safe, cancel_button: ~
                  '&#x2717;'.html_safe, ok_button_class: '~a', cancel_button_class: '~a'"
              *bip-ok-button-class* *bip-cancel-button-class*)))

(defmethod best-in-place-collection-args (item aspect)
  (if (typep (domain item) 'static-enumeration)
      (let*((null? (nullable? item))
            (prompt (if null? "" "(required)")))
        (format nil "[~a~{[~s, ~:*~s]~^, ~}]" (format nil "[\"~a\", \"\"], " prompt)
                (legal-values (domain item))))
      (unparse-referential-collection item aspect)))

(defmethod best-in-place-collection-args (item aspect)
  (cond
    ((and (eql :boolean (data-type item)) (not (implement-as-string? item)))
     (let ((options (list (list "true" (ecase (id (logical-type item))
                                         (:yes/no "yes")
                                         (:on/off "on")
                                         (:true/false "True")))
                          (list "false" (ecase (id (logical-type item))
                                          (:yes/no "no")
                                          (:on/off "off")
                                          (:true/false "False"))))))
       (format nil "[~{~a~^, ~}]" (mapcar #'(lambda (opt) (format nil "[~a, ~s]" (car opt) (cadr opt)))
                                                options))))
    ((typep (domain item) 'static-enumeration)
     (let*((null? (nullable? item))
           (prompt (if null? "" "(required)")))
       (format nil "[~a~{[~s, ~:*~s]~^, ~}]" (format nil "[\"~a\", \"\"], " prompt)
               (legal-values (domain item)))))
    (t (unparse-referential-collection item aspect))))

;; @tenant.business_types.order(:name).map{|v| [v.name, v.name]}
;; also handle foreign keys

(defun unparse-referential-collection (item aspect)
  (declare (ignorable aspect))
  (let* ((filter nil)
         (data-source (data-source (domain item)))
         (source-entity (my-entity data-source))
         (user-data (schema-name
                     (if (typep item 'foreign-key)
                        (default-user-key source-entity)
                        (or (find-field :name source-entity) data-source))))
         (prompt (if (nullable? item) "[[nil, ' ']] + " "")))
    (format nil "~a~a.order(:~a).map{ |v| [v.~a, v.~a]}" prompt
            (target-data-expression item filter) user-data (schema-name data-source) user-data)))


(defun index.html.erb (aspect &optional stream)
 (let ((file (layout-file-path aspect "index"))
       (actions (action-links aspect :detail)))
   (with-open-file (html.erb file :direction :output :if-exists :supersede)
     (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "index.html.erb")))
     (format (or stream html.erb) (ui-label aspect :list))
     (unless (root? aspect)
       (format (or stream html.erb) "~%  <%= render partial: '~a/~a/context' %>~%"
               (snake-case (name (view aspect))) (schema-name (next-level aspect))))
     (when actions (format (or stream html.erb) "~%  <%= render partial: 'index_actions' %>~%"))

     (format (or stream html.erb) "
  <%= render partial: 'shared/table', locals: {
    header_title: '',
    header_columns: [~{~a~^, ~}],
    collection: @~a
  } %>" (mapcar #'t.name (panel-items (list-panel aspect))) (snake-case (plural (entity aspect))))
     (when actions (format (or stream html.erb) "~%  <%= render partial: 'index_actions' %>~%")))))

(defun next-level (aspect)
  (get-closest-relative (entity aspect) (mapcar #'entity (aspects (view aspect)))))

(defun _context.html.erb (aspect &optional stream)
  (unless (or (nested-fields? aspect) (null (context-panel aspect)))
    (let* ((file (layout-file-path aspect "_context"))
           (actions (action-links aspect :detail)))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
        (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "_context.html.erb")))
        ;; present a heading?
        (format (or stream html.erb) "~%<div class=\"pcmdcrud-container-4 pcmdcrud-context\">")
        (unless (root? aspect)
          (format (or stream html.erb) "~%  <%= render partial: '~a/~a/context' %>~%"
                  (snake-case (name (view aspect))) (schema-name (next-level aspect))))
        (when actions
          (format (or stream html.erb) "~%  <div class=\"pcmdcrud-show-icon-holder\">~%    ~{~a~%    ~}</div>"
                  (make-action-links aspect :context :var (strcat "@" (instance-name (entity aspect))) :icons? t)))
        (format (or stream html.erb) "
    <div class=\"pcmdcrud-show-content-holder\">
      <dl class=\"divide-y\">~{~a~}
      </dl>
    </div>" (mapcar #'(lambda(item)
                    (unparse-detail-element item aspect))
                (apply #'append (panel-items (context-panel aspect)))))
        (format (or stream html.erb) "~%</div>")))))

(defun edit.html.erb (aspect &optional stream)
  (unless (typep (entity aspect) 'attribute-table)
    (let* ((entity (entity aspect))
           (file (layout-file-path aspect "edit"))
           (model-var (strcat "@" (instance-name entity)))
           (action-html (make-action-links aspect :edit :var model-var :icons? t)))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
        (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "edit.html.erb")))
        (format (or stream html.erb) "~%~a"
                (unparse-erb t
                             (format nil "model_error_messages(~a, ~a)" model-var (t.long-name entity))))
        (format (or stream html.erb) (crud-page-heading))
        (unless (root? aspect)
          (format (or stream html.erb) "~%  <%= render partial: '~a/~a/context' %>~%"
                  (snake-case (name (view aspect))) (schema-name (next-level aspect))))
        (format (or stream html.erb) "<br>~{~a~%~}" action-html)
        (format (or stream html.erb)
                (unparse-erb t (format nil "render 'form', ~a: ~a" (instance-name entity) model-var)))
        (format (or stream html.erb) "<br>~{~a~%~}" action-html)))))

(defun new.html.erb (aspect &optional stream)
  (unless (typep (entity aspect) 'attribute-table)
    (let* ((entity (entity aspect))
           (file (layout-file-path aspect "new"))
           (model-var (strcat "@" (instance-name entity)))
           (action-html (make-action-links aspect :new :icons? t)))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
        (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "new.html.erb")))
        (format (or stream html.erb) "~%~a"
                (unparse-erb t
                             (format nil "model_error_messages(~a, ~a)" model-var (t.long-name entity))))
        (format (or stream html.erb) "~%~a~%" (ui-label aspect :new))
        (unless (root? aspect)
          (format (or stream html.erb) "~%  <%= render partial: '~a/~a/context' %>~%"
                  (snake-case (name (view aspect))) (schema-name (next-level aspect))))
        (format (or stream html.erb) "<br>~{~a~%~}" action-html)
        (format (or stream html.erb)
                (unparse-erb t (format nil "render 'form', ~a: ~a" (instance-name entity) model-var)))
        (format (or stream html.erb) "<br>~{~a~%~}" action-html)))))

;; there is a lot of duplicate code in view.lisp::form.html.erb
(defun _form.html.erb (aspect &optional stream)
  (unless (typep (entity aspect) 'attribute-table)
    (let ((*simple-table-layouts?* nil)
          (file (layout-file-path aspect "_form"))
          (before-elements (get-additional-page-elements aspect :form :before))
          (after-elements (get-additional-page-elements aspect :form :after)))
      (with-open-file (html.erb file :direction :output :if-exists :supersede)
        (format (or stream html.erb) (unparse-erb nil (format-file-notice nil (file-namestring file))))
        (write-update-form aspect (or stream html.erb) :before before-elements :after after-elements)))
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


(defmethod _entity.html.erb ((aspect symbol) &optional stream)
  (_entity.html.erb (find-aspect aspect stream) t))
(defmethod _entity.html.erb ((aspect aspect) &optional stream)
  (let* ((entity (entity aspect))
         (var (instance-name entity))
         (file (layout-file-path aspect (strcat "_" (instance-name entity)))))
    (with-open-file (html.erb file :direction :output :if-exists :supersede)
      (format (or stream html.erb) (unparse-erb nil (format-file-notice nil "_entity.html.erb")))
      (format (or stream html.erb)
              (html:trow
               (strcat
                (format nil "~{~%    <td class=\"pcmdcrud-td\">~a</td>~}~%"
                        (mapcar #'(lambda(item)
                                    (let* ((expression (unparse-template-expression item var))
                                           (formatted-expression
                                             (cond
                                               ((typep item 'attribute)
                                                (unparse-formatting expression (logical-type item)))
                                               ((field-reference-expression? item)
                                                (unparse-formatting expression (logical-type (cadr item))))
                                               (t expression))))
                                      (if (and (typep item 'summary-attribute)
                                               (find-aspect (view aspect) (entity (car (path item)))))
                                          (make-linked-element item aspect formatted-expression var)
                                          (unparse-erb t formatted-expression))))
                                (panel-items (list-panel aspect))))
                (if (typep (entity aspect) 'attribute-table)
                    ""
                    (format nil "~{~%    <td class=\"pcmdcrud-td-action-icon\">~a</td>~}~%"
                            (make-action-links aspect :list :var var :icons? t))))
               :class "bg-white")))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
