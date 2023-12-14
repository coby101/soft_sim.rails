;;;===========================================================================
;;;
;;;   Code for uparsing layout and form elements
;;;
;;;===========================================================================
 
(in-package :ror)

(defmethod unparse-form-element ((item attribute))
  (html:div
   (list (unparse-erb t (format nil "form.label :~a, t('.~a'), class: \"~a\""
                                (schema-name item) (schema-name item) (form-label-class item)))
         (unparse-erb t (format nil "~a~a" (unparse-form-helper item (logical-type item))
                                (if (nullable? item) ""
                                    ""))))
   :class (form-element-class item)))

(defmethod unparse-form-element ((item string))
  item)

(defmethod unparse-form-element ((item multi-valued-attribute))
  (let ((entity (child-entity item)))
    (format nil "<br>~a<br>"
            (html:div
             (format nil "~%~a~%~a~a~%~a~a~%~a~a~%~a"
                     (with-nesting (html:heading 3 (unparse-erb t (t.short-plural entity))))
                     (html:make-indent)
                     (unparse-erb t (format nil "form.fields_for :~a do |~a_form|"
                                            (schema-name entity) (instance-name entity)))
                     (html:make-indent)
                     (unparse-erb t (format nil "render \"~a_fields\", form: ~:*~a_form" (instance-name entity)))
                     (html:make-indent)
                     (unparse-erb nil "end")
                     (unparse-erb t
                        (format nil "link_to_add_fields ~a + ' ' + ~a, form, :~a"
                                (t.add) (t.long-name entity) (schema-name entity))))
             :class *nested-form-class*))))

(defmethod unparse-form-helper ((item attribute) (type t) &key (class (form-field-class item)))
  (declare (ignorable type))
  (text_field (schema-name item) :class class :nullable? (not (require-user-input? item))))

(defmethod unparse-form-helper ((item foreign-key) (type logical-type) &key (class (form-field-class item)))
  (unparse-form-helper item :foreign-key :class class))

(defmethod unparse-form-helper ((item attribute) (type logical-type) &key (class (form-field-class item)))
  (let* ((domain (domain item)))
    (cond
      ((and (eql (data-type item) :boolean)
            (not (implement-as-string? item)))
       (unparse-form-helper item :boolean :class class))
      ((typep domain 'static-enumeration)
       (unparse-select-helper item (legal-values domain)))
      ((typep domain 'referential-enumeration)
       (unparse-referential-controller item))
      (t (unparse-form-helper item (id type) :class class)))))

(defmethod unparse-form-helper ((item attribute) (type (eql :label)) &key (class (form-field-class item)))
  (text_field (schema-name item) :size 25 :class class :nullable? (not (require-user-input? item))))

(defmethod unparse-form-helper ((item attribute) (type (eql :long-name)) &key (class (form-field-class item)))
  (text_area (schema-name item) :size "60x1" :class class :nullable? (not (require-user-input? item))))
(defmethod unparse-form-helper ((item attribute) (type (eql :phone)) &key (class (form-field-class item)))
  (format nil "form.telephone_field :~a, class: ~s~a" (schema-name item) class
          (if (require-user-input? item) ", required: true" "")))
(defmethod unparse-form-helper ((item attribute) (type (eql :email)) &key (class (form-field-class item)))
  (format nil "form.email_field :~a, class: ~s~a" (schema-name item) class
          (if (require-user-input? item) ", required: true" "")))
(defmethod unparse-form-helper ((item attribute) (type (eql :color)) &key (class (form-field-class item)))
  (format nil "form.color_field :~a, class: ~s" (schema-name item) class))
(defmethod unparse-form-helper ((item attribute) (type (eql :url)) &key (class (form-field-class item)))
  (format nil "form.url_field :~a, class: ~s~a" (schema-name item) class
          (if (require-user-input? item) ", required: true" "")))
(defmethod unparse-form-helper ((item attribute) (type (eql :date)) &key (class (form-field-class item)))
;;https://api.rubyonrails.org/v6.1.4/classes/ActionView/Helpers/DateHelper.html#method-i-date_select
  (format nil "form.date_select :~a, class: ~s" (schema-name item) class))
(defmethod unparse-form-helper ((item attribute) (type (eql :date)) &key (class (form-field-class item)))
  (format nil "form.date_field :~a, class: ~s~a" (schema-name item)
          class (if (require-user-input? item) ", required: true" "")))
(defmethod unparse-form-helper ((item attribute) (type (eql :password)) &key (class (form-field-class item)))
  (format nil "form.password_field :~a, class: ~s~a" (schema-name item)
          (if (require-user-input? item) ", required: true" "") class))
(defmethod unparse-form-helper ((item attribute) (type (eql :memo)) &key (class (form-field-class item)))
  (text_area (schema-name item) :class class :nullable? (not (require-user-input? item))))
(defmethod unparse-form-helper ((item attribute) (type (eql :long-text)) &key (class (form-field-class item)))
  (text_area (schema-name item) :size "60x2" :class class :nullable? (not (require-user-input? item))))
(defmethod unparse-form-helper ((item attribute) (type (eql :short-text)) &key (class (form-field-class item)))
  (text_field (schema-name item) :class class :nullable? (not (require-user-input? item))))
(defmethod unparse-form-helper ((item attribute) (type (eql :google-address)) &key (class (form-field-class item)))
  (text_field (schema-name item) :id "google_addrress_field" :class class :nullable? (not (require-user-input? item))))

(defun text_area (name &rest properties &key nullable? (size "70x4") (class *default-text-area-class*)
                  &allow-other-keys)
  (strcat (apply #'text_ "area" name :size size :class class
                 (filtered-properties properties :nullable? :size :class))
          (or (and (not nullable?) ", required: true") "")))

(defun text_field (name &rest properties &key nullable? (size 40) (class *default-text-input-class*)
                   &allow-other-keys)
  (strcat (apply #'text_ "field" name :size size :class class
                 (filtered-properties properties :nullable? :size :class))
          (or (and (not nullable?) ", required: true") "")))

(defun text_ (text-type name &rest properties)
  (format nil "form.text_~a :~a~{, ~a: \"~a\"~}"
          text-type name (downcase-keys properties)))


(defmethod unparse-form-helper ((item attribute) (type (eql :boolean)) &key (class (form-field-class item)))
  (let ((checked? (member (default-value item) (list "yes" "on" "true") :test #'string-equal)))
    (format nil "form.check_box :~a, class: ~s~a~a" (schema-name item) class
          (if checked? ", checked: true" "")
          (if (require-user-input? item) "" "")))) ;; javascript does not allow an unchecked checkbox
;          (if (require-user-input? item) ", required: true" ""))))

(defmethod unparse-form-helper ((item attribute) (type (eql :foreign-key)) &key (class (form-field-class item)))
  (declare (ignorable class))
  (let* ((target (my-entity (source item)))
	     (order (or (ordering (path item)) (default-sort-field target))))
    (unparse-collection-select-helper
     item (default-user-key target) (primary-key target) order (constraints (path item)))))

;;; check out option groups as well
;;; https://www.w3schools.com/tags/tag_optgroup.asp
;;; https://guides.rubyonrails.org/form_helpers.html#option-groups
(defun unparse-select-helper (att values)
  (let*((null? (nullable? att))
        (default (default-value att))
        (prompt (if null? "" "(required)")))
    (format nil "form.select :~a, [~a~{[~s, ~:*~s]~^, ~}], {~a}, { class: ~s~a }" (schema-name att)
	        (format nil "[\"~a\", \"\"], " prompt)
            values
	        (if default
	            (format nil " selected: form.object.~a || ~s " (schema-name att) default)
	            (format nil " selected: form.object.~a || ''" (schema-name att)))
            *default-form-collection-class*
            (if (require-user-input? att) ", required: true" ""))))
;; select box behaviour:
;;   - mandatory field with no default:
;;      - options must include a "please select" prompt as the initial value,
;;        model validations will enforce non-null
;;   - mandatory field with a default value:
;;      - no blank option, default is preselected
;;   - optional field with no default:
;;      - values include a blank with empty text that is preselected
;;   - optional field with a default
;;      - options include a blank with "(not applicable)", default is preselected
(defun unparse-referential-controller (item)
  (let* ((data-source (data-source (domain item)))
         (sort-data (or (find-field :code (my-entity data-source))
                        (find-field :name (my-entity data-source))
                        data-source))
	     (prompt-data (or (default-user-key (my-entity data-source)) data-source)))
    (unparse-collection-select-helper item prompt-data data-source sort-data)))

(defun unparse-collection-select-helper (item display-data store-data sort &optional filter)
  (let* ((prompt (if (nullable? item) "''" "t('helpers.select.prompt')")))
    (format nil "form.collection_select :~a, ~a.order(:~a), :~a, :~a, ~
                { :include_blank => ~a }, { class: ~s~a }"
	        (schema-name item) (target-data-expression item filter)
	        (schema-name sort) (schema-name store-data)
	        (schema-name display-data)
	        prompt *default-form-collection-class*
            (if (require-user-input? item) ", required: true" ""))))

(defmethod unparse-template-element ((item multi-valued-attribute) (aspect aspect) &key obj-var labeled? css-class-string)
  (declare (ignorable obj-var labeled? css-class-string aspect))
  (let ((entity (child-entity item))
        (actions nil))
    (format nil "~a~%~a~%~a"
            (indent-block nil (data-list-element (ui-label entity :list)
                                    :colspan (+ (length actions) (length (user-attributes entity)))))
              (indent-block nil (data-list-row (list-column-headings entity (length actions))))
              (indent-block nil  ;; FIXME simian::make-aspect - not appropriate
                  (list-body (simian::make-aspect entity '(:list) :list-panel (user-attributes entity))
                             actions)))))

(defmethod unparse-template-element ((item formula) (aspect aspect) &key obj-var labeled? css-class-string)
  (declare (ignorable obj-var labeled? css-class-string aspect))
  (unparse-expression (unparse-attribute-references (expression item) (context item) obj-var) :ruby))

(defmethod unparse-template-element ((item string) (aspect t) &key obj-var labeled? css-class-string)
  (declare (ignorable obj-var labeled? css-class-string aspect))
  item);(format nil "~a" (escape-characters item #\')))

(defmethod unparse-template-element ((element t) (aspect aspect) &key obj-var labeled? css-class-string)
  (when (not (or (typep element 'attribute) (typep element 'list)))
    (error "no code to handle object ~a of type ~a" element (type-of element)))
  (let* ((expression (unparse-template-expression element obj-var))
         (label (if labeled? (unparse-template-label element) ""))
         (formatted-expression (cond
                                 ((and (typep element 'attribute)
                                       (eql :boolean (data-type element))
                                       (implement-as-string? element))
                                  (unparse-formatting expression :label))
                                 ((typep element 'attribute)
                                  (unparse-formatting expression (logical-type element)))
                                 ((field-reference-expression? element)
                                  (unparse-formatting expression (logical-type (cadr element))))
                                 (t expression))))
    (format nil "~a~a" label
            (html:div (if (and (typep element 'summary-attribute)
                                (find-aspect (view aspect) (entity (car (path element)))))
                           (make-linked-element element aspect formatted-expression obj-var)
                           (unparse-erb t formatted-expression))
                       :class (strcat (or (and css-class-string (strcat css-class-string " ")) "")
                                      (field-value-class element))))))

(defmethod make-linked-element ((element summary-attribute) aspect display obj-var)
  (let ((child-aspect (find (my-entity (source element)) (child-aspects aspect) :key #'entity)))
    (if child-aspect
        (unparse-template-link child-aspect :list :var obj-var :label display :external? t)
        (unparse-erb t display))))

;; can adjust the class value here according to logical type
(defmethod unparse-template-label ((item attribute))
  (format nil "<strong>~a</strong>" (html:div (unparse-erb t (t.name item)) :class (field-label-class item))))

(defmethod unparse-template-label ((item list))
  (if (field-reference-expression? item)
      (format nil "<strong>~a</strong>"
              (html:div (unparse-erb t (t.name item)) :class (field-label-class item)))
      (error "don't know what to do with this list: ~a" item)))

(defmethod unparse-template-expression ((attribute attribute) &optional obj-var)
  (format nil "~a.~a" (or obj-var (strcat "@" (instance-name (my-entity attribute))))
          (schema-name attribute)))

(defmethod unparse-template-expression ((attribute summary-attribute) &optional obj-var)
  (if (typep attribute 'persistent-attribute)
      (call-next-method)
      (format nil "~a.~a.~a" (or obj-var (strcat "@" (instance-name (my-entity attribute)))) 
              (schema-name (car (path attribute)))
              (unparse-summary-method attribute))))

(defmethod unparse-template-expression ((item list) &optional obj-var)
  (if (field-reference-expression? item)
      (let ((var (or obj-var (strcat "@" (instance-name (entity (my-relation (car item))))))))
        (unparse-parent-field item var))
      (mapcar #'(lambda (elt) (unparse-template-expression elt obj-var))
              item)))

(defmethod unparse-template-expression ((item formula) &optional obj-var)
  (unparse-expression (unparse-attribute-references (expression item) (context item) obj-var) :ruby))

(defun unparse-template-link (aspect action &key var (short-label? t) icon label css-class style external?)
  (when (and icon external?)
    (error "can't unparse an icon with an external link call"))
  (let* ((entity (entity aspect))
         (link-method-call
           (unparse-erb t
             (format nil "~a ~a~a~a~a~a~a~a"
                     (if external? "link_with_external_link" "link_to")
                     (if icon
                         ""
                         (if label 
                             (strcat label ", ")
                             (format nil "~a, " (action-label entity action :brief? short-label?))))
                     (path-method-call aspect action var)
                     (if css-class (format nil ", class: '~a'" css-class) "")
                     (if external? "" (format nil ", title: ~a" (t.button-title action entity)))
                     (if style (format nil ", style: '~a'" style) "")
                     (if external? ", '_self'" "")
                     (if icon " do" "")))))
    (if icon
        (format nil "~a~%~a~a~%~a<% end %>" link-method-call (make-indent) icon (make-indent))
        link-method-call)))

(defun unparse-form-object-id (aspect &optional (var "form.object"))
  (let* ((model (model-name (entity aspect)))
         (parent (model-name (owner (entity aspect)))))
    (format nil "~a<%= ~a.id %>" (replace-all model parent (snake-case parent))
            var)))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
