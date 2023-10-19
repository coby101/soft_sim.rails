;;;===========================================================================
;;; file:   generators/ror/unparser.lisp
;;; auth:   Coby Beck
;;; date:   2020-12-04
;;; update: 
;;;---------------------------------------------------------------------------
;;;   Code for uparsing Ruby on Rails specific methods/syntax
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(defun reserved-column-name? (name ent)
  ;; still need to add 0 to many (association_name)_type for relations
  ;; and 0 to many (table_name)_count for children
  (member
   name
   (list
    (strcat (snake-case (name ent)) "_id")
    "id"
    "type" ;; a table designation for a single inheritance
    "lock_version" ;; Adds optimistic locking to a model
    "updated_at" ;; modificatioin timestamp
    "created_at" ;; creation timestamp
    )))

(defmethod model-name ((att attribute))
  (model-name (my-entity att)))
(defmethod model-name ((ent entity))
  (camel-case (name ent)))
(defmethod model-name ((rel relation))
  (camel-case (name (entity rel))))
(defmethod model-name ((str string))
  (camel-case str))

(defmethod schema-name ((ref list))
  (if (field-reference-spec? ref)
      (strcat (snake-case (name (car ref))) "_"
              (if (= (length (cdr ref)) 1)
                  (schema-name (cadr ref))
                  (schema-name (cdr ref))))
      (error "can not handle ~a" ref)))

(defmethod schema-name ((rel relation))
  (snake-case (plural rel)))

(defmethod schema-name ((ent entity))
  (snake-case (plural ent)))
(defmethod schema-name ((ent specialized-entity))
  (schema-name (super ent)))

(defmethod schema-name ((att attribute))
  (let ((name (snake-case (name att))))
    (if (reserved-column-name? name (my-entity att))
        (strcat (schema-name (my-entity att)) "_" name) 
        name)))

(defmethod schema-name ((att foreign-key))
  (call-next-method))

(defmethod schema-name ((att primary-key))
  (call-next-method));"id")

(defmethod unparse ((obj list))
  (if (and (= 2 (length obj))
           (field-reference-expression? obj)
           (or (eq (entity (car obj)) (my-entity (cadr obj)))
               (eq (car obj) (my-entity (cadr obj)))))
      (if (eq (entity (car obj)) (my-entity (cadr obj)))
          (format nil "~a_~a" (snake-case (name (car obj))) (schema-name (cadr obj))) 
          (unparse (cadr obj)))
      (ruby:unparse obj)))

(defmethod unparse ((obj entity))
  ;; snake-case is a bit arbitrary but a common convention
  (schema-name obj))

(defmethod unparse ((obj attribute))
  (snake-case (name obj)))

(defmethod unparse ((obj calculated-attribute))
  (unparse (expression (formula obj))))

(defmethod unparse ((obj relation)) (call-next-method)); (unparse (keywordify (schema-name obj))))

(defmethod unparse-expression ((obj attribute) &optional args)
  (when args
    (error "we shouldn't have any args here...? (~a)" args))
  (schema-name obj))

(defmethod unparse-expression ((obj calculated-attribute) &optional args)
  (when args
    (error "we shouldn't have any args here...? (~a)" args))
  (unparse-expression (formula obj)))

(defmethod unparse-expression ((operator (eql '$rows)) &optional args) 
  (let ((class (model-name (car args)))
        (where (if (cadr args)
                   (format nil ".where(\"~a\")" (sql::unparse-expression (cadr args)))
                   "")))
    (format nil "~a~a.count" class where)))

(defmethod unparse-expression ((operator (eql '$unchanged)) &optional args)
;; this check fails on ($literal "supplier_id")
;  (unless (or (typep (car args) 'string) (typep (car args) 'attribute))
;    (error "$UNCHANGED is only appropriate for an attribute expression"))
  (format nil "~a_change_to_be_saved == nil" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$new-value)) &optional args)
  (format nil "~a" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$old-value)) &optional args)
 ; (unless (or (typep (car args) 'string) (typep (car args) 'attribute))
 ;   (error "$OLD_VALUE is only appropriate for an attribute expression"))  
  (format nil "~a_change_to_be_saved ? ~:*~a_change_to_be_saved.first : ~:*~a" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$stop-delete)) &optional args) 
  (format nil "errors.add(:~a, ~s)" (unparse (primary-key (my-entity (car args))))
            "deletion is not allowed"))

;;Record.count(:all, :conditions => {:created_at => start_date..end_date, :finished_at => nil })
(defmethod unparse-expression ((operator (eql '$max-rows)) &optional args)
  (unparse-expression
   '$<= (list (list '$rows (car args) (caddr args)) (cadr args))))

(defmethod unparse-expression ((operator (eql '$min-rows)) &optional args)
  (unparse-expression
   '$>= (list (list '$rows (car args) (caddr args)) (cadr args))))

(defmethod unparse-expression ((operator (eql '$rows-eql)) &optional args)
  (unparse-expression
   '$= (list (list '$rows (car args) (caddr args)) (cadr args))))

(defmethod unparse-expression ((operator (eql '$as-money)) &optional args)
  (format nil "number_to_currency(~a)" (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$as-quantity)) &optional args)
  (format nil "helper.number_with_precision(~a, :precision => 2, :delimiter => ',')"
          (unparse-expression (car args))))

(defmethod unparse-expression ((operator (eql '$not-null)) &optional args)
  (let ((field-var (unparse-expression (first args))))
    (format nil "~a.present?" field-var)))

(defmethod unparse-expression ((operator (eql '$null)) &optional args)
  (let ((field-var (unparse-expression (first args))))
    (format nil "~a.blank?" field-var)))

(defmethod unparse-attribute-value ((attribute attribute) (value t))
  (unparse-data (data-type attribute) value))

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
  (format nil "form.color_field :~a" (schema-name item)))
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

(defun path-to-scope-object (common-ancestor entity source-entity)
  (if common-ancestor
      (if (eq common-ancestor (tenant-entity))
          (strcat "@" "tenant");(instance-name (path (tenant-key entity)))) - fails on repeated attribute tables
          (format nil "@~{~a~^.~}"
                  (mapcar #'instance-name
                          (list* entity
                                 (butlast (path-to-attribute entity (primary-key common-ancestor)))))))
      (if (tenant-scoped-entity? source-entity)
          (format nil "@~a.~a" (instance-name (path (tenant-key source-entity))) (schema-name source-entity))
          (model-name source-entity))))

(defun path-down-to-target-data (common-ancestor source-entity)
  (when common-ancestor
    (let* ((path-to-attribute (butlast (path-to-attribute source-entity (primary-key common-ancestor))))
           (effective-path (if (eq common-ancestor (tenant-entity))
                               (butlast path-to-attribute)
                               (cdr path-to-attribute))))
      (when effective-path
        (if (and (= 1 (length effective-path))
                 (member source-entity (aggregate-children (entity (car effective-path)))))
            (setf effective-path nil)))
      (reverse (list* source-entity effective-path)))))
#|
(setf item simian:@claim.project)
(setf entity (my-entity item))
(setf source-entity (my-entity (data-source (domain item))))
(setf common-ancestor (nearest-common-ancestor entity source-entity 
                        :ignore-list (list* source-entity entity (parents entity))))
(setf path-up (path-to-scope-object
                (unless (member common-ancestor (parents entity)) common-ancestor)
 entity source-entity))
(setf path-down (path-down-to-target-data 
                  (unless (member common-ancestor (parents entity)) common-ancestor)
 source-entity)))

(mapcar #'(lambda (item) 
             (format t "~%~a.~a: ~s" (name (my-entity item)) (name item)
                    (ror:target-data-expression item)))
  (list (find-field :codecontroller :estimate)
 (find-field :workitem :estimate)
 (find-field :projectpart :workitem)
 (find-field :scheduleitem :workitem)
 (find-field :schedulepart :scheduleitem)
 (find-field :project :claim)
 (find-field :itemcodecontroller :claimline)))

|#

(defmethod target-data-expression ((item attribute) &optional filter)
  (declare (ignorable filter))
  (or (get-custom-code :target-data-expression item)
      (let* ((entity (my-entity item))
             (source-entity (my-entity (data-source (domain item))))
             (common-ancestor (nearest-common-ancestor entity source-entity
                                 :ignore-list (list* source-entity entity
                                                     (remove-if #'(lambda(p)
                                                                    (member p (parents source-entity)))
                                                                (parents entity)))))
             (path-up (path-to-scope-object common-ancestor entity source-entity))
             (path-down (path-down-to-target-data common-ancestor source-entity)))
        (when (> (length path-down) 1)
          (warn "generating select box for ~s: ~%~a~{.~a~} will not work!"
                (english:unparse item) path-up (mapcar #'schema-name path-down)))
        (format nil "~a~{.~a~}" path-up (mapcar #'schema-name path-down)))))

;  (format nil "~a~a"  (model-name source-entity)
;	        ;; note if test, fix when needed!
;	      (if (and filter nil) ".<state> or .where<expression goes here>" ""))))

(defmethod unparse-template-element ((item multi-valued-attribute) (aspect aspect)
                                        &key obj-var labeled? css-class-string)
  (declare (ignorable obj-var labeled? css-class-string aspect))
  (let ((entity (child-entity item))
        (actions nil))
    (format nil "~a~%~a~%~a"
            (indent-block nil (data-list-element (ui-label entity :list)
                                    :colspan (+ (length actions) (length (user-attributes entity)))))
              (indent-block nil (data-list-row (list-column-headings entity (length actions))))
              (indent-block nil
                  (list-body (make-aspect entity '(:list) :list-panel (user-attributes entity))
                             actions)))))

(defmethod unparse-best-in-place (item aspect)
  (format nil
          "
</div>
" (t.long-name item)))

(defmethod unparse-template-element ((item formula) (aspect aspect)
                                        &key obj-var labeled? css-class-string)
  (declare (ignorable obj-var labeled? css-class-string aspect))
  (ruby:unparse-expression
   (unparse-attribute-references
    (expression item) (context item) obj-var)))

(defmethod unparse-template-element ((item string) (aspect t)
                                        &key obj-var labeled? css-class-string)
  (declare (ignorable obj-var labeled? css-class-string aspect))
  item);(format nil "~a" (escape-characters item #\')))

(defmethod unparse-template-element ((element t) (aspect aspect)
                                     &key obj-var labeled? css-class-string)
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
  (ruby:unparse-expression
   (unparse-attribute-references
    (expression item) (context item) obj-var)))

#|
  <td class="pcmdcrud-td-action-icon">
    <%= link_to mng_comp_company_path(company), method: :delete, 
                data: { confirm: t('global.delete_confirmation') },
               class: 'text-indigo-600 hover:text-indigo-900' do %>
       <svg class="h-5 w-5 text-red-600" 
            width="24"
            height="24"
            viewBox="0 0 24 24"
            stroke-width="2"
            stroke="currentColor"
            fill="none" 
            stroke-linecap="round"
            stroke-linejoin="round"> 
            <path stroke="none" d="M0 0h24v24H0z"/> 
            <line x1="4" y1="7" x2="20" y2="7" />  
            <line x1="10" y1="11" x2="10" y2="17" /> 
            <line x1="14" y1="11" x2="14" y2="17" /> 
            <path d="M5 7l1 12a2 2 0 0 0 2 2h8a2 2 0 0 0 2 -2l1 -12" />
            <path d="M9 7v-3a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v3" />
       </svg>
    <% end %>
|#

(defun unparse-template-link (aspect action &key var (short-label? t)
                                     icon label css-class style external?)
  (when (and icon external?)
    (error "can't unparse an icon with an external link call"))
  (let* ((entity (entity aspect))
         (link-method-call
           (unparse-erb t
             (format nil "~a ~a~a~a~a~a~a~a"
                     (if external? "link_with_external_link" "link_to")
                     (if icon
                         ""
                         (if label (strcat label ", ")
                             (format nil "~a, "
                                     (action-label entity action :brief? short-label?))))
                     (path-method-call aspect action var)
                     (if css-class (format nil ", class: '~a'" css-class) "")
                     (if external? ""
                         (format nil ", title: ~a" (t.button-title action entity)))
                     (if style (format nil ", style: '~a'" style) "")
                     (if external? ", '_self'" "")
                     (if icon " do" "")))))
    (if icon
        (format nil "~a~%~a~a~%~a<% end %>" link-method-call (make-indent) icon (make-indent))
        link-method-call)))

(defmethod unparse-path ((att attribute) (context entity) &key bad-start context-var)
  (let ((path (path-to-attribute context att (my-entity att) bad-start)))
    (format nil "~a~{.~a~}" (or context-var (instance-name (car path)))
            (mapcar #'instance-name (cdr path)))))

(defmethod unparse-summary-method ((att summary-attribute))
  (format nil "~a(~a)" (summary-method (summary-type att))
          (summary-arg att)))

(defun summary-method (sum-type)
  (ecase sum-type
    (:count "size") ;; not "count", avoids a db query
    (:sum "sum")
    (:average "average")
    (:mean (or (warn "unknown summary method for :mean") "mean"))
    (:median (or (warn "unknown summary method for :median") "median"))
    (:range (or (warn "unknown summary method for :range") "range"))
    (:max "maximum")
    (:min "minimum")))

(defun summary-arg (att)
  (if (eql (summary-type att) :count)
      ""
      ;; this will need more effort for calculated attributes etc
      (strcat ":" (schema-name (source att)))))

(defmethod unparse-erb (output? (code string))
  (format nil "<%~a ~a %>" (if output? "=" "") code))

(defmethod unparse-erb (output? (obj t))
  (unparse-erb output? (ruby:unparse obj)))

(defmethod unparse-erb (output? (obj list))
  (if (or (typep (car obj) 'operator) (operator-symbol? (car obj)))
      (unparse-erb output? (ruby:unparse-expression (car obj) (cdr obj)))
      (ruby:unparse-expression (ruby:unparse obj) output?)))

;; this is not debugged yet, but not used seriously yet either
(defmethod unparse-formatting ((data t) (type (eql :checkbox)))
  (format nil "check_box_tag  '~a', ~a, (~a == 1 ? true : false), ~
               disabled: true, style: \"width: 20px; height: 20px\""
	  data data data))

(defmethod unparse-formatting ((data t) (type t))
  (ruby:unparse-formatting data type))
(defmethod unparse-formatting ((data t) (type logical-type))
  (unparse-formatting data (id type)))

(defmethod unparse-formatting ((data t) (type (eql :yes/no)))
  (format nil "~a ? 'yes' : 'no'" data))
(defmethod unparse-formatting ((data t) (type (eql :on/off)))
  (format nil "~a ? 'on' : 'off'" data))
(defmethod unparse-formatting ((data t) (type (eql :true/false)))
  (format nil "~a ? 'True' : 'False'" data))

(defmethod unparse-formatting ((data t) (type (eql :money)))
  (format nil "~a.blank? ? ~a : number_to_currency(~a)" data (t.no-data) data))

(defmethod unparse-formatting ((data t) (type (eql :percentage)))
  (format nil "~a.blank? ? ~a : number_to_percentage((~a) * 100, precision: 2)"
          data (t.no-data) data))

;;;not nicely sorted but not needed yet and this hack works
(defmethod unparse-formatting ((data t) (type (eql :color)))
  (format nil "%><table><tr><td width=\"70px\" height=\"20px\" ~
               bgcolor=\"~a\"></td></tr></table><%=" (unparse-erb t data)))
;%><table><tr><td bgcolor=<%= @cactus.color %> width=70px height=20px ></td></tr></table><%=

(defmethod unparse-datatype ((type logical-type))
  (or (unparse-datatype (id type))
      (unparse-datatype (data-type type))))

(defun unparse-parent-field (ref-spec context-var)
  (let* ((field (cadr ref-spec))
         (relation (car ref-spec))
         (field-expr (unparse-attribute-references field relation context-var))
         (mandatory? (not (required-relation? relation))))
    (if mandatory?
        (let ((record-probe (format nil "~a.~a.blank?"
                                    context-var (instance-name relation))))
          (ruby:unparse-if-statement (as-literal record-probe) (as-literal (t.no-parent)) field-expr))
        (ruby:unparse-expression field-expr))))

;; unparse-attribute-references will be called by functions trying to write methods and
;; expressions for model class definitions (derived attributes and state predicates)
;; It finds attribute objects that may require some kind of special reference code and does
;; the necessary work to provide it as appropriate.
(defmethod unparse-attribute-references :before ((exp t) (context t) &optional obj-var)
  (declare (ignorable obj-var)))

(defmethod unparse-attribute-references ((exp t) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  (error "this one fell through the cracks: ~a - ~a" exp context))
(defmethod unparse-attribute-references ((exp operator) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp number) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp string) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp entity) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((obj entity-state) (context t) &optional obj-var)
  (unparse-attribute-references (expression (predicate obj)) context obj-var))
(defmethod unparse-attribute-references ((att attribute) (context attribute) &optional obj-var)
  (unparse-attribute-references att (my-entity context) obj-var))

;; not specializing on calculated-attribute as there will be a method with the attribute's
;; name and it can be directly referenced the same as a stored attribute
(defmethod unparse-attribute-references ((att attribute) (context entity) &optional obj-var)
  (if (eq (my-entity att) context)
      (as-literal (strcat (if obj-var (strcat obj-var ".") "") (schema-name att)))
      (error "unable to resolve attribute reference (~a, ~a) without a condition component" att context)))

(defmethod unparse-attribute-references ((att attribute) (context relation) &optional obj-var)
  (if (find att (attributes context))
      (as-literal
       (format nil "~a~a.~a"
               (if obj-var (strcat obj-var ".") "")
               (instance-name context) (schema-name att)))
      (error "unable to resolve attribute reference (~a, ~a)" att context)))

(defmethod unparse-attribute-references ((att attribute) (context entity) &optional obj-var)
  (let ((path (path-to-attribute context att)))
    (as-literal
     (format nil "~a~{~a~^.~}" (if obj-var (strcat obj-var ".") "")
             (mapcar #'instance-name path)))))

(defun reachable-field? (field context)
  (format t "~a ~a" field context)
  (or (eq context (my-entity field))
      (typep context 'relation)
      (and (typep context 'entity)
           (path-to-attribute context field))))

(defmethod unparse-attribute-references ((expr list) (context attribute) &optional obj-var)
  (unparse-attribute-references expr (my-entity context) obj-var))
(defmethod unparse-attribute-references ((expr list) (context t) &optional obj-var)
  (if (field-reference-expression? expr)
      ;; stuck here: if expr is 3 long there is a where expression with two contexts for attribute var names
      (if (= 2 (length expr))
          (progn
            (unless (reachable-field? (cadr expr) context)
              (error "unless an attribute is specified in the context of a relationship or is ~
                  otherwise reachable, there needs to be a conditional component for a ~
                  proper reference (~a - context: ~a)" expr context))
            (unparse-attribute-references (cadr expr) (car expr) obj-var))
          ;; we are here because this is a reference to an attribute in an unrelated model
          (as-literal (strcat (unparse-find-record (first expr) (cddr expr)) "."
                              (schema-name (cadr expr)))))
      (mapcar #'(lambda (ex)
                  (unparse-attribute-references ex context obj-var))
              expr)))

(defun unparse-find-record (entity conditions)
  (format nil "~a.find_by(~{~a~^, ~})" (model-name entity)
      (mapcar #'unparse-find-condition conditions)))

;; expecting only literal strings or standard operator expression (albeit simple ones)
(defmethod unparse-find-condition ((con string) &optional args)
  (when args (error "we should not have any args here. ~a ~a" con args))
  (format nil "'~a'" con))

(defmethod unparse-find-condition ((obj list) &optional args)
  (if (or (typep (car obj) 'operator) (operator-symbol? (car obj)))
      (progn
        (when args (error "we shouldn't have any args here...? (~a)" args))
        (ruby:unparse-expression (car obj) (cdr obj)))
      (unparse obj))) ;; not sure this UNPARSE will ever be appropriate...

(defmethod unparse-find-condition ((operator (eql '$eql)) &optional args)
  (format nil "~a: ~a" (ruby:unparse-expression (car args)) (ruby:unparse-expression (cadr args))))

                    
;(defvar conditions (cddr expr))

;ModelName.find_by(attribute-name: value)
;PhoneType.find_by name: n
;PhoneType.find_by("name = '#{n}'").id
;(defvar contexts (list (find-entity :parent) (find-entity :other)))
;(setf expr (find-field :index :other))
;; this is kind of a mirror to resolve-symbol-references in formulas.lisp
(defun unparse-where-clause-refs (expr &key obj-var context other-contexts)
    "go through an expression and determine if attributue objects need extra
     work to reference them in unparsing code given possibly multiple contexts"
  (cond
    ((null expr) nil)
    ((typep expr 'operator) expr)
    ((keywordp expr) expr)
    ((typep expr 'attribute)
     (let ((possible-context (remove-if-not
                  #'(lambda (e) (eq e (my-entity expr)))
                  (mapcar #'(lambda(c)
                      (if (typep c 'entity) c (entity c)))
                      (list* context other-contexts)))))
       (if (= 1 (length possible-context))
           (unparse-attribute-references expr (car possible-context))
           (let ((fmtstr (if (null possible-context)
                             "unable to reach attribute ~a in any of the given contexts: ~a"
                             "more than one possible route to ~a in these contexts: ~a")))
             (error fmtstr expr (list* context other-contexts))))))
    ((atom expr) expr) ;; numbers, strings
    ((field-reference-expression? expr) (unparse-attribute-references expr context))
    ((listp expr) (mapcar #'(lambda (item)
                  (apply #'unparse-where-clause-refs item (list* context other-contexts)))
              expr))
    (t (error "how did we get here?? ~a ~a" expr (list* context other-contexts)))))


(defmethod unparse-summary-attribute ((att summary-attribute))
  (ruby:unparse-method (schema-name att) nil
    (as-literal
     (format nil "self.~a.~a" (schema-name (car (path att))) (unparse-summary-method att)))))

(defmethod unparse-derived-attribute ((att calculated-attribute))
  (ruby:unparse-method (schema-name att) nil
       (unparse-attribute-references (expression att) (my-entity att))))

(defun extract-attributes-from-expression (exp &optional  context)
  (cond
    ((typep exp 'persistent-attribute)
     (let* ((model (or (and (typep context 'attribute) (my-entity context))
                       (and (typep context 'entity) context)
                       (and (typep context 'relation) (entity context)))))
       (when (or (null model) (eq model (my-entity exp)))
         exp)))
    ((listp exp) (mapcar #'(lambda (i) (extract-attributes-from-expression i context))
                         exp))
    (t nil)))

(defun vulnerable-to-null? (exp)
  (cond
    ((atom exp) nil)
    ((and (or (typep (car exp) 'operator) (operator-symbol? (car exp))) 
          (not (can-handle-null-args? (car exp))))
     (some #'(lambda(arg)
               (or (and (typep arg 'attribute) (nullable? arg))
                   (vulnerable-to-null? arg)))
           (cdr exp)))
    (t (some #'vulnerable-to-null? exp))))

(defun unparse-validation-method (method-name context test error-msg)
  (let* ((model-attributes (remove nil (flatten (extract-attributes-from-expression test context))))
         (nullable-atts (remove-if-not #'nullable? model-attributes)))
    (with-output-to-string (code)
      (format code "def ~a" method-name)
;; this <unless> nesting should only happen when a vulnerable operation will occur (or put the onus on callers?
      (when (vulnerable-to-null? test)
        (incf *nesting-level*)
	    (format code "~%~aunless ~{~a.blank?~^ || ~}" (make-indent)
                (mapcar #'ruby:unparse (remove-duplicates nullable-atts))))
	  (with-nesting
          (format code "~%~aif ~a" (make-indent)
                  (ruby:unparse-expression
                   (unparse-attribute-references test context)))
        (with-nesting
            (format code "~%~aerrors.add(:~a, ~s)"
                    (make-indent) (if (typep context 'attribute)
                                      (ruby:unparse context)
                                      (ruby:unparse (primary-key context)))
                    (or error-msg
                        (format nil "The check to ensure that ~a has failed"
                                (english:unparse-expression (ruby:negate-expression test))))))
        (format code "~%~aend" (make-indent)))
      (when (vulnerable-to-null? test)
        (format code "~%~aend" (make-indent))
	    (decf *nesting-level*))
      (format code "~%~aend" (make-indent)))))

(defun unparse-url-heirarchy (entity view &key (obj-ref :symbol) include-self?)
  (let ((fmt-str (ecase obj-ref
                   (:symbol "~a/:~a")
                   (:form-object "~a/#{form.object.~a}"))))
    (format nil "~{/~a~}~a"
            (let ((heirarchy (reverse (cons entity (path-to-root entity view)))))
              (loop for i from 1 to (1- (length heirarchy))
                    collect
                    (format nil fmt-str (schema-name (nth (1- i) heirarchy))
                            (strcat (snake-case
                                     (name 
                                      (lhs (find-if #'(lambda(r)
		                                                (and (eq (entity (lhs r)) (nth (1- i) heirarchy))
			                                                 (eq (entity (rhs r)) (nth i heirarchy))))
		                                            (relationships (nth i heirarchy))))))
                                    "_id"))))
            (if include-self?
                (format nil fmt-str (strcat "/" (schema-name entity)) (schema-name (primary-key entity)))
                ""))))

(defun unparse-form-object-id (aspect &optional (var "form.object"))
  (let* ((model (model-name (entity aspect)))
         (parent (model-name (owner (entity aspect)))))
    (format nil "~a<%= ~a.id %>" (replace-all model parent (snake-case parent))
            var)))

(defmethod unparse-namespace ((aspect aspect) (context (eql :path-method)))
  (let ((view-name (name (view aspect))))
    (if view-name (strcat (snake-case view-name) "_") "")))

(defmethod unparse-namespace ((aspect aspect) (context (eql :route)))
  (if (name (view aspect)) (strcat "/" (snake-case (name (view aspect)))) ""))

(defmethod unparse-namespace ((aspect aspect) (context (eql :file-path)))
  (if (name (view aspect)) (strcat (snake-case (name (view aspect))) "/") ""))

(defmethod unparse-namespace ((aspect aspect) (context (eql :list)))
  (let ((view-name (name (view aspect))))
    (if view-name (list (snake-case view-name)) nil)))

(defmethod unparse-namespace ((aspect aspect) (context (eql :array)))
  (let ((view-name (name (view aspect))))
    (if view-name (format nil ":~a, " (snake-case view-name)) "")))


'(defmethod unparse-template-item ((item cached-summary) model-var)
  (unparse-formatting (format nil "~a.~a" model-var (schema-name item))
                      (logical-type item)))
'(defmethod unparse-template-item ((item cached-calculation) model-var)
  (format nil "~a.~a" model-var (schema-name item)))
'(defmethod unparse-template-item ((item string) model-var)
  (declare (ignorable model-var))
  item)
'(defmethod unparse-template-item ((item formula) model-var)
  (ruby:unparse-expression
   (unparse-attribute-references
    (expression item) (context item) model-var)))
'(defmethod unparse-template-item ((item list) model-var)
  (if (field-reference-expression? item)
      (unparse-parent-field item model-var)
      (mapcar #'(lambda (elt) (unparse-template-item elt model-var))
              item)))
'(defmethod unparse-template-item ((item multi-valued-attribute) model-var)
  (warn "multi-valued attributes are not implemented yet in unparse-template-item")
  (format nil "'~a.~a'" model-var (schema-name item)))

'(defmethod unparse-template-item ((item attribute) model-var)
  (unparse-formatting (format nil "~a.~a" (or model-var (strcat "@" (instance-name (my-entity item))))
                              (schema-name item))
                      (logical-type item)))

'(defmethod unparse-template-item ((item summary-attribute) model-var)
  (unparse-formatting
   (format nil "~a.~a.~a" (or model-var (strcat "@" (instance-name (my-entity item)))) 
           (instance-name (car (path item)))
           (unparse-summary-method item))
   (logical-type item)))


'(defmethod unparse-detail-item ((item multi-valued-attribute) &optional var)
  (declare (ignorable var))
  (let ((entity (child-entity item))
        (actions nil))
    (format nil "~a~%~a~%~a"
            (indent-block nil (data-list-element (ui-label entity :list)
                                    :colspan (+ (length actions) (length (user-attributes entity)))))
                          (indent-block nil (data-list-row (list-column-headings entity (length actions))))
            (indent-block nil (list-body entity actions)))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
