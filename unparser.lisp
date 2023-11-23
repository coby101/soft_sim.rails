;;;===========================================================================
;;;
;;;   Code for uparsing general Ruby on Rails specific methods/syntax
;;;
;;;===========================================================================
 
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
  (ruby:unparse-data (data-type attribute) value))

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

(defmethod unparse-datatype ((type symbol))
  nil)
;; https://stackoverflow.com/questions/11889048/is-there-documentation-for-the-rails-column-types
(defmethod unparse-datatype ((sym (eql :string)))
  "string")
(defmethod unparse-datatype ((sym (eql :memo)))
  "text")
(defmethod unparse-datatype ((sym (eql :long-text)))
  "text")
(defmethod unparse-datatype ((sym (eql :short-text)))
  "string")
(defmethod unparse-datatype ((sym (eql :label)))
  "string")
(defmethod unparse-datatype ((sym (eql :name)))
  "string")
(defmethod unparse-datatype ((sym (eql :code)))
  "string")
(defmethod unparse-datatype ((sym (eql :datetime)))
  "timestamp")
(defmethod unparse-datatype ((sym (eql :integer)))
  "integer")
(defmethod unparse-datatype ((sym (eql :date)))
  "date")
(defmethod unparse-datatype ((sym (eql :boolean)))
  "boolean")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :money)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :sequence)))
  "integer")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")
(defmethod unparse-datatype ((sym (eql :float)))
  "decimal")

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
  (declare (ignorable obj-var))
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
                                (unparse-expression (negate-expression test) :english)))))
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

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
