;;;====================================================
;;;
;;;   Code for unparsing in the route and erb context
;;;
;;;====================================================
 
(in-package #:rails-unparser)

(defmethod unparse-erb (output? (code string))
  (format nil "<%~a ~a %>" (if output? "=" "") code))

(defmethod unparse-erb (output? (obj t))
  (unparse-erb output? (unparse obj :ruby)))

(defmethod unparse-erb (output? (obj list))
  (if (or (typep (car obj) 'operator) (operator-symbol? (car obj)))
      (unparse-erb output? (unparse-expression (car obj) :ruby (cdr obj)))
      (unparse-expression (unparse obj :ruby) :ruby output?)))

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

(defun unparse-parent-field (ref-spec context-var)
  (let* ((field (cdr ref-spec))
         (relation (car ref-spec))
         (field-expr (unparse-attribute-reference field relation context-var))
         (mandatory? (not (required-relation? relation))))
    (if mandatory?
        (let ((record-probe (format nil "~a.~a.blank?"
                                    context-var (instance-name relation))))
          (ruby:unparse-if-statement (as-literal record-probe) (as-literal (t.no-parent)) field-expr))
        (unparse-expression field-expr :ruby))))

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
                (unparse item :english) path-up (mapcar #'schema-name path-down)))
        (format nil "~a~{.~a~}" path-up (mapcar #'schema-name path-down)))))

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

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
