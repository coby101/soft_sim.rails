;;;===========================================================================
;;; file:   generators/ror/styling.lisp
;;; auth:   Coby Beck
;;; date:   2021-11-09
;;; update: 
;;;---------------------------------------------------------------------------
;;;   Code for adding CSS class stylings to erb templates
;;;--------------------------------------------------------------------------- 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(defvar *nested-attribute-title-class* "nested-attribute-title")

(defvar *svg-delete-icon* "")
(setf *svg-delete-icon* "<%= inline_svg_tag \"icons/delete.svg\" %>")
(defvar *svg-view-icon* nil)
(setf *svg-view-icon* "<%= inline_svg_tag \"icons/view.svg\" %>")

(defvar *svg-edit-icon* "")
(setf *svg-edit-icon* "<%= inline_svg_tag \"icons/edit.svg\" %>")

(defvar *svg-list-icon* nil)
(setf *svg-list-icon* "<%= inline_svg_tag \"icons/back.svg\" %>")

(defvar *svg-add-icon* nil)
(setf *svg-add-icon* "<%= inline_svg_tag \"icons/add.svg\" %>")

(defvar *error-list-style* "list-style: square")
(defvar *user-error-feedback-style* "
    background-color: #FFCCBA;
    background-image: url(https://i.imgur.com/GnyDvKN.png);
    border: 1px solid;
    align: left
    margin: 10px auto;
    padding: 15px 10px 15px 50px;
    background-repeat: no-repeat;
    max-width: 900px;")
(defvar *simple-table-layouts?* nil
  "this switch will determine if templates generate with ~
   divs and CSS classes or basic HTML tables")
(defvar *justify-grid-layouts?* nil
  "this parameter controles whether grid layouts are styled with variable ~
   row-lengths per line or just variable colspans per element")

(defvar *crud-page-title-class* "pcmdcrud-index-header")
(defvar *datagrid-title* "pcmdcrud-index-header")
(defvar *datasheet-title* "pcmdcrud-index-header")
(defvar *datasheet-class* "datasheet")
(defvar *record-context-class* "record-context")
(defvar *record-context-section-class* "record-context-section")
(defvar *datasheet-content-class* "datasheet-main-content")
(defvar *datasheet-line-class* "datasheet-line")
(defvar *datasheet-element-class* "datasheet-element")

(defvar *update-form-class* (strcat *datasheet-class* " update-form"))
(defvar *update-form-line-class* "update-form-line")
(defvar *update-form-element-class* "update-form-element")

(defvar *data-listing-class* "datagrid")
(defvar *list-body-class* "datagrid-body")
(defvar *list-row-class* "datagrid-row")
(defvar *list-element-class* "datagrid-column")
(defvar *heading-row-class* (strcat *list-row-class* " datagrid-heading-row"))
(defvar *heading-element-class* "datagrid-column-heading")

(defvar *nested-form-class* "nested-fields")
(defvar *default-form-field-class* "field")

(defvar *form-submit-button-class* "action")
(defvar *delete-button-class* "crud-op crud-op-delete btn")
(defvar *delete-nested-attribute-class* "remove_fields btn")
(defvar *add-nested-attribute-class* "add_fields btn")
(defvar *link-button-class* "pcmdcrud-show-icon")
(defvar *default-form-label-class* "form-label")
(defvar *default-form-field-class* "form-field")
(defvar *default-form-collection-class* "form-collection")
(defvar *default-form-element-class* "form-element")


(defmethod form-element-class ((item t)) *default-form-element-class*)
(defmethod form-label-class ((item t)) *default-form-label-class*)
(defmethod form-field-class ((item t)) *default-form-field-class*)

(defvar *default-field-value-class* "field-value")
(defvar *default-field-label-class* "field-label")
(defvar *default-text-input-class* "text-input")
(defmethod text-input-class ((item t)) *default-text-input-class*)

(defvar *default-text-area-class* "text-area")
(defmethod text-area-class ((item t)) *default-text-input-class*)

(defvar *default-select-box-class* "select-box-class")

(defmethod field-value-class ((item t))
  *default-field-value-class*)
(defmethod field-label-class ((item t))
  *default-field-label-class*)

(defun datasheet-line-class (&optional colspan)
  (class-with-span *datasheet-line-class* colspan))
(defun datasheet-element-class (&optional colspan)
  (class-with-span *datasheet-element-class* colspan))
(defun list-element-class (&optional colspan)
  (class-with-span *list-element-class* colspan))

(defun class-with-span (class span &optional (span-type "col"))
  (when (and span (not (integerp span))) (error "~aspan must be an integer (~a)" span-type span))
  (when (and span (> span 12)) (error "there is no class for higher than 12 ~aspan" span-type))
  (when (and span (< span 1)) (error "~aspan less than 1 does not make sense" span-type))
  (strcat class (if (and span (not (= 1 span))) (format nil "-~a" span) "")))
  
(defun heading-element-class (&optional colspan)
  (strcat (list-element-class colspan)
          " " *heading-element-class*))

(defun update-form-element-class (&optional colspan)
  (strcat (datasheet-element-class colspan)
          " " *update-form-element-class*))

(defun update-form-line-class (&optional cols)
  (strcat (datasheet-line-class cols)
          " " *update-form-line-class*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  data context form styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-data-context (sections &optional id)
  (format nil "~a~%~{~a~%~}~%~a"
          (data-context.open id)
          (with-nesting (mapcar #'data-context-section sections))
          (data-context.close)))
(defun data-context.open (&optional id)
  (if *simple-table-layouts?*
      (html::open-tag "table")
      (apply #'html::open-tag "div" (append (list :class *record-context-class*) (when id (list :id id))))))
(defun data-context.close ()
  (if *simple-table-layouts?*
      (html::close-tag "table")
      (html::close-tag "div")))

(defmethod data-context-section ((rows list))
  (format nil "~a~%~{~a~%~}~a"
          (data-context-section.open)
          (with-nesting (mapcar #'datasheet-line rows))
          (data-context-section.close)))

(defmethod data-context-section ((rows string))
  (format nil "~a~%~a~%~a"
          (data-context-section.open)
          (indent-block nil rows)
          (data-context-section.close)))

(defun data-context-section.open ()
  (if *simple-table-layouts?*
      (html::open-tag "table" :border 1)
      (html::open-tag "div" :class *record-context-section-class*)))
(defun data-context-section.close ()
  (if *simple-table-layouts?*
      (html::close-tag "table")
      (html::close-tag "div")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  update/new form styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-update-form (rows &optional id)
  (let ((screen-cols (calculate-screen-denominator rows)))
    (format nil "~a~%~{~a~%~}~%~a"
            (update-form.open id)
            (with-nesting
                (mapcar #'(lambda (row)
                            (update-form-line row screen-cols))
                        rows))
            (update-form.close))))
(defun update-form.open (&optional id)
  (if *simple-table-layouts?*
      (html::open-tag "table")
      (apply #'html::open-tag "div" (append (list :class *update-form-class*) (when id (list :id id))))))
(defun update-form.close ()
  (if *simple-table-layouts?*
      (html::close-tag "table")
      (html::close-tag "div")))

(defmethod update-form-line ((items string) &optional panel-width)
  (format nil "~a~%~a~%~a"
          (update-form-line.open)
          items
          (update-form-line.close)))
(defmethod update-form-line ((items list) &optional panel-width)
  (format nil "~a~%~{~a~%~}~a"
          (update-form-line.open (or panel-width (length items)))
          (with-nesting
              (mapcar #'(lambda (item)
                          (update-form-item item
                             :colspan (calculate-item-width item (length items) (or panel-width (length items)))))
                      items))
          (update-form-line.close)))
(defun update-form-line.open (&optional length)
  (if *simple-table-layouts?*
      (html::open-tag "tr")
      (html::open-tag "div" :class (update-form-line-class length))))
(defun update-form-line.close ()
  (if *simple-table-layouts?* (html::close-tag "tr") (html::close-tag "div")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update form ITEM
(defmethod update-form-item ((element t) &key id colspan rowspan)
  (format nil "~a~%~a~%~a"
          (update-form-item.open :id id :rowspan rowspan :colspan colspan)
          (indent-block nil (unparse-form-element element)) (update-form-item.close)))

(defun update-form-item.open (&key id colspan rowspan)
  (if *simple-table-layouts?*
      (apply #'html::open-tag "td" (append (when colspan (list :colspan colspan))
                                           (when rowspan (list :rowspan rowspan))))
      ;; missing the logic to use colspan and rowspan
      (apply #'html::open-tag "div" (append (list :class (update-form-element-class colspan))
                                            (when id (list :id id))))))

(defun update-form-item.close ()
  (if *simple-table-layouts?* (html::close-tag "td") (html::close-tag "div")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  show page styling
;;;;;
;;;;;   - format-datasheet
;;;;;     - datasheet-section
;;;;;        - datasheet-line
;;;;;          - datasheet-element
;;;;;          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-datasheet (context content &optional id)
  (format nil "~a~%~a~%~a~%~a"
          (datasheet.open id)
          (if context
              (with-nesting (datasheet-section context *record-context-class*))
              "")
          (with-nesting (datasheet-section content *datasheet-content-class*))
          (datasheet.close)))
(defun datasheet.open (&optional id)
  (if *simple-table-layouts?*
      (html::open-tag "p")
      (html::open-tag "div" :class *datasheet-class*)))
(defun datasheet.close ()
  (if *simple-table-layouts?*
      (html::close-tag "p")
      (html::close-tag "div")))

(defmethod datasheet-section ((rows list) class)
  (let ((screen-cols (calculate-screen-denominator rows)))
    (format nil "~a~%~{~a~%~}~a"
            (datasheet-section.open class)
            (with-nesting (mapcar #'(lambda(row)
                                      (datasheet-line row screen-cols))
                                  rows))
            (datasheet-section.close))))

(defmethod datasheet-section ((rows string) class)
  (format nil "~a~%~a~%~a"
          (datasheet-section.open class)
          (indent-block nil rows)
          (datasheet-section.close)))

(defun datasheet-section.open (class)
  (if *simple-table-layouts?*
      (html::open-tag "table" :border 1)
      (html::open-tag "div" :class class)))
(defun datasheet-section.close ()
  (if *simple-table-layouts?*
      (html::close-tag "table")
      (html::close-tag "div")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; datasheet LINE
(defmethod datasheet-line ((items list) &optional panel-width)
  (format nil "~a~%~{~a~%~}~a"
          (datasheet-line.open (length items))
          (with-nesting
              (mapcar #'(lambda(item)
                          (datasheet-item item
                             :colspan (calculate-item-width item (length items) panel-width)))
                      items))
          (datasheet-line.close)))

(defmethod datasheet-line ((items string) &optional panel-width)
  (format nil "~a~%~a~%~a"
          (datasheet-line.open)
          (indent-block nil items)
          (datasheet-line.close)))

(defun datasheet-line.open (&optional length)
  (if *simple-table-layouts?*
      (html::open-tag "tr")
      (html::open-tag "div" :class (datasheet-line-class length))))

(defun datasheet-line.close ()
  (if *simple-table-layouts?* (html::close-tag "tr") (html::close-tag "div")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; datasheet ITEM
(defmethod datasheet-item ((element string) &key id colspan rowspan)
  (strcat (datasheet-item.open :id id :rowspan rowspan :colspan colspan)
          element
          (datasheet-item.close)))

(defun datasheet-item.open (&key id colspan rowspan)
  (if *simple-table-layouts?*
      (apply #'html::open-tag "td" (append (when colspan (list :colspan colspan))
                                           (when rowspan (list :rowspan rowspan))))
      ;; missing the logic to use colspan and rowspan
      (apply #'html::open-tag "div" (append (list :class (datasheet-element-class colspan))
                                            (when id (list :id id))))))
(defun datasheet-item.close ()
  (if *simple-table-layouts?* "</td>" "</div>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Index page styling
;;;
;;;    - format-data-listing
;;;      - data-list-headings
;;;        - data-list-heading
;;;      - data-list-body
;;;        - data-list-rows
;;;          - data-list-row
;;;            - data-list-element
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-data-listing (header content &optional id)
  (format nil "~a~%~a~%~a~%~a"
          (data-list.open id)
          (with-nesting (data-list-headings header))
          (with-nesting (data-list-body content))
          (data-list.close)))
(defun data-list.open (&optional id)
  (if *simple-table-layouts?*
      (html::open-tag "table" :border 1 :cellpadding 5)
      (apply #'html::open-tag "div"
             (append (list :class *data-listing-class*) (when id (list :id id))))))
(defun data-list.close ()
  (if *simple-table-layouts?*
      (html::close-tag "table")
      (html::close-tag "div")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data listing HEADINGS ROW
(defmethod data-list-headings ((headings list) &optional id)
  (let ((fmt-str (with-nesting (format nil "~~a~~a~~{~%~a~~a~~}~%~~a~~a" (html::make-indent)))))
    (format nil fmt-str
            (html::make-indent)
            (data-list-headings.open id)
             (mapcar #'data-list-heading headings)
            (html::make-indent)
            (data-list-headings.close))))

(defun data-list-headings.open (&optional id)
  (if *simple-table-layouts?*
      (format nil "~a~%~a" (html::open-tag "thead") (html::open-tag "tr"))
      (apply #'html::open-tag "div" (append (list :class *heading-row-class*)
                                            (when id (list :id id))))))
(defun data-list-headings.close ()
  (if *simple-table-layouts?*
      (format nil "~a~%~a" (html::close-tag "tr") (html::close-tag "thead"))
      (html::close-tag "div")))

(defmethod data-list-headings ((headings string) &optional id)
  (strcat (data-list-headings.open id) headings (data-list-headings.close)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data listing BODY
(defmethod data-list-body ((body string) &optional id)
  (format nil "~a~%~a~%~a" (data-list-body.open id)
          (with-nesting (data-list-rows body))
          (data-list-body.close)))
(defmethod data-list-body ((body list) &optional id)
  (format nil "~a~a~%~{~a~%~}~%~a~a" (html::make-indent)
          (data-list-body.open id)
          (with-nesting (with-nesting (data-list-rows body)))
          (html::make-indent) (data-list-body.close)))
(defun data-list-body.open (&optional id)
  (if *simple-table-layouts?*
      (html::open-tag "tbody")
      (apply #'html::open-tag "div"
             (append (list :class *list-body-class*) (when id (list :id id))))))
(defun data-list-body.close ()
  (if *simple-table-layouts?*
      (html::close-tag "tbody")
      (html::close-tag "div")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data listing ROWS
(defmethod data-list-rows ((rows string)) rows)
(defmethod data-list-rows ((rows list))
;  (let ((screen-cols (calculate-screen-denominator rows)))
    (mapcar #'(lambda(row)
                (data-list-row row))
            rows));)

(defun data-list-row.open (&optional id)
  (if *simple-table-layouts?*
      (html::open-tag "tr")
      (apply #'html::open-tag "div"
             (append (list :class *list-row-class*)
                     (when id (list :id id))))))

(defun data-list-row.close ()
  (if *simple-table-layouts?* (html::close-tag "tr") (html::close-tag "div")))

(defmethod data-list-row ((row string) &optional id)
  (strcat (data-list-row.open id) row (data-list-row.close)))

(defmethod data-list-row ((row list) &optional id)
  (format nil "~a~{~%~a~}~%~a"
            (data-list-row.open id)
            (with-nesting (mapcar #'data-list-element row))
            (data-list-row.close)))

;:colspan (calculate-item-width item row-length panel-width)

(defmethod data-list-row ((row string) &optional id)
  (strcat (data-list-row.open id) row (data-list-row.close)))
(defun data-list-row.open (&optional id)
  (if *simple-table-layouts?*
      (html::open-tag "tr")
      (apply #'html::open-tag "div"
             (append (list :class *list-row-class*)
                     (when id (list :id id))))))
(defun data-list-row.close ()
  (if *simple-table-layouts?* (html::close-tag "tr") (html::close-tag "div")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data listing HEADING ELEMENT
(defun data-list-heading.open (&key id colspan rowspan)
  (if *simple-table-layouts?*
      (apply #'html::open-tag "th" 
             (append (when colspan (list :colspan colspan))
                     (when rowspan (list :rowspan rowspan))))
      (apply #'html::open-tag "div"
             (append (list :class (heading-element-class colspan))
                     (when id (list :id id))))))

(defun data-list-heading.close ()
  (if *simple-table-layouts?* (html::close-tag "th") (html::close-tag "div")))

(defmethod data-list-heading ((heading string) &key id colspan rowspan)
  (strcat (data-list-heading.open :id id :rowspan rowspan :colspan colspan)
          heading
          (data-list-heading.close)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data listing DATA ELEMENT
(defun data-list-element.open (&key id colspan rowspan)
  (if *simple-table-layouts?*
      (apply #'html::open-tag "td" (append (when colspan (list :colspan colspan))
                                           (when rowspan (list :rowspan rowspan))))
      (apply #'html::open-tag "div" (append (list :class (list-element-class colspan))
                                            (when id (list :id id))))))

(defun data-list-element.close () (if *simple-table-layouts?* "</td>" "</div>"))

(defmethod data-list-element ((element string) &key id colspan rowspan)
  (strcat (data-list-element.open :id id :rowspan rowspan :colspan colspan)
          element
          (data-list-element.close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; these methods take an item (eg name field, date field, notes field), the number of
;; page columns the screen is divided into and the number of elements sharing this row
;; and returns a col-span number
(defmethod calculate-item-width ((item list) row-cols max-cols)
  (if (field-reference-expression? item)
      (calculate-item-width (cadr item) row-cols max-cols)
      (progn
        (warn "trouble in calculate-item-width...? ~a" item)
        1)))

(defmethod calculate-item-width ((item attribute) row-elements page-denominator)
  (let ((ideal-share (ideal-screen-share item)))
    (cond
      ((= 1 row-elements) (ceiling (* ideal-share page-denominator)))
      ((> row-elements 6) (floor (* ideal-share page-denominator)))
      (t (max 1 (round (* ideal-share page-denominator)))))))

(defmethod calculate-item-width ((item string) row-elements page-denominator)
  (if (= row-elements 1)
      page-denominator
      (1+ (- page-denominator row-elements))))

(defmethod calculate-item-width ((item multi-valued-attribute) row-elements page-denominator)
  (min page-denominator
       (apply #'+ (mapcar #'(lambda (att)
                              (calculate-item-width att row-elements page-denominator))
                          (user-attributes (child-entity item))))))

(defun max-length (list)
  (apply #'max
         (mapcar #'(lambda (row)
                     (if (listp row)
                         (length row)
                         1))
                 list)))

(defun calculate-screen-denominator (rows)
  (let ((max-elements (max-length rows)))
    (cond
      ((> max-elements 6) max-elements)
      ((> max-elements 4) (* 2 max-elements))
      (t 12))))


(defun translate-long-name (view)
  (format nil "t('~a.long_name')" (snake-case (name view))))

(defun crud-page-heading(&optional (text "<%= t('.title') %>"))
  (html::heading 1 text :class *crud-page-title-class*))

(defmethod ui-label ((obj aspect) (action (eql :list)) &optional id)
  (let ((heading (crud-page-heading)))
    (if *simple-table-layouts?*
        heading
        (apply #'html::div heading
               (append (list :class *datagrid-title*)
                       (when id (list :id id)))))))

(defmethod ui-label ((obj aspect) (action (eql :new)) &optional id)
  (let ((heading (crud-page-heading)))
    (if *simple-table-layouts?*
        heading
        (apply #'html::div heading
               (append (list :class *datagrid-title*)
                       (when id (list :id id)))))))

(defmethod ui-label ((obj aspect) (action (eql :detail)) &optional id)
  (let ((heading (crud-page-heading)))
    (if *simple-table-layouts?*
        heading
        (apply #'html::div heading
               (append (list :class *datasheet-title*)
                       (when id (list :id id)))))))

(defmethod ui-label ((obj attribute-table) (action (eql :list)) &optional id)
  (let ((text (format nil "t('schema.~a.long_plural')" (schema-name obj))))
    (if *simple-table-layouts?*
        (html::heading 3 text :class *nested-attribute-title-class*)
        (apply #'html::div (html::heading 3 text)
               (append (list :class *datasheet-title*)
                       (when id (list :id id)))))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
