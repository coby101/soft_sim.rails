;;;===========================================================================
;;;
;;;   Code for generating application_helper.rb
;;;
;;;    NB: need a new approach, leave application_helper.rb to manual development
;;;
;;;===========================================================================

(in-package ror)

(defun link_with_external_link ()
  "
  def link_with_external_link(title, link, target = \"_blank\", css_class = \"text-blue-600 inline align-middle\")
    link = <<-HTML
      #{title}
      <a href=\"#{link}\" target=\"#{target}\">
        <svg xmlns=\"http://www.w3.org/2000/svg\" class=\"h-4 w-4 #{css_class}\" fill=\"none\" viewBox=\"0 0 24 24\" stroke=\"currentColor\">
          <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14\" />
        </svg>
      </a>
    HTML

    link.html_safe
  end

  def model_error_messages(model, class_name = nil)
    if model.errors.any?

      errors = \"\"
      model.errors.each do |error|
        errors += \"<p> #{error.full_message} </p>\"
      end

      error_message = <<-HTML
        <div class=\"border-2 border-red-900 text-red-900 rounded relative mb-4 pcmd-flash-message\" role=\"alert\">
          <div class=\"bg-red-800 px-4 py-3\">
          <h2> #{pluralize(model.errors.count, \"error\")} prevented this #{class_name.nil? ? model.class : class_name} from saving:</h2>
          #{errors}
          </div>
        </div>
      HTML

      error_message.html_safe
    end
  end

  def display_error_flash(alert)
    if alert.class != String
      errors = \"\"
      alert[\"base\"].each do |error|
        errors += \"<p> #{error} </p>\"
      end
      errors.html_safe
    else
      alert
    end
  end
")

(defun menu-level-methods ()
  "
  def first_level_active_menu(menu = 'framework')
    controller_name == menu ? 'active' : ''
  end

  def second_level_active_menu(parent_menu, child_menu)
    controller_name == parent_menu && action_name == child_menu ? 'active' : ''
  end
")

(defun link_to_add_fields()
  (format nil "
  def link_to_add_fields(name, form, association)~%    ~a~%    ~a~%    ~a
               ~a~%             end~%    ~a~%  end~%"
;;; Takes an object (@company) and creates a new instance of its associated model (:employees)
;;; To better understand, run the following in your terminal:
;;; rails c --sandbox
;;; @company = Company.new
;;; new_object = @company.send(:employees).klass.new
    "new_object = form.object.send(association).klass.new"
;;; Saves the unique ID of the object into a variable.
;;; This is needed to ensure the key of the associated array is unique. This is makes parsing the
;;; content in the `data-fields` attribute easier through Javascript.
;;; We could use another method to achieve this.
    "id = new_object.object_id"
;;; https://api.rubyonrails.org/ fields_for(record_name, record_object = nil, fields_options = {}, &block)
;;; record_name = :employees
;;; record_object = new_object
;;; fields_options = { child_index: id }
;;; child_index` is used to ensure the key of the associated array is unique, and that it matched
;;; the value in the `data-id` attribute. `company[employees_attributes][child_index_value][_destroy]`
    "fields = form.fields_for(association, new_object, child_index: id) do |builder|"
;;; `association.to_s.singularize + "_fields"` ends up evaluating to `employee_fields`
;;; The render function will then look for `views/company/_employee_fields.html.erb`
;;; The render function also needs to be passed the value of 'builder', because
;;; `views/company/_employee_fields.html.erb` needs this to render the form tags.
    "render(association.to_s.singularize + \"_fields\", form: builder)"
;;; This renders a simple link, but passes information into `data` attributes.
;;; This info can be named anything we want, but in this case we chose `data-id:` and `data-fields:`.
;;; The `id:` is from `new_object.object_id`.
;;; The `fields:` are rendered from the `fields` blocks.
;;; We use `gsub("\n", "")` to remove anywhite space from the rendered partial.
;;; The `id:` value needs to match the value used in `child_index: id`.
    (format nil "link_to(name, '#', class: ~s, data: {id: id, fields: fields.gsub(\"\\n\", \"\")})"
            *add-nested-attribute-class*)
;;    "def remove_child_button(name)
;;    content_tag(:button,\"Remove\".html_safe,
;;      class: \"remove_fields\")
;;  "end"
))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
