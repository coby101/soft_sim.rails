;;;===========================================================================
;;;
;;;   code for creating application_record.rb and the instance and class methods
;;;   relied on by other generated components 
;;;
;;;===========================================================================

(in-package #:model)

(defun check_deletable?(&optional (stream t))
  (indent-block stream "
# overridden in many model files as a pre-check to alert users that a particular record
# will fail validation if a destroy is attempted, due to existing dependent records
def check_deleteable?
  true
end
"))

(defun db_import!(&optional (stream t))
  (indent-block stream "
# used in seeds.rb so seeds can be reloaded during development without creating duplicate records
def self.db_import!(data)
  return true unless data.present?
  data.each do |atts|
    self.find_or_create_by!(atts) unless atts[:id].present? && self.where(id: atts[:id]).present?
  end
end
"))

(defun polymorphic_reference(&optional (stream t))
  (indent-block stream "
# used for forms and controllers to unambiguously identify records involved in polymorphic relationships
def polymorphic_reference
  \"#{self.class.name}:#{self.id}\"
end
"))

(defun meta-data-class-methods(&optional (stream t))
  (format stream "~aclass BadMetaData < StandardError; end~%" (make-indent))
  (indent-block stream "
# these methods allow for a great deal of abstraction in view helpers and general services as 
# object.class can be interrogated for data relevant to alot of formating and behaviour
class << self

  def attributes
    base_data = self.meta_data || {}
    add_shared_attributes(base_data)
    base_data[:attributes]
  end

  def children    = meta_data[:children]
  def parents      = meta_data[:parents]
  def auditable?    = meta_data[:keep_history?]
  def select_text    = meta_data[:select_display_method] || select_value
  def select_value    = meta_data[:select_value] || :id
  def foreign_keys     = parents&.flatten&.map { |c| (c.name.underscore + '_id').to_sym } || []
  def user_managed?     = !meta_data[:read_only?]
  def tenant_scoped?     = (parents || []).include?(DBTenant)
  def list_attributes     = attributes.keys
  def default_sort_method  = meta_data[:sort_by] || :id

  def attribute_meta_data(key) = (attributes[key] || {})
  def attribute_property(attribute, property) = attribute_meta_data(attribute)[property] rescue nil

  def nullable?(attribute)  = data_type(attribute) == :boolean || attribute_property(attribute, :nullable?)
  def use_type(attribute)    = attribute_property(attribute, :logical_type)
  def data_type(attribute)    = attribute_domain(attribute)[:data_type]
  def domain_type(attribute)   = attribute_domain(attribute)[:domain_type]
  def data_source(attribute)    = attribute_domain(attribute)[:source]
  def is_foreign_key?(attribute) = attribute.in?(foreign_keys)
  def attribute_domain(attribute) = attribute_property(attribute, :domain) || {}

  # default attribute values can be:
  #  - literal data (eg. 'current' or 100)
  #  - a reference to another attribute or a specific method to call on an object
  #  - a Proc object that will be passed a class instance
  def default_value(attribute, instance = nil)
    spec = attribute_property(attribute, :default)
    if    spec.class == Proc   then instance && spec.call(instance)
    elsif spec.class == Symbol then instance.send(spec)
    else  spec
    end
  end

  # if a numerical or date attribute was defined with a constrained value range, form templates
  # will use these minimum_ and maximum_value methods to similarily constrain user interfaces
  # as with default_value, the property value can be literal data, a method, or a Proc
  def minimum_value(attribute, instance = nil)
    spec = attribute_property(attribute, :minimum_value)
    if    spec.class == Proc   then instance && spec.call(instance)
    elsif spec.class == Symbol then instance.send(spec)
    else  spec
    end
  end

  def maximum_value(attribute, instance = nil)
    spec = attribute_property(attribute, :maximum_value)
    if    spec.class == Proc   then instance && spec.call(instance)
    elsif spec.class == Symbol then instance.send(spec)
    else  spec
    end
  end

  # for referential enumerations, attribute_select_display_method and attribute_select_value_method
  # provide form helpers with data needed to properly populate a select box field
  def attribute_select_display_method(attribute)
    source = attribute_domain(attribute)[:source]
    if source.first.is_a?(Class)
      source.first.select_text
    else # polymorphic reference
      methods = source.map(&:first).map(&:select_text).uniq
      if methods.size == 1
        return methods.first
      else
        raise BadMetaData.new \"on the #{self.name} model, the :source property of #{attribute} is polymorphic and the potential parents have differing select_text methods\"
      end
    end
  end

  def attribute_select_value_method(attribute)
    source = attribute_domain(attribute)[:source]
    if source.first.is_a?(Class)
      source.second
    else # polymorphic reference
      methods = source.map(&:second).uniq
      if methods.size == 1
        return methods.first
      else
        raise BadMetaData.new \"on the #{self.name} model, the :source property of #{attribute} is not configured correctly\"
      end
    end
  end

  def add_shared_attributes(base_data)
    # all concerns that specify shared meta data via 'def self.derived_attributes'
    # must be included in this 'concerns' array. The approach below makes assumptions
    # about how concerns are named, assuming no colons. This is to eliminate
    # native rails modules and this approach may not be very robust.  BEWARE
    concerns = included_modules.select { |mod| mod.to_s.exclude?(':') && mod != Kernel }
    base_data[:attributes] = {} if base_data[:attributes].nil?
    # visit each concern and add any shared attributes
    concerns.each do |module|
      shared = module.derived_attributes rescue {}
      shared.each do |att, properties|
        base_data[:attributes][att] = properties
      end
    end
    base_data
  end
end
"))

(defun application_record.rb (&optional stream)
  (let ((file (merge-pathnames
               (make-pathname :name "application_record"
                              :type "rb")
               (model-directory)))
        (*nesting-level* 0)
	    (out nil))
    (with-open-file (mod-file file :direction :output :if-exists :supersede)
      (if stream
	      (setf out stream)
	      (progn
	        (setf out mod-file)
	        (format-file-notice mod-file "application_record.rb")))
      (format out "~&class ApplicationRecord < ActiveRecord::Base~%")
      (let ((*nesting-level* 1))
	    (terpri out)
	    (format out "~aself.abstract_class = true~%" (make-indent))
	    (db_import! out)
	    (check_deletable? out)
	    (polymorphic_reference out)  
	    (meta-data-class-methods out)
	    (terpri out))
      (format out "~aend~%" (make-indent)))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
