(in-package #:services)

(defun general_import_service.rb (&optional stream)
  (let ((file (merge-pathnames
               (make-pathname :name "general_import_service"
                              :type "rb")
               (service-directory)))
        (*nesting-level* 0)
	    (out nil))
    (with-open-file (service-file file :direction :output :if-exists :supersede)
      (if stream
	      (setf out stream)
	      (progn
	        (setf out service-file)
	        (format-file-notice service-file "general_import_service.rb")))
      (format out "~&class GeneralImportService < ApplicationService~%")
      (let ((*nesting-level* 1))
	    (terpri out)
        (format out "
  def initialize(source, tenant_id: nil, file_format: :excel)
    @tenant = DBTenant.find_by(id: tenant_id) if tenant_id
    if source.is_a?(String)
      @file_name = source
      file_open_result = FileOpenService.call(source, file_format)
      file_object = file_open_result[:file]
      if file_object.nil?
        @error = open_file_result[:error]
      else
        if file_format.to_sym == :excel
          @header = file_object.map(&:itself).first
          # convert to an array without the header row
          @data = file_object.map(&:itself)[1..] rescue []
        else # no csv or other formats implemented just yet
          @error = I18n.t('services.upload.unknown_format')
        end
      end
    elsif source.is_a?(Array)
      @file_name = "-"
      @header = source.first
      @data   = source[1..]
    end
    # start with row 1 rather than 0 as row 0 is the headings
    @loaded_records = []
    @row_number = 1
  end
  
  def data = @data
  def error = @error
  def file_name = @file_name
  
  def call
    validate_header
    load_data() unless @error
    import_data() unless @error
    @error.nil? ? 'success' : @error
  end

  def validate_header
    unless @header == HEADER_COLUMNS
      @error = I18n.t('services.client_import_service.errors.invalid_column_headings', expected: HEADER_COLUMNS, received: @header)
    end
  end


	    (format out "~aself.abstract_class = true~%" (make-indent))
	    (db_import! out)
	    (check_deletable? out)
	    (polymorphic_reference out)  
	    (meta-data-class-methods out)
	    (terpri out)
	    (format out "~aclass GeneralImportServiceError > ApplicationServiceError; end~%" (make-indent)))
      (format out "~aend~%" (make-indent)))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
