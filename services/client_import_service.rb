class ClientImportService < ApplicationService
  HEADER_COLUMNS = [I18n.t('activerecord.attributes.client.name.long_name'),
                    I18n.t('activerecord.attributes.client.email.long_name'),
                    I18n.t('activerecord.attributes.client.address1.long_name'),
                    I18n.t('activerecord.attributes.client.address.long_name'),
                    I18n.t('activerecord.attributes.client.locality.long_name'),
                    I18n.t('activerecord.attributes.client.state.long_name'),
                    I18n.t('activerecord.attributes.client.post_code.long_name')]
  DATA_TYPES = [:name, :email, :short_text, :short_text, :short_text, :short_text, :post_code]

  def load_spreadsheet()
    if @data.blank?
      @error = I18n.t('services.client_import_service.errors.no_data', file: @file_name)
    else
      @data.each do |row|
        break if @error
        @row_number += 1
        validated_record = validate_record(row)
        @loaded_records << validated_record if validated_record
      end
    end
    @loaded_records
  end

  def validate_record
    

  def import_data()
    schedule = @project.tenant.schedules.find_by(code: @schedule_code)
    scope_controller = ScopeType.find_by(name: 'original scope')
    if schedule.nil?
      @error = I18n.t('services.client_import_service.errors.no_schedule', code: @schedule_code)
      return @error
    end
      @budget[:parts].each do |part|
        project_part = @project.project_parts.find_or_initialize_by(code: part[:code])
        if project_part.persisted?
          unless part[:name] == project_part.name
            @error = I18n.t('services.client_import_service.errors.conflicting_part_data', data: 'Name', code: part[:code])
            return @error
          end
        elsif !project_part.update(name: part[:name], scope_controller: scope_controller)
          @error = I18n.t('services.client_import_service.errors.save_part', code: part[:code])
          return @error
        end
        part[:items].each do |item|
          schedule_item = schedule.schedule_items.find_by(code: item[:schedule])
          if schedule_item.nil?
            @error = I18n.t('services.client_import_service.errors.no_schedule', code: "#{@schedule_code} - #{item[:schedule]}")
            return @error
          end
          work_item = project_part.work_items.find_or_initialize_by(code: item[:code])
          if work_item.persisted?
            translator = 'services.client_import_service.errors.conflicting_item_data'
            @error = I18n.t(translator, item: item[:code], part: part[:code], data: 'Schedule Code') unless work_item.schedule_item_code.casecmp(item[:schedule]) == 0 
            @error = I18n.t(translator, item: item[:code], part: part[:code], data: 'Description') unless work_item.description.casecmp(item[:description]) == 0 
            return @error if @error
          elsif !work_item.update(description: item[:description], quantity_expected: -1, schedule_item: schedule_item)
            @error = I18n.t('services.client_import_service.errors.save_item', part: part[:code], item: item[:code])
            return @error
          end
          item[:estimates].each do |est|
            cost_code = @project.tenant.cost_codes.find_by(code: est[:cost_code])
            if cost_code.nil?
              @error = I18n.t('services.client_import_service.errors.no_cost_code', part: part[:code], item: item[:code], code: est[:cost_code])
              return @error
            end
            estimate = work_item.estimates.find_or_initialize_by(code_controller: cost_code, description: est[:description])
            if estimate.persisted?
              translator = 'services.client_import_service.errors.conflicting_estimate_data'
              @error = I18n.t(translator, item: item[:code], part: part[:code], code: est[:cost_code], data: 'Quantity') unless (estimate.budget_quantity - est[:quantity]).abs <= 0.01 
              @error = I18n.t(translator, item: item[:code], part: part[:code], code: est[:cost_code], data: 'Rate') unless (estimate.rate - est[:rate]).abs <= 0.01 
              @error = I18n.t(translator, item: item[:code], part: part[:code], code: est[:cost_code], data: 'Amount') unless (estimate.budget_amount - est[:amount]).abs <= 0.01 
              return @error if @error
            elsif !estimate.update(budget_quantity: est[:quantity], rate: est[:rate], budget_amount: est[:amount])
              @error = I18n.t('services.client_import_service.errors.save_estimate', part: part[:code], item: item[:code], code: est[:cost_code])
              return @error
            end
          end
        end
      end
    end
