# soft_sim.rails

## Package: simian.rails-generator.config (:nicknames #:rails-config #:app-config)

This package contains the code that produces routes files, initializers (just inflections.rb at the moment) and `.yml` translation files.  After (rails:generate) you will find the generated code in `<*implementation-directory*>/<your-project>/ror/config/`

As an example, execute `(rails:generate (soft-sim:load-demo))` which will load the simian specification forms below (taken from the demo application in the soft-sim repo).

```lisp

(define-lookup-table ("PhoneType") :keep-history? nil
  :documentation  "a set of standard labels for phone contact numbers"
  :seed-data ((Name Description)
              ("Home" "Primary home phone number")
              ("Work" "Primary work phone number")
              ("Mobile" "Personal mobile phone number")
              ("Work Mobile" "Company provided mobile phone number")))

(define-entity ("Employee")
  :documentation "personal and company related data for individuals working for the organization"
  :attributes ((:entity-code)
               :name-fields
               :address-fields
               :birthdate)
  :repeated-attributes
  ((("Phone" "Ph. Num" "Phone Number")
    :components
    (("PhoneType" :default "Work" :domain (Phonetype . Name) :constraints (($not-null) ($unique)))
     (("Number")  :type phone :nullable? nil)
     (("Comment") :type memo :documentation "Any additional free form comments or notes")))))

(define-aggregation :parent-dependent
    :name "CorporateStructure"
    :parent (("Company")
             :attributes
             ((:entity-code) (:entity-name) :description
              (:entity-type ("retail" "engineering" "holding") "construction"))
             :documentation "the top level in the heirarchy of business structure")
    :child (("Division")
            :attributes
            ((:entity-code)
             (:entity-name)
             :description
             ("Status" :default "active" :nullable? nil :domain ("active" "dormant" "archived")))
            :documentation "physically or operationally distinct business units"))

(define-relationship ((Company (0 1) "hires") (Employee (0 *) "are employed by"))
    :name ("CompanyStaff")
    :lhs-properties (:name "Employer" :dependency :independent)
    :rhs-properties (:name ("Staff" :plural "Staff") :dependency :independent))

(define-relationship ((Employee (1 1) "is the operational manager of") (Division (0 *) "are managed by"))
    :name ("OperationalManager")
    :lhs-properties (:dependency :independent :name "OperationalManager")
    :rhs-properties (:dependency :changeable))

(define-recursive-relationship Employee ((0 1) "supervises") ((0 *) "are supervised by")
  :name ("StaffReport" :short "Reporting Structure" :long "Staff Reporting Structure")
  :lhs-properties (:name "Manager")
  :rhs-properties (:name "subordinate" :dependency :independent))

(define-application-space ("Config" "Configuration" "Configuration and Setup Data") :parent-space nil)
(define-application-space ("Info" "Information" "Company Reference Information") :parent-space nil)
(define-application-space ("Corp" "Company Reference" "Company Reference Information") :parent-space Info)
(define-application-space ("Comp" "Company Setup" "Company Setup Management") :parent-space Config)

(define-view ("CompanyList" "Our Company" "Our Company Structure") :roles (staff)
  :application-space Corp
  :layouts
  ((Company :operations (:create :show :list))
   (Division :operations (:create :show :list))
   (Employee :operations (:show :list)))
  :description "provides access to basic company data and a company's division components.  It also ~
                provides a view of publically available contact information for company staff members")

(define-view ("CompanyManagement" "Company Setup" "Manage Company Structure") :roles (staff)
  :application-space ("Config" "Corp")
  :layouts
  ((Company :operations (:create :show :list))
   (Division :operations (:create :show :list))
   (Employee :operations (:show :list)))
  :description "provides access to basic company data and a company's division components.  It also ~
                provides a view of publically available contact information for company staff members")

```
## Routes
config/routes.rb:

```ruby

Rails.application.routes.draw do

  get '/', to: 'framework#home', as: 'home'
  get '/no_access', to: 'framework#denied'
  get '/about', to: 'framework#about', as: 'about'
  mount ActionCable.server => '/cable'

  # routes for the "Company Setup Management" application space
  draw :comp
  # routes for the "Company Reference Information" application space
  draw :corp
  # routes for the "Company Reference Information" application space
  draw :info
  # routes for the "Configuration and Setup Data" application space
  draw :config
  # routes for the "Application Developer Views" application space
  draw :dev

end
```

config/routes/config.rb

```ruby

scope '/config' do
  resources :companies, only: %i[edit update new create destroy show index] do
    resources :divisions, only: %i[edit update new create destroy show index]
    resources :employees, only: %i[show index]
  end
end
```
## Initializers
config/initializers/inflections.rb


```ruby
ActiveSupport::Inflector.inflections(:en) do |inflect|
  inflect.irregular 'Cactus', 'Cacti'
  inflect.uncountable 'Staff'
  inflect.acronym 'DB'
end
```
## Locales
NB: there is a keyword argument to the `define-project` macro (`:natural-language :english`) that set the appropriate key and names for the locale files

config/locales/generated.en.yml (excerpted)

```yml
en:
  activerecord:
    models:
      company:
        one: "Company"
        other: "Companies"
        long_name: "Company"
        short_name: "Company"
        long_plural: "Companies"
        short_plural: "Companies"
      company_type:
        one: "Company Type"
        other: "Company Types"
        long_name: "Company Company Type"
        short_name: "Company Type"
        long_plural: "Company Company Types"
        short_plural: "Company Types"
      division:
        one: "Division"
        other: "Divisions"
        long_name: "Division"
        short_name: "Division"
        long_plural: "Divisions"
        short_plural: "Divisions"
      division_status:
        one: "Division Status"
        other: "Division Statuses"
        long_name: "Division Status"
        short_name: "Division Status"
        long_plural: "Division Statuses"
        short_plural: "Division Statuses"
      employee:
        one: "Employee"
        other: "Employees"
        long_name: "Employee"
        short_name: "Employee"
        long_plural: "Employees"
        short_plural: "Employees"
      employee_phone:
        one: "Phone"
        other: "Phones"
        long_name: "Employee Phone"
        short_name: "Phone"
        long_plural: "Employee Phones"
        short_plural: "Phones"
    attributes:
      company:
        code:
          one: "Company Code"
          other: "Company Codes"
          long_name: "Company Code"
          short_name: "Code"
          description: "a short, unique, user-managed record identifier"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        company_type_type:
          one: "Company Type"
          other: "Company Types"
          long_name: "Company Type"
          short_name: "Type"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        description:
          one: "Description"
          other: "Descriptions"
          long_name: "Description"
          short_name: "Description"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        divisions:
          one: "Divisions"
          other: "Divisions"
          long_name: "Divisions"
          short_name: "Divisions"
          description: "The Divisions this Company has"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        name:
          one: "Company Name"
          other: "Company Names"
          long_name: "Company Name"
          short_name: "Name"
          description: "a short name string that uniquely identifies a record"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        staff:
          one: "Staff"
          other: "Staff"
          long_name: "Staff"
          short_name: "Staff"
          description: "The Staff this Company has"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
      company_type:
        company_type_type:
          one: "Company Type"
          other: "Company Types"
          long_name: "Company Type"
          short_name: "Type"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
      division:
        code:
          one: "Division Code"
          other: "Division Codes"
          long_name: "Division Code"
          short_name: "Code"
          description: "a short, unique, user-managed record identifier"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        company_id:
          one: "Company"
          other: "Companies"
          long_name: "Company"
          short_name: "Company"
          description: "A link to this Division's Company, a record in the Company table"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        description:
          one: "Description"
          other: "Descriptions"
          long_name: "Description"
          short_name: "Description"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        name:
          one: "Division Name"
          other: "Division Names"
          long_name: "Division Name"
          short_name: "Name"
          description: "a short name string that uniquely identifies a record"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        operational_manager_id:
          one: "Operational Manager"
          other: "Operational Managers"
          long_name: "Operational Manager"
          short_name: "Operational Manager"
          description: "A link to this Division's Operational Manager, a record in the Employee table"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        status:
          one: "Status"
          other: "Statuses"
          long_name: "Status"
          short_name: "Status"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
      division_status:
        status:
          one: "Status"
          other: "Statuses"
          long_name: "Status"
          short_name: "Status"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
      employee:
        address1:
          one: "Address (line 1)"
          other: "Address (line 1)s"
          long_name: "Address (line 1)"
          short_name: "Address"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        address2:
          one: "Address (line 2)"
          other: "Address (line 2)s"
          long_name: "Address (line 2)"
          short_name: "Address"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        address3:
          one: "Address (line 3)"
          other: "Address (line 3)s"
          long_name: "Address (line 3)"
          short_name: "Address"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        birth_date:
          one: "Date of Birth"
          other: "Date of Births"
          long_name: "Date of Birth"
          short_name: "DOB"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        code:
          one: "Employee Code"
          other: "Employee Codes"
          long_name: "Employee Code"
          short_name: "Code"
          description: "a short, unique, user-managed record identifier"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        country:
          one: "Country"
          other: "Countries"
          long_name: "Country"
          short_name: "Country"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        divisions:
          one: "Divisions"
          other: "Divisions"
          long_name: "Divisions"
          short_name: "Divisions"
          description: "The Divisions this Employee has"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        employee_phones:
          one: "Employee Phones"
          other: "Employee Phones"
          long_name: "Employee Phones"
          short_name: "Phones"
          description: "The Employee Phones this Employee has"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        employer_id:
          one: "Employer"
          other: "Employers"
          long_name: "Employer"
          short_name: "Employer"
          description: "A link to this Employee's Employer, a record in the Company table"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        fam_given_name:
          one: "Full Name"
          other: "Full Names"
          long_name: "Full Name"
          short_name: "Full Name"
          description: "Asian style fullname with family name first"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        family_name:
          one: "Family Name"
          other: "Family Names"
          long_name: "Family Name"
          short_name: "Family Name"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        first_last_name:
          one: "Full Name"
          other: "Full Names"
          long_name: "Full Name"
          short_name: "Full Name"
          description: "Western style fullname with given name first"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        given_name:
          one: "Given Name"
          other: "Given Names"
          long_name: "Given Name"
          short_name: "Given Name"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        lattitude:
          one: "Lattitude"
          other: "Lattitudes"
          long_name: "Lattitude"
          short_name: "Lattitude"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        locality:
          one: "Municipality or Suburb"
          other: "Municipality or Suburbs"
          long_name: "Municipality or Suburb"
          short_name: "Locality"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        longitude:
          one: "Longitude"
          other: "Longitudes"
          long_name: "Longitude"
          short_name: "Longitude"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        manager_id:
          one: "Manager"
          other: "Managers"
          long_name: "Manager"
          short_name: "Manager"
          description: "A link to this Employee's Manager, a record in the Employee table"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        middle_name:
          one: "Middle Name"
          other: "Middle Names"
          long_name: "Middle Name"
          short_name: "Middle Name"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        nick_name:
          one: "Nick Name"
          other: "Nick Names"
          long_name: "Nick Name"
          short_name: "Nick Name"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        post_code:
          one: "Post Code"
          other: "Post Codes"
          long_name: "Post Code"
          short_name: "Post Code"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        state:
          one: "State"
          other: "States"
          long_name: "State"
          short_name: "State"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        subordinates:
          one: "Subordinates"
          other: "Subordinates"
          long_name: "Subordinates"
          short_name: "Subordinates"
          description: "The Subordinates this Employee has"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        sur_first_name:
          one: "Full Name"
          other: "Full Names"
          long_name: "Full Name"
          short_name: "Full Name"
          description: "Western style fullname with surname first"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        user_select_data:
          one: "User Select Data"
          other: "User Select Datas"
          long_name: "User Select Data"
          short_name: "User Select Data"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
      employee_phone:
        comment:
          one: "Comment"
          other: "Comments"
          long_name: "Comment"
          short_name: "Comment"
          description: "Any additional free form comments or notes"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        employee_id:
          one: "Employee"
          other: "Employees"
          long_name: "Employee"
          short_name: "Employee"
          description: "A link to this Phone's Employee, a record in the Employee table"
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        number:
          one: "Number"
          other: "Numbers"
          long_name: "Number"
          short_name: "Number"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
        phone_type:
          one: "Phone Type"
          other: "Phone Types"
          long_name: "Phone Type"
          short_name: "Phone Type"
          description: ""
          detailed_description: ""
          technical_summary: ""
          technical_detail: ""
