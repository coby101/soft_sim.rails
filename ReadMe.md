# soft_sim.rails

## Overview
Ruby on Rails code generator for use with the soft_sim application parser. Once a Software Simian application object model is in your lisp memory, `rails:generate` will use the entity, attribute, relationship and user interface requirements embodied within and produce:
* a fully populated app/models directory
* a fully populated app/controllers directory
* a fully populated app/views directory
* routes.rb
* a fully specified schema and a seed file
* translation files for the locale in which your SoftSim specification was defined

It is sufficient to produce a functional Rails application. 

\<Coming Soon\> Depending on your application requirements, you will have app/service and app/model/concerns files as well.\</Coming Soon\>

## Usage
With Quicklisp installed in your lisp environment, navigate to its `local-projects` directory. Execute:

- `git clone https://github.com/coby101/soft_sim`
- `git clone https://github.com/coby101/soft_sim.rails`

Then, in your Lisp REPL do the following: 

```
(ql:quickload :rails-generator)
(in-package :rails)
```

To generate a rails implementation from an application specification, where the specification is in your quicklisp local-projects directory or you have specified the correct path in `*project-directory*`, do this:
```
(load-project "my-project")
(generate)
```

for just exploring you can also try:
```
(generate (soft-sim.tests:load-demo))
```
## Example

## Specification
From the soft_sim repo demo/applicationlisp file, this is an example specification file contents:
``` lisp
(use-package :simian-builder)

(define-project demo
  :org         "Simian Software"
  :name        "Demo Web Application"
  :nick-name   "Demo"
  :description "Demo Web application for the testing and review of Software ~
                Simian application generating facilities"
  :gui ("Rails" :version "7.1.1")
  :db  ("PostgreSQL" :version "16.1")
  :os  ("Linux" :code-name "Debian (bullseye)" :version 11)
  :include-frameworks (:calendar :meta-data :geo-political))

(define-lookup-table ("PhoneType") :keep-history? nil
  :documentation  "a set of standard labels for phone contact numbers"
  :seed-data ((Name Description)
              ("Home" "Primary home phone number")
              ("Work" "Primary work phone number")
              ("Mobile" "Personal mobile phone number")
              ("Work Mobile" "Company provided mobile phone number")))

(define-entity ("Employee")
  :documentation
  "personal and company related data for individuals working for the organization"
  :attributes ((:entity-code)
               :name-fields
               :address-fields
               :birthdate)
  :importable (GivenName FamilyName BirthDate)  
  :exportable (Code GivenName FamilyName BirthDate)  
  :default-layouts
  (:summary (Code :casual-full-name :short-address)
   :details ((Code Nickname GivenName FamilyName)
             :full-address)
   :context ((Code FirstLastName)))
  :repeated-attributes
  ((("Phone" "Ph. Num" "Phone Number")
    :components
    (("PhoneType" :default "Work" :domain (Phonetype . Name)
                  :constraints (($not-null) ($unique)))
     (("Number")  :type phone :nullable? nil)
     (("Comment") :type memo :documentation
      "Any additional free form comments or notes")))))

(define-aggregation :parent-dependent
    :name "CorporateStructure"
    :parent (("Company")
             :attributes
             ((:entity-code) (:entity-name) :description
              (:entity-type ("retail" "engineering" "holding") "construction"))
             :default-layouts
             (:summary (Code Name Type Description)
              :details ((Name Code Type)
                        (Description)
                        (Divisions Staff))
              :add-fields ((Code Name Type) (Description))
              :context ((Code Name Type)))
             :documentation "the top level in the heirarchy of business structure")
    :child (("Division")
            :attributes
            ((:entity-code)
             (:entity-name)
             :description
             ("Status" :default "active" :nullable? nil :domain
                       ("active" "dormant" "archived")))
            :default-layouts
            (:summary (Code Name (OperationalManager . FirstLastName) Description)
             :details ((Name Code)
                       ((OperationalManager . FirstLastName) Status)
                       (Description))
             :edit-fields ((Code Name OperationalManager) (Description))
             :add-fields ((Code Name) (OperationalManager) (Description))
             :context ((Code Name (OperationalManager . FirstLastName))))
            :documentation
            "physically or operationally distinct business units"))

(define-relationship
    ((Company (0 1)) ("hires" "are employed by") (Employee (0 *)))
    :name ("CompanyStaff")
    :lhs-properties (:name "Employer" :dependency :independent)
    :rhs-properties (:name ("Staff" :plural "Staff") :dependency :independent))

(define-relationship
    ((Employee (1 1)) ("is the operational manager of" "are managed by") (Division (0 *)))
    :name ("OperationalManager")
    :lhs-properties (:dependency :independent :name "OperationalManager")
    :rhs-properties (:dependency :changeable))

(define-recursive-relationship Employee (0 1) ("supervises" "are supervised by") (0 *)
  :name ("StaffReport" :short "Reporting Structure" :long "Staff Reporting Structure")
  :lhs-properties (:name "Manager")
  :rhs-properties (:name "Subordinates" :dependency :independent))

;; these are here only to test/demonstrate irregular (Cacti) and uncountable (Staff) entity names
;; and was an early ruby on rails generating concern given the importance of convention in Rails apps
(define-entity ("Staff" :plural "Staff") :attributes (("Name" :type id-name) ("Code" :type code)))
(define-entity ("Cactus" :plural "Cacti") :attributes (("Color" :type color) (("Prickly" "Prickly?" "Is It Prickly?") :type checkbox)))

(define-role ("staff" :short "Staff Member" :long "Company Staff Member"))

;; interface generation is in a very unstable state and no longer useful
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

### Product
After executing the above, the rails generator will have produced these files:
```
<you>:~/soft-sim/implementations/demo/ror$ find . -name '*.rb' | xargs wc -l
    20 ./app/controllers/framework_controller.rb
    52 ./app/controllers/company_management/employees_controller.rb
   102 ./app/controllers/company_management/divisions_controller.rb
    93 ./app/controllers/company_management/companies_controller.rb
    52 ./app/controllers/company_list/employees_controller.rb
   102 ./app/controllers/company_list/divisions_controller.rb
    93 ./app/controllers/company_list/companies_controller.rb
    21 ./app/controllers/application_controller.rb
    71 ./app/helpers/application_helper.rb
   141 ./app/models/role.rb
   115 ./app/models/division.rb
   108 ./app/models/application_space.rb
    88 ./app/models/employee_phone.rb
   170 ./app/models/application_module.rb
   101 ./app/models/quarter_name.rb
   130 ./app/models/financial_year.rb
   199 ./app/models/db_entity.rb
   139 ./app/models/financial_quarter.rb
    68 ./app/models/user_role.rb
    88 ./app/models/week_day.rb
   129 ./app/models/company.rb
   130 ./app/models/year.rb
   139 ./app/models/quarter.rb
    61 ./app/models/phone_type.rb
   139 ./app/models/week.rb
    88 ./app/models/month_name.rb
   211 ./app/models/db_application.rb
   127 ./app/models/application_record.rb
    57 ./app/models/attribute_class.rb
    68 ./app/models/view_role.rb
    63 ./app/models/staff.rb
    47 ./app/models/division_status.rb
   265 ./app/models/month.rb
   127 ./app/models/logical_type.rb
   122 ./app/models/day.rb
   104 ./app/models/user.rb
    46 ./app/models/calendar_status.rb
    63 ./app/models/cactus.rb
   179 ./app/models/db_attribute.rb
    46 ./app/models/attribute_owner.rb
    76 ./app/models/application_status.rb
   113 ./app/models/tenant_module.rb
   165 ./app/models/db_tenant.rb
    86 ./app/models/country.rb
    47 ./app/models/company_type.rb
   374 ./app/models/employee.rb
   147 ./app/models/view.rb
   584 ./db/schema.rb
  3861 ./db/seeds.rb
    55 ./config/routes.rb
    11 ./config/initializers/inflections.rb
    53 ./load.rb
  9736 total
```

Subdirectory ReadMe files will have example file contents













(this repo has about 8000 lines of lisp code in about 70 files as of 07/02/2024)
