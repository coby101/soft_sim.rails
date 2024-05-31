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
## Product
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
