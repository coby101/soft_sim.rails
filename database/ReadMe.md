This directory contains the code that produces migrations, seeds.rb and schema.rb.  After (rails:generate) you will find seeds.rb and schema.rb in `<*implementation-directory*>/<your-project>/ror/db/`

As an example, given the simian specification forms below (taken from the demo application in the soft-sim repo), after loading and executing `(rails:generate)`, the app/db/schema.rb file, whose contents is produced by `(database::schema.rb)`, will look like the Ruby on Rails code presented further below.

After that, there are also a couple of additional examples from seeds.rb and calls to `(add-entity-migration <entity>)`
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
  :rhs-properties (:name "Subordinate" :dependency :independent))
```

excerpts from app/db/schema.rb which can be reproduced via example calls like `(database::create_table (find-entity :employee))` and `(database::change_table (find-entity :employee))`:

```ruby
# ############################################################################## #
# this file was generated by "schema.rb". If you modify this file, please remove #
# this header to make clear the generated content is now obsolete.               #
# ############################################################################## #
ActiveRecord::Schema.define() do

  enable_extension "plpgsql"

  create_table(:companies) do |t|

    t.string :code, index: { unique: true }, null: false
    t.string :company_type_type, index: true, default: 'construction'
    t.text :description
    t.string :name, index: { unique: true }, null: false
    t.timestamps
    #  associations (as below) are realized in the change_table method
    #

  end

  create_table(:company_types) do |t|

    t.string :company_type_type, index: { unique: true }, null: false
    t.timestamps

  end

   create_table(:divisions) do |t|

    t.string :code, index: { unique: true }, null: false
    t.text :description
    t.string :name, index: { unique: true }, null: false
    t.string :status, default: 'active', null: false
    t.timestamps
    #  associations (as below) are realized in the change_table method
    #
    #     t.references :operational_manager, null: false, index: true, default: 0, foreign_key: { to_table: :employees }
    #     t.belongs_to :company, null: false, index: true, default: 0, foreign_key: { to_table: :companies }

  end

  create_table(:division_statuses) do |t|

    t.string :status, index: { unique: true }, null: false
    t.timestamps

  end

   create_table(:employees) do |t|

    t.string :address1
    t.string :address2
    t.string :address3
    t.date :birth_date
    t.string :code, index: { unique: true }, null: false
    t.string :country, default: 'AU'
    t.string :family_name
    t.string :given_name, null: false
    t.decimal :lattitude
    t.string :locality
    t.decimal :longitude
    t.string :middle_name
    t.string :nick_name
    t.string :post_code
    t.string :state
    t.timestamps
    #  associations (as below) are realized in the change_table method
    #
    #     t.references :manager, null: true, index: true, foreign_key: { to_table: :employees }
    #     t.references :employer, null: true, index: true, foreign_key: { to_table: :companies }

  end

  create_table(:employee_phones) do |t|

    t.text :comment
    t.string :number, null: false
    t.string :phone_type, default: 'Work', null: false
    t.timestamps
    #  associations (as below) are realized in the change_table method
    #
    #     t.references :employee, null: false, index: true, default: 0, foreign_key: { to_table: :employees }

  end

  change_table :divisions do |t|
    t.references :operational_manager, null: false, index: true, default: 0, foreign_key: { to_table: :employees }
    t.belongs_to :company, null: false, index: true, default: 0, foreign_key: { to_table: :companies }
  end

  change_table :employees do |t|
    t.references :manager, null: true, index: true, foreign_key: { to_table: :employees }
    t.references :employer, null: true, index: true, foreign_key: { to_table: :companies }
  end

  change_table :employee_phones do |t|
    t.references :employee, null: false, index: true, default: 0, foreign_key: { to_table: :employees }
  end

  ```
 From app/db/seeds.rb:

```ruby
CompanyType.db_import!([
  { :company_type_type => 'construction' },
  { :company_type_type => 'retail' },
  { :company_type_type => 'engineering' },
  { :company_type_type => 'holding' }
])
ActiveRecord::Base.connection.reset_pk_sequence!('company_types')

DivisionStatus.db_import!([
  { :status => 'active' },
  { :status => 'dormant' },
  { :status => 'archived' }
])
ActiveRecord::Base.connection.reset_pk_sequence!('division_statuses')

PhoneType.db_import!([
  { :name => 'Home', :description => 'Primary home phone number' },
  { :name => 'Work', :description => 'Primary work phone number' },
  { :name => 'Mobile', :description => 'Personal mobile phone number' },
  { :name => 'Work Mobile', :description => 'Company provided mobile phone number' }
])
ActiveRecord::Base.connection.reset_pk_sequence!('phone_types')
```

Executing `(database:add-entity-migration (find-entity :employee))` produces this code to put in a the rails migration file created with `rails generate migration CreateEmployees`:
```ruby
class CreateEmployees < ActiveRecord::Migration[7.1.1]
  def up
    create_table(:employees) do |t|

      t.string :address1
      t.string :address2
      t.string :address3
      t.date :birth_date
      t.string :code, index: { unique: true }, null: false
      t.string :country, default: 'AU'
      t.string :family_name
      t.string :given_name, null: false
      t.decimal :lattitude
      t.string :locality
      t.decimal :longitude
      t.string :middle_name
      t.string :nick_name
      t.string :post_code
      t.string :state
      t.timestamps

      t.references :manager, null: true, index: true, foreign_key: { to_table: :employees }
      t.references :employer, null: true, index: true, foreign_key: { to_table: :companies }
    end


  end
  def down
    drop_table :employees
  end
end
```
