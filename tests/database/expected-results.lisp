(in-package #:database-tests)

(defun database-test-application ()
  (let ((*application* (test-application)))
  ;; add any specific application features for database implementation tests
    *application*))

(defun phone-type-data-import ()
  "PhoneType.db_import!([
  { :name => 'Home', :description => 'Primary home phone number' },
  { :name => 'Work', :description => 'Primary work phone number' },
  { :name => 'Mobile', :description => 'Personal mobile phone number' },
  { :name => 'Work Mobile', :description => 'Company provided mobile phone number' }
])
ActiveRecord::Base.connection.reset_pk_sequence!('phone_types')
"
  )

(defun add-description-to-company-migration ()
  "
class AddDescriptionToCompanies < ActiveRecord::Migration[7.1.1]
  def up
    add_column :companies, :description, :text
  end

  def down
    remove_column :companies, :description
  end
end
"
  )

(defun add-division-migration ()
  "
class CreateDivisions < ActiveRecord::Migration[7.1.1]
  def up
    create_table(:divisions) do |t|

      t.string :code, index: { unique: true }, null: false
      t.text :description
      t.string :name, index: { unique: true }, null: false
      t.string :status, default: 'active', null: false
      t.timestamps

      t.references :operational_manager, null: false, index: true, default: 0, foreign_key: { to_table: :employees }
      t.belongs_to :company, null: false, index: true, default: 0, foreign_key: { to_table: :companies }
    end


  end
  def down
    drop_table :divisions
  end
end
"
  )

(defun add-phonetype-migration ()
  "
class CreatePhoneTypes < ActiveRecord::Migration[7.1.1]
  def up
    create_table(:phone_types) do |t|

      t.text :description
      t.string :name, index: { unique: true }, null: false
      t.timestamps

    end

    PhoneType.db_import!([
      { :name => 'Home', :description => 'Primary home phone number' },
      { :name => 'Work', :description => 'Primary work phone number' },
      { :name => 'Mobile', :description => 'Personal mobile phone number' },
      { :name => 'Work Mobile', :description => 'Company provided mobile phone number' }
    ])
    ActiveRecord::Base.connection.reset_pk_sequence!('phone_types')

  end
  def down
    drop_table :phone_types
  end
end
")

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
