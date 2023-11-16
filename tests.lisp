;;;===========================================================================
;;; file:   lib/tests/ror.lisp
;;; auth:   Coby Beck
;;; date:   2021-08-06
;;;
;;;---------------------------------------------------------------------------
;;;
;;;  tests for generating ruby on rails code
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :simian)
(load-generator "ror")

(define-test :views "template-items are resolved and unparsed correctly 1"
  (let ((*auto-create-lookups* nil))
    (with-new-schema
        (define-aggregation :parent-dependent
          :name "CorporateStructure"
          :parent (("Company")
                   :attributes
                   ((:entity-code) (:entity-name) (:range "CEOAge" :type human-age)
                    (:entity-type "construction" ("retail" "engineering"))))
          :child (("Division")
                  :attributes
                  ((:entity-code)
                   (:entity-name) (:range "MDAge" :type human-age)
                   ("Status" :default "active" :nullable? nil :domain
                             ("active" "dormant" "archived")))))
      (ror::conventionalize-app)
      (post-parse-project)
      (let ((ruby::*include-rails* t)
            (div (find-entity :division))
            (comp (find-entity :company)))
        (list
         (ror::unparse-template-expression (resolve-panel-items div 'status) "@division")
         (ror::unparse-template-expression (resolve-panel-items div '(Company Name)) "@division")
         (ror::unparse-template-expression (resolve-panel-items div 'Type) "@division")
         (ror::unparse-template-expression (resolve-panel-items comp '($strcat Name " - " Type)) "@company")
         (ror::unparse-template-expression
          (resolve-panel-items comp '($divide ($add MaxCEOAge MinCEOAge) 2)) "@company")
         (ror::unparse-template-expression
          (resolve-panel-items div '($divide ($add (Company MaxCEOAge) MaxMDAge) 2)) "@division")))))
  '("@division.status" "@division.company.name"
    "@division.company.company_type"
    "(@company.name.to_s + ' - ' + @company.company_type.to_s)"
    "((@company.max_ceo_age + @company.min_ceo_age) / 2)"
    "((@division.company.max_ceo_age + @division.max_md_age) / 2)"))

(define-test :views "form helpers are generating correctly 1"
  (let ((*auto-create-lookups* nil))
    (with-new-schema
        (define-aggregation :parent-dependent
          :name "CorporateStructure"
          :parent (("Corporation")
                   :attributes
                   ((:entity-code) (:entity-name) :description
                    (:entity-type "construction" ("retail" "engineering"))))
          :child (("Department")
                  :attributes
                  ((:entity-code)
                   (:entity-name)
                   ("Status" :default "active" :nullable? nil :domain
                             ("active" "dormant" "archived")))))
      (ror::conventionalize-app)
      (post-parse-project)
      (let ((ruby::*include-rails* t))
        (list
         (let ((item (find-field :name :corporation)))
           (ror::unparse-form-helper item (logical-type item)))
         (let ((item (find-field :code :corporation)))
           (ror::unparse-form-helper item (logical-type item)))
         (let ((item (find-field :description :corporation)))
           (ror::unparse-form-helper item (logical-type item)))
         (let ((item (find-field :type :corporation)))
           (ror::unparse-form-helper item (logical-type item)))
         (let ((item (find-field :status :department)))
           (ror::unparse-form-helper item (logical-type item)))
         (let ((item (find-field :corporation :department)))
           (ror::unparse-form-helper item (logical-type item)))))))
  '("form.text_field :name, size: \"40\", class: \"field\", required: true"
   "form.text_field :code, size: \"40\", class: \"field\", required: true"
   "form.text_area :description, size: \"70x4\", class: \"field\""
   "form.select :corporation_type, [[\"\", \"\"], [\"construction\", \"construction\"], [\"retail\", \"retail\"], [\"engineering\", \"engineering\"]], { selected: form.object.corporation_type || \"construction\" }, { class: \"form-collection\" }"
   "form.select :status, [[\"(required)\", \"\"], [\"active\", \"active\"], [\"dormant\", \"dormant\"], [\"archived\", \"archived\"]], { selected: form.object.status || \"active\" }, { class: \"form-collection\", required: true }"
   "form.collection_select :corporation_id, Corporation.order(:name), :id, :name, { :include_blank => 'please select' }, { class: \"form-collection\", required: true }"))


(define-test :views "form helpers are generating correctly 2"
  (with-new-schema
      (define-lookup-table ("PhoneType"))
    (define-entity ("Employee")
        :documentation
        "personal and company related data for individuals working for the organization"
        :attributes ((:entity-code)
                     :name-fields
                     :birthdate)
        :repeated-attributes
        ((("Phone" "Ph. Num" "Phone Number")
          :components
          (("PhoneType" :default "main" :domain (Phonetype Name)
                        :constraints (($not-null) ($unique)))
           (("Number")  :type phone :nullable? nil)
           (("Comment") :type memo :documentation
                        "Any additional free form comments or notes")))))
    (define-recursive-relationship Employee (0 1) ("supervises" "are supervised by") (0 *)
      :name ("StaffReport")
      :lhs-properties (:name "Manager")
      :rhs-properties (:name "Subordinate" :dependency :independent))
    (define-entity ("Cactus" :plural "Cacti") :attributes (("Color" :type color)))
    (ror::conventionalize-app)
    (post-parse-project)
    (let ((ruby::*include-rails* t))
      (list
       (let ((item (find-field :comment :employeephone)))
         (ror::unparse-form-helper item (logical-type item)))
       (let ((item (find-field :phonetype :employeephone)))
         (ror::unparse-form-helper item (logical-type item)))
       (let ((item (find-field :givenname :employee)))
         (ror::unparse-form-helper item (logical-type item)))
       (let ((item (find-field :birthdate :employee)))
         (ror::unparse-form-helper item (logical-type item)))
       (let ((item (find-field :manager :employee)))
         (ror::unparse-form-helper item (logical-type item)))
       (let ((item (find-field :color :cactus)))
         (ror::unparse-form-helper item (logical-type item))))))
  '("form.text_area :comment, size: \"70x4\", class: \"field\""
    "form.collection_select :phone_type, PhoneType.order(:name), :name, :name, { :include_blank => 'please select' }, { class: \"form-collection\", required: true }"
    "form.text_field :given_name, size: \"40\", class: \"field\", required: true"
    "form.text_field :birth_date, size: \"40\", class: \"field\""
    "form.collection_select :manager_id, Employee.order(:code), :id, :user_select_data, { :include_blank => '' }, { class: \"form-collection\" }"
    "form.color_field :color"))

(define-test :attributes "logical boolean fields are implemented in the schema correctly"
  (with-new-schema
      (define-entity ("Anything")
        :attributes ((("Private") :type yes/no :nullable? nil)
                     (("Tall") :type yes/no :nullable? t)
                     (("Agreed") :type yes/no :nullable? nil :default "no")
                     (("Important") :type yes/no :nullable? t :default "no")
                     (("Normal") :type yes/no :nullable? nil :default "yes")))
    (ror::conventionalize-app)
    (post-parse-project)
    (list
     (ror::t.column (find-field :private :anything) nil)
     (ror::t.column (find-field :tall :anything) nil)
     (ror::t.column (find-field :agreed :anything) nil)
     (ror::t.column (find-field :important :anything) nil)
     (ror::t.column (find-field :normal :anything) nil)))
  '("t.string :private, null: false"
    "t.string :tall"
    "t.boolean :agreed, default: false, null: false"
    "t.string :important, default: 'no'"
    "t.boolean :normal, default: true, null: false"))

(define-test :validations "logical boolean fields are validated correctly"
  (with-new-schema
      (define-entity ("Anything")
        :attributes ((("Private") :type yes/no :nullable? nil)
                     (("Tall") :type yes/no :nullable? t)
                     (("Agreed") :type yes/no :nullable? nil :default "no")
                     (("Important") :type yes/no :nullable? t :default "no")
                     (("Normal") :type yes/no :nullable? nil :default "yes")))
    (ror::conventionalize-app)
    (resolve-constraint-objects)
    (reverse (ror::model-validations (find-entity :anything))))
  '("validates :private, inclusion: 
    { in: [\"yes\", \"no\"],
      message: \"Private value must be one of \\\"yes\\\" or \\\"no\\\". \\\"%{value}\\\" is not in there\"}"
    "" "validates :tall, inclusion: 
    { in: [\"yes\", \"no\", nil],
      message: \"Tall value must be one of \\\"yes\\\" or \\\"no\\\". \\\"%{value}\\\" is not in there\",
      allow_blank: true}"
    "validates :agreed, inclusion: 
    { in: [true, false],
      message: \"Agreed value must be one of \\\"yes\\\" or \\\"no\\\". \\\"%{value}\\\" is not in there\"}"
    "" "validates :important, inclusion: 
    { in: [\"yes\", \"no\", nil],
      message: \"Important value must be one of \\\"yes\\\" or \\\"no\\\". \\\"%{value}\\\" is not in there\",
      allow_blank: true}"
    "validates :normal, inclusion: 
    { in: [true, false],
      message: \"Normal value must be one of \\\"yes\\\" or \\\"no\\\". \\\"%{value}\\\" is not in there\"}"
    ""))

(define-test :validations "logical boolean fields have correct form helpers"
  (with-new-schema
      (define-entity ("Anything")
        :attributes ((("Private") :type yes/no :nullable? nil)
                     (("Tall") :type yes/no :nullable? t)
                     (("Agreed") :type yes/no :nullable? nil :default "no")
                     (("Important") :type yes/no :nullable? t :default "no")
                     (("Normal") :type yes/no :nullable? nil :default "yes")))
    (ror::conventionalize-app)
    (resolve-constraint-objects)
    (mapcar #'(lambda (field)
                (ror::unparse-form-helper field (logical-type field)))
            (user-attributes (find-entity :anything))))
  '("form.check_box :normal, class: \"field\", checked: true"
    "form.select :important, [[\"\", \"\"], [\"yes\", \"yes\"], [\"no\", \"no\"]], { selected: form.object.important || \"no\" }, { class: \"form-collection\" }"
    "form.check_box :agreed, class: \"field\""
    "form.select :tall, [[\"\", \"\"], [\"yes\", \"yes\"], [\"no\", \"no\"]], { selected: form.object.tall || ''}, { class: \"form-collection\" }"
    "form.select :private, [[\"(required)\", \"\"], [\"yes\", \"yes\"], [\"no\", \"no\"]], { selected: form.object.private || ''}, { class: \"form-collection\", required: true }"))

(define-test :routing "routes for nested aspects have proper nested path names"
  (with-new-schema
      (define-entity ("Owner")
          :attributes (("Name" :type name)))
      (define-entity ("Dog")
          :attributes (("Age" :type age)))
    (define-relationship ((Owner (1 1)) ("has" "is owned by") (Dog (0 *))))
    (define-application-space ("Test") :parent-space nil)
    (define-view ("MngDog")
      :roles ()
      :application-space (Test)
      :layouts
      ((Owner :operations (:create :show :list)
              :add-fields ((Name))
              :details ((Name))
              :summary (Name))
       (Dog  :operations (:create :show :list)
             :add-fields ((Age))
             :details ((Age))
             :summary (Age))))
    (let ((ruby::*include-rails* t))
      (ror::conventionalize-app)
      (mapcar #'ror::unparse-routes (aspects (find-view :MngDog)))))
  '(("get '/mng_dog/owners/:owner_id/dogs', to: 'mng_dog/dogs#index', as: 'mng_dog_owner_dogs'"
  "get '/mng_dog/owners/:owner_id/dogs/new', to: 'mng_dog/dogs#new', as: 'new_mng_dog_owner_dog'"
  "get '/mng_dog/owners/:owner_id/dogs/:id/edit', to: 'mng_dog/dogs#edit', as: 'edit_mng_dog_owner_dog'"
  "get '/mng_dog/owners/:owner_id/dogs/:id', to: 'mng_dog/dogs#show', as: 'mng_dog_owner_dog'"
  "post '/mng_dog/owners/:owner_id/dogs', to: 'mng_dog/dogs#create'"
  "put '/mng_dog/owners/:owner_id/dogs/:id', to: 'mng_dog/dogs#update'"
  "patch '/mng_dog/owners/:owner_id/dogs/:id', to: 'mng_dog/dogs#update'"
  "delete '/mng_dog/owners/:owner_id/dogs/:id', to: 'mng_dog/dogs#destroy'")
 ("get '/mng_dog/owners', to: 'mng_dog/owners#index', as: 'mng_dog_owners'"
  "get '/mng_dog/owners/new', to: 'mng_dog/owners#new', as: 'new_mng_dog_owner'"
  "get '/mng_dog/owners/:id/edit', to: 'mng_dog/owners#edit', as: 'edit_mng_dog_owner'"
  "get '/mng_dog/owners/:id', to: 'mng_dog/owners#show', as: 'mng_dog_owner'"
  "post '/mng_dog/owners', to: 'mng_dog/owners#create'"
  "put '/mng_dog/owners/:id', to: 'mng_dog/owners#update'"
  "patch '/mng_dog/owners/:id', to: 'mng_dog/owners#update'"
  "delete '/mng_dog/owners/:id', to: 'mng_dog/owners#destroy'")))

(define-test :routing "controller class assignments unparse well"
  (with-new-schema
      (define-entity ("Owner")
        :attributes (("Name" :type name)))
    (define-entity ("Dog")
      :attributes (("Age" :type age))
      :repeated-attributes (("NickName" :type name)))
    (define-relationship ((Owner (1 1)) ("has" "is owned by") (Dog (0 *))))
    (define-application-space ("Test") :parent-space nil)
    (define-view ("MngDog")
      :roles ()
      :application-space (Test)
      :layouts
      ((Owner :operations (:create :show :list)
              :add-fields ((Name))
              :details ((Name))
              :summary (Name))
       (Dog  :operations (:create :show :list)
             :add-fields ((Age))
             :details ((Age))
             :summary (Age))
       (DogNickName :operations (:list)
                    :summary (NickName))))
    (let ((ruby::*include-rails* t)
          (*nesting-level* 4))
      (ror::conventionalize-app)
      (loop for action in '(:show :delete :list :new :create)
            collect
            (list
             action
             (loop for aspect in (aspects (find-view :MngDog))
                   collect (list (id (entity aspect))
                                 (ror::instance-variable-assignment aspect action)))))))
  '((:SHOW
     ((:DOGNICKNAME "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.includes(:dog_nick_names).find(params[:dog_id])
        @dog_nick_name = @dog.dog_nick_names.find(params[:id])")
      (:DOG "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.includes(:dog_nick_names).find(params[:id])")
      (:OWNER "@owner = Owner.find(params[:id])")))
    (:DELETE
     ((:DOGNICKNAME "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.includes(:dog_nick_names).find(params[:dog_id])
        @dog_nick_name = @dog.dog_nick_names.find(params[:id])")
      (:DOG "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.includes(:dog_nick_names).find(params[:id])")
      (:OWNER "@owner = Owner.find(params[:id])")))
    (:LIST
     ((:DOGNICKNAME "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.includes(:dog_nick_names).find(params[:dog_id])
        @dog_nick_names = @dog.dog_nick_names.page params[:page]")
      (:DOG "@owner = Owner.find(params[:owner_id])
        @dogs = @owner.dogs.page params[:page]")
      (:OWNER "@owners = Owner.all.page params[:page]")))
    (:NEW
     ((:DOGNICKNAME "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.includes(:dog_nick_names).find(params[:dog_id])
        @dog_nick_name = @dog.dog_nick_names.build")
      (:DOG "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.build")
      (:OWNER "@owner = Owner.new")))
    (:CREATE
     ((:DOGNICKNAME "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.includes(:dog_nick_names).find(params[:dog_id])
        @dog_nick_name = @dog.dog_nick_names.build(dog_nick_name_params)")
      (:DOG "@owner = Owner.find(params[:owner_id])
        @dog = @owner.dogs.build(dog_params)")
      (:OWNER "@owner = Owner.new(owner_params)")))))

(define-test :routing "models with regular inflections have proper path names"
  (with-new-schema
      (define-entity ("Dog")
          :attributes (("Age" :type age)))
    (let ((ruby::*include-rails* t))
      (ror::unparse-routes (car (aspects (create-default-view (find-entity :dog)))))))
  '("get '/dogs', to: 'dogs#index', as: 'dogs'"
    "get '/dogs/new', to: 'dogs#new', as: 'new_dog'"
    "get '/dogs/:dog/edit', to: 'dogs#edit', as: 'edit_dog'"
    "get '/dogs/:dog', to: 'dogs#show', as: 'dog'"
    "post '/dogs', to: 'dogs#create'" "put '/dogs/:dog', to: 'dogs#update'"
    "patch '/dogs/:dog', to: 'dogs#update'"
    "delete '/dogs/:dog', to: 'dogs#destroy'"))

(define-test :routing "models with uncountable names have proper path names"
  (with-new-schema
      (define-entity ("Staff" :plural "Staff")
          :attributes (("Age" :type age)))
    (let ((ruby::*include-rails* t))
      (ror::unparse-routes (car (aspects (create-default-view (find-entity :staff)))))))
  ;; order matters!
  '("get '/staff', to: 'staff#index', as: 'staff_index'"
    "get '/staff/new', to: 'staff#new', as: 'new_staff'"
    "get '/staff/:staff/edit', to: 'staff#edit', as: 'edit_staff'"
    "get '/staff/:staff', to: 'staff#show', as: 'staff'"
    "post '/staff', to: 'staff#create'"
    "put '/staff/:staff', to: 'staff#update'"
    "patch '/staff/:staff', to: 'staff#update'"
    "delete '/staff/:staff', to: 'staff#destroy'"))

(define-test :routing "models with irregular plurals have proper path names"
  (with-new-schema
      (define-entity ("Cactus" :plural "Cacti")
          :attributes (("Color" :type label)))
    (let ((ruby::*include-rails* t))
      (ror::unparse-routes (car (aspects (create-default-view (find-entity :cactus)))))))
  '("get '/cacti', to: 'cacti#index', as: 'cacti'"
    "get '/cacti/new', to: 'cacti#new', as: 'new_cactus'"
    "get '/cacti/:cactus/edit', to: 'cacti#edit', as: 'edit_cactus'"
    "get '/cacti/:cactus', to: 'cacti#show', as: 'cactus'"
    "post '/cacti', to: 'cacti#create'"
    "put '/cacti/:cactus', to: 'cacti#update'"
    "patch '/cacti/:cactus', to: 'cacti#update'"
    "delete '/cacti/:cactus', to: 'cacti#destroy'"))

(define-test :validations "numericality validations are written correctly 1"
  (with-new-schema
      (define-entity ("Anything")
          :attributes (("Qty" :type quantity :nullable? nil
                              :constraints ((:expression ($= 17) :message "Wrong! Try again!")
                                            ($gt 5) (:expression ($<= 4) :event :update)))))
    (resolve-constraint-objects nil)
    (let ((ruby::*include-rails* t))
      (mapcar #'ror::format-model-validation
              (constraints (find-field :qty :anything)))))
  '("validates :qty, numericality: 
    { less_than_or_equal_to: 4,
      message: \"the value of Qty must be less than or equal to 4. \\\"%{value}\\\" is too large\",
      on: :update}"
   "validates :qty, numericality: 
    { greater_than: 5,
      message: \"the value of Qty must be greater than 5. \\\"%{value}\\\" is too small\"}"
   "validates :qty, numericality: 
    { equal_to: 17,
      message: \"Wrong! Try again!\"}"
   "validates :qty, presence: { message: \"all %{model} records require a value for the %{attribute} field. \" }"))

(define-test :validations "numericality validations are written correctly 2"
  (with-new-schema
      (define-entity ("Anything")
          :attributes (("Count" :type integer
                              :constraints (($odd) ($even) (:expression ($!= 17) :message "Wrong! Try again!")
                                            ($>= 5) (:expression ($lt 4) :event :update)))))
    (resolve-constraint-objects nil)
    (let ((ruby::*include-rails* t))
      (mapcar #'ror::format-model-validation
            (constraints (find-field :Count :anything)))))
  '("validates :count, numericality: 
    { less_than: 4,
      message: \"the value of Count must be less than 4. \\\"%{value}\\\" is too large\",
      only_integer: true,
      on: :update,
      allow_blank: true}"
    "validates :count, numericality: 
    { greater_than_or_equal_to: 5,
      message: \"the value of Count must be greater than or equal to 5. \\\"%{value}\\\" is too small\",
      only_integer: true,
      allow_blank: true}"
    "validates :count, numericality: 
    { other_than: 17,
      message: \"Wrong! Try again!\",
      only_integer: true,
      allow_blank: true}"
    "validates :count, numericality: 
    { even: true,
      message: \"the value of Count must be even.\",
      only_integer: true,
      allow_blank: true}"
    "validates :count, numericality: 
    { odd: true,
      message: \"the value of Count must be odd.\",
      only_integer: true,
      allow_blank: true}"))

(define-test :validations "numericality validations are written correctly 3"
  (with-new-schema
      (define-entity ("Anything")
          :attributes (("Req" :type quantity :nullable? nil
                              :constraints ((:expression ($= 17))
                                            ($> 5) (:expression ($< 4) :event :create)))))
    (resolve-constraint-objects nil)
    (let ((ruby::*include-rails* t))
      (mapcar #'ror::format-model-validation
            (constraints (find-field :Req :anything)))))
  '("validates :req, numericality: 
    { less_than: 4,
      message: \"the value of Req must be less than 4. \\\"%{value}\\\" is too large\",
      on: :create}"
   "validates :req, numericality: 
    { greater_than: 5,
      message: \"the value of Req must be greater than 5. \\\"%{value}\\\" is too small\"}"
   "validates :req, numericality: 
    { equal_to: 17,
      message: \"the value of Req must be 17.\"}"
   "validates :req, presence: { message: \"all %{model} records require a value for the %{attribute} field. \" }"))

(define-test :validations "other validations are written correctly 1"
  (with-new-schema
      (define-entity ("Anything")
          :attributes (("Req" :type integer :nullable? nil
                        :constraints ((:expression ($call "ReqCheq") :event :create)
                                      (:expression ($when ($not-null Req) ($between 5 10))
                                       :event :update)
                                      (:expression ($when ($call "NeedsIt") ($call "DoIt"))
                                       :event :update)))))
    (resolve-constraint-objects nil)
    (let ((ruby::*include-rails* t))
      (with-nesting
          (mapcar #'ror::format-model-validation
              (constraints (find-field :Req :anything))))))
  ;; the validates_with expressions below are not run-time debugged yet
  '("
  validate :when_function_call__needs__function_call__do_it_, on: :update
  def when_function_call__needs__function_call__do_it_
    if (NeedsIt && !(DoIt))
      errors.add(:req, \"The check to ensure that TRUE if any of call the function \\\"NeedsIt\\\" with nil is not true and call the function \\\"DoIt\\\" with nil is TRUE has failed\")
    end
  end
"
    "validates :req, numericality: 
    { in: (5..10),
      message: \"the value of Req must be between 5 and 10.\",
      only_integer: true,
      if: -> () { req.present? },
      on: :update}"
    "
  validate :function_call__req_ch_, on: :create
  def function_call__req_ch_
    if !(ReqCheq)
      errors.add(:req, \"The check to ensure that call the function \\\"ReqCheq\\\" with nil has failed\")
    end
  end
"
    "validates :req, presence: { message: \"all %{model} records require a value for the %{attribute} field. \" }"))

(define-test :validations "other validations are written correctly 2"
  (with-new-schema
      (define-aggregation :parent-dependent
        :name "TestAgg"
        :parent (("ParentEntity")
                 :attributes ((:entity-name) :email))
        :child (("ChildEntity")
                :attributes
                (("Code" :type code :nullable? nil
                  :constraints ((:expression ($unique-within ParentEntity)
                                 :message "need to tell us apart"))))))
    (resolve-constraint-objects nil)
    (ror::conventionalize-app)
    (let ((ruby::*include-rails* t))
      (mapcar #'ror::format-model-validation
              (constraints (find-field :Code :ChildEntity)))))
  '("validates :code, uniqueness: 
    { scope: :parent_entity_id,
      message: \"need to tell us apart\"}"
   "validates :code, presence: { message: \"all %{model} records require a value for the %{attribute} field. \" }"
   "validates :code, format: 
    { message: \"all %{attribute} values must match a pattern. \",
      with: /\\A[a-zA-Z0-9_.-]{0,15}\\z/}"))

(define-test :validations "string validations are written correctly"
  (with-new-schema
      (let ((*auto-create-lookups* nil))
        (define-entity ("Anything")
            :attributes (("ABN" :type ABN :nullable? nil :constraints
                                (($unique) (:expression ($length 1) :event :create)))
                         ("Label" :type text
                                  :constraints (($length 5) ($length-between 6 10)
                                                (:expression ($length-lt 17) :message "tl;dr")))
                         ("Status" :type label :domain ("good" "bad" "ugly"))))
        (resolve-constraint-objects nil)
        (let ((ruby::*include-rails* t))
          (mapcar #'ror::format-model-validation
                  (append (constraints (find-field :ABN :anything))
                          (constraints (find-field :Label :anything))
                          (constraints (find-field :Status :anything)))))))
  '("validates :abn, length: 
    { is: 1,
      too_long: \"ABN length must be one character. \\\"%{value}\\\" is too long\",
      too_short: \"ABN length must be one character. \\\"%{value}\\\" is too short\",
      on: :create}"
   "validates :abn, uniqueness: { message: \"all %{model} records must have a unique ABN value. \\\"%{value}\\\" is taken\" }"
   "validates :abn, presence: { message: \"all %{model} records require a value for the %{attribute} field. \" }"
   "validates :abn, format: 
    { message: \"all %{attribute} values must match a pattern. ABNs must be exactly 11 digits, with no letters or spaces\",
      with: /\\A[0-9]{11}\\z/}"
   "validates :label, length: 
    { maximum: 16,
      message: \"tl;dr\",
      allow_blank: true}"
   "validates :label, length: 
    { in: (6..10),
      too_long: \"Label length must be no more than ten characters. \\\"%{value}\\\" is too long\",
      too_short: \"Label length must be at least six characters. \\\"%{value}\\\" is too short\",
      allow_blank: true}"
   "validates :label, length: 
    { is: 5,
      too_long: \"Label length must be five characters. \\\"%{value}\\\" is too long\",
      too_short: \"Label length must be five characters. \\\"%{value}\\\" is too short\",
      allow_blank: true}"
   "validates :status, inclusion: 
    { in: [\"good\", \"bad\", \"ugly\", nil],
      message: \"Status value must be one of \\\"good\\\", \\\"bad\\\", or \\\"ugly\\\". \\\"%{value}\\\" is not in there\",
      allow_blank: true}"))

(define-test :validations "validates_with methods are written correctly"
  (with-new-schema
      (let ((*auto-create-lookups* nil))
        (define-entity ("Anything")
            :attributes
          (("Number" :type child-age :nullable? t)
           ("Label" :type text
                    :constraints ((:expression ($when ($>= Number 10)
                                                      ($or ($length 5)
                                                           ($length-between 6 10)))
                                               :event :update :message "weird constraint...")
                                  (:expression ($length-lt 17) :message "tl;dr")))
           ("Status" :type label :domain ("good" "bad" "ugly")
                     :constraints (($when ($eql "good") ($!= Number 2))))))
        (resolve-constraint-objects nil)
        (let ((ruby::*include-rails* t))
          (mapcar #'ror::format-model-validation
                  (append (constraints (find-field :Number :anything))
                          (constraints (find-field :Label :anything))
                          (constraints (find-field :Status :anything)))))))
  '("validates :number, numericality: 
    { in: (0..18),
      message: \"A child must be between 0 and 18 years of age\",
      allow_blank: true}"
   "validates :label, length: 
    { maximum: 16,
      message: \"tl;dr\",
      allow_blank: true}"
   "
validate :when_greater_than_or_equal_to_number_10_one_is_true_length_is_label_5__length_is_between_label_6__10, on: :update
def when_greater_than_or_equal_to_number_10_one_is_true_length_is_label_5__length_is_between_label_6__10
  unless number.blank? || label.blank?
    if ((number >= 10) && (!((label.length == '5')) && !(label.length.between?('6', '10'))))
      errors.add(:label, \"weird constraint...\")
    end
  end
end
"
   "
validate :when_equal_status_good__not_equal_number_2_
def when_equal_status_good__not_equal_number_2_
  unless status.blank? || number.blank?
    if ((status == 'good') && (number == 2))
      errors.add(:status, \"The check to ensure that TRUE if any of Anything Status does not equal \\\"good\\\" and Anything Number does not equal 2 is TRUE has failed\")
    end
  end
end
"
   "validates :status, inclusion: 
    { in: [\"good\", \"bad\", \"ugly\", nil],
      message: \"Status value must be one of \\\"good\\\", \\\"bad\\\", or \\\"ugly\\\". \\\"%{value}\\\" is not in there\",
      allow_blank: true}"))

(define-test :validations "other validations are written correctly 3"
  (with-new-schema
      (define-aggregation :parent-dependent
        :name "CorporateStructure"
        :parent (("Company")
                 :attributes
                 ((:entity-code) (:entity-name) :description))
        :child (("Division")
                :attributes
                ((:entity-code) (:entity-name) :description)))
    (define-lookup-table ("PhoneType"))
    (define-entity ("Employee")
      :attributes ((:entity-code) :name-fields)
      :repeated-attributes
      ((("Phone" "Ph. Num" "Phone Number")
        :components
        (("PhoneType" :default "main" :domain (Phonetype Name)
                      :constraints (($not-null) ($unique)))
         (("Number")  :type phone :nullable? nil)
         (("Comment") :type memo :documentation
                      "Any additional free form comments or notes")))))
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
      :rhs-properties (:name "Subordinate" :dependency :independent))
    (ror::conventionalize-app)
    (list
     (mapcar #'ror::declare-model-association (my-roles (find-entity :company)))
     (mapcar #'ror::declare-model-association (my-roles (find-entity :division)))
     (mapcar #'ror::declare-model-association (my-roles (find-entity :employee)))
     (mapcar #'ror::declare-model-association (my-roles (find-entity :phonetype)))
     (mapcar #'ror::declare-model-association (my-roles (find-entity :employeephone)))))
  '(("has_many :staff, class_name: \"Employee\", foreign_key: \"employer_id\""
     "has_many :divisions, dependent: :destroy
  validates_associated :divisions")
    ("belongs_to :operational_manager, class_name: \"Employee\""
     "belongs_to :company")
    ("has_many :subordinates, class_name: \"Employee\", foreign_key: \"manager_id\""
     "belongs_to :manager, class_name: \"Employee\", optional: true"
     "has_many :divisions, foreign_key: \"operational_manager_id\""
     "belongs_to :employer, class_name: \"Company\", optional: true"
     "has_many :employee_phones, dependent: :destroy
  validates_associated :employee_phones
  accepts_nested_attributes_for :employee_phones, allow_destroy: true, reject_if: :all_blank")
    NIL ("belongs_to :employee")))

(define-test :unparsing "attribute referencing expressions can be unparsed"
  (with-new-schema
      (let ((ruby::*include-rails* t))
        (define-entity ("Parent")
            :attributes ((:entity-name) :email
                         ("ChildrenLabel" :type label :formula (Other Value ($eql Index Children))))
            :states (("LotsOfKids" ($>= Children 4))
                     ("IsSpecial" ($eql Name (Other Value ($eql Index 0))))))
        (define-entity ("Child")
            :attributes ((:entity-name) :birthdate
                         ("EmailContact" :type email :formula (MyParent Email)))
            :states (("ParentHasEmail" ($not-null (MyParent Email)))
                     ("AlsoParentHasEmail" ($not-null (Parent Email)))))
        (define-entity ("Other")
            :attributes (("Index" :type integer)
                         ("Value" :type label)))
        (define-relationship ((Parent (1 1))
                              ("has" "have")
                              (Child (0 *)))
            :name ("Family")
            :lhs-properties (:dependency :independent :name "MyParent")
            :rhs-properties (:dependency :dependent))
        (post-parse-project)
        (format nil "~%~{~a~%~}"
                (list (ror::unparse-attribute-references (find-field :Birthdate :Child)  (find-entity :child))
                      (ror::unparse-attribute-references (find-field :EmailContact :Child)  (find-entity :child))
                      (ror::unparse-attribute-references
                       (find-field :email :parent) (lhs (car (relationships (find-entity :child)))))
                      (ror::unparse-attribute-references
                       (find-state (find-entity :parent) :lotsofkids) (find-entity :parent))
                      (ror::unparse-attribute-references
                       (find-state (find-entity :parent) :IsSpecial) (find-entity :parent))
                      (ror::unparse-attribute-references
                       (find-state (find-entity :child) :ParentHasEmail) (find-entity :child))
                      (ror::unparse-attribute-references
                       (find-state (find-entity :child) :AlsoParentHasEmail) (find-entity :child))))))
  "
($LITERAL birth_date)
($LITERAL email_contact)
($LITERAL my_parent.email)
(#$>= ($LITERAL children) 4)
(#$EQL ($LITERAL name) ($LITERAL Other.find_by((index == 0)).value))
(#$NOT-NULL ($LITERAL my_parent.email))
(#$NOT-NULL ($LITERAL my_parent.email))
")




;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
