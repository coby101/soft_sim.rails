(in-package #:model-tests)

(defun model-test-application ()
  (let ((*application* (test-application)))
  ;; add any specific application features for model tests
    *application*))

(defun division-status-meta-data ()
  '((:attributes
     (("status"
       ((:logical_type :|label|)
	(:domain ((:domain_type :user_controlled) (:data_type :string)))
	(:default nil) (:nullable? simian.rails-generator.model::false)))))
    (:read_only? t) (:keep_history? simian.rails-generator.model::false)
    (:select_display_method nil) (:select_data nil) (:children nil) (:parents nil)))

(defun division-status-meta-data-hash ()
  "{
  :attributes =>
  {
    :status =>
    {
      :logical_type => :label,
      :domain =>
      {
        :domain_type => :user_controlled,
        :data_type => :string
      },
      :default => nil,
      :nullable? => false
    }
  },
  :read_only? => true,
  :keep_history? => false,
  :select_display_method => nil,
  :select_data => nil,
  :children => nil,
  :parents => nil
}"
   )


(defun division-status-meta-data-method ()
  "
  def self.meta_data
    {
      :attributes =>
      {
        :status =>
        {
          :logical_type => :label,
          :domain =>
          {
            :domain_type => :user_controlled,
            :data_type => :string
          },
          :default => nil,
          :nullable? => false
        }
      },
      :read_only? => true,
      :keep_history? => false,
      :select_display_method => nil,
      :select_data => nil,
      :children => nil,
      :parents => nil
    }
  end
")

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
