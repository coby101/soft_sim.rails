;;;====================================================
;;;
;;;   code associated with generating Rails tasks
;;;
;;;====================================================

(in-package :app-tasks)

(defun data-sets (arg) arg "WIP - see soft_sim data-sets.lisp")

(defun write-data-load-tasks()
  (when (data-sets *application*)
    (let ((file (task-file-path "test_data")))
      (with-open-file (task file :direction :output :if-exists :supersede)
        (format-file-notice task "write-data-load-tasks")
        (format task "~%namespace :db do")
        (dolist (dataset (data-sets *application*))
          (let ((name (snake-case (name dataset))))
            (format task "
  desc ~s
  task :~a => :environment do
    if ENV['TEST_ENVIRONMENT'] == \"true\"
      filename = Rails.root.join('db', 'data', '~a.rb')
      load(filename) if File.exist?(filename)
    end
  end~%" (description dataset) name name)))

        (format task "
  desc \"Default test data set\"
  task :test_data => :environment do
    if ENV['TEST_ENVIRONMENT'] == \"true\"
      filename = Rails.root.join('db', 'test_data.rb')
      load(filename) if File.exist?(filename)
    end
  end

  desc \"Add Test Dataset\"
  task :staging_test_data => :environment do
    if ENV['REDIRECT_URL'].present? && ENV['REDIRECT_URL'].include?('pcmd-staging')
      filename = Rails.root.join('db', 'data', 'test_data.rb')
      load(filename) if File.exist?(filename)
    end
  end~%")
        (format task "end~%")))))

(defun create-test-data-file (data-set)
  (declare (ignorable data-set))
  "WIP - see soft_sim data-sets.lisp")

(defun create-test-data ()
  (mapcar #'create-test-data-file (data-sets *application*)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:

