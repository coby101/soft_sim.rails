;;;===========================================================================
;;;
;;;   directory structure and code associated with generating installation scripts 
;;;
;;;===========================================================================
(defpackage #:simian.rails-generator.implementation
  (:use
   #:cl #:rails-unparser #:unparser #:configuration #:utilities #:entity #:foundation #:interface)
  (:nicknames #:implementation)
  (:export
   #:controller-directory
   #:controller-file-path
   #:create-bash-scripts
   #:css-directory
   #:css-file-path
   #:factories-directory
   #:factories-file-path
   #:db-directory
   #:db-migration-file-path
   #:db-structure-file-path
   #:helper-directory
   #:helper-file-path
   #:javascript-directory
   #:javascript-file-path
   #:layout-directory
   #:layout-file-path
   #:model-directory
   #:model-file-path
   #:routes-file-path
   #:schema-file-path
   #:seed-file-path
   #:task-file-path
   #:test-data-file-path
   ))

(in-package #:implementation)

(defun model-directory ()
  (implementation-subdirectory "ror" "app" "models"))

(defun model-file-path (entity)
  (merge-pathnames (make-pathname :name (if (typep entity 'entity) (snake-case (name entity)) entity)
                                  :type "rb")
                   (model-directory)))

(defun controller-directory (&optional aspect)
  (apply #'implementation-subdirectory
         (list* "ror" "app" "controllers" (when aspect (unparse-namespace aspect :list)))))
(defun controller-file-path (aspect)
  (merge-pathnames
   (make-pathname
    :name (snake-case (strcat (plural (entity aspect)) "_controller"))
    :type "rb")
   (controller-directory aspect)))


(defun db-migration-directory ()
  (implementation-subdirectory "ror" "db" "migrate"))

(defun db-directory ()
  (implementation-subdirectory "ror" "db"))

(defun db-migration-file-path (entity)
  (merge-pathnames
   (make-pathname :name (strcat (timestamp) "_create_" (snake-case (plural entity))) :type "rb")
   (db-migration-directory)))

(defun javascript-directory (&rest sub-dirs)
  (apply #'implementation-subdirectory "ror" "app" "javascript" "packs" sub-dirs))

(defun javascript-file-path (file-name &rest subdirs)
  (merge-pathnames
   (make-pathname :name file-name :type "js")
   (apply #'javascript-directory subdirs)))

(defun css-directory (&rest sub-dirs)
  (apply #'implementation-subdirectory "ror" "app" "assets" "stylesheets" "components" sub-dirs))

(defun css-file-path (file-name &rest subdirs)
  (merge-pathnames
   (make-pathname :name file-name :type "css")
   (apply #'css-directory subdirs)))

(defun helper-directory ()
  (implementation-subdirectory "ror" "app" "helpers"))

(defun helper-file-path (file-name)
  (merge-pathnames
   (make-pathname :name file-name :type "rb")
   (helper-directory)))

(defun layout-directory (aspect)
  (apply #'implementation-subdirectory
         (append '("ror" "app" "views")
                 (unparse-namespace aspect :list)
                 (list (snake-case (plural (entity aspect)))))))

(defun layout-file-path (aspect panel)
  (merge-pathnames
   (make-pathname :name (strcat panel ".html") :type "erb")
   (layout-directory aspect)))

(defun routes-file-path()
  (merge-pathnames
   (make-pathname :name "routes" :type "rb")
   (implementation-subdirectory "ror" "config")))

(defun task-file-path(name)
  (merge-pathnames
   (make-pathname :name name :type "rake")
   (implementation-subdirectory "ror" "lib" "tasks")))

(defun test-data-file-path(name)
  (merge-pathnames
   (make-pathname :name name :type "rb")
   (implementation-subdirectory "ror" "db" "data")))

(defun schema-file-path()
  (merge-pathnames
   (make-pathname :name "schema" :type "rb")
   (db-directory)))

(defun seed-file-path(&optional (name "seeds"))
  (merge-pathnames
   (make-pathname :name name :type "rb")
   (db-directory)))

(defun db-structure-file-path()
  (merge-pathnames
   (make-pathname :name "structure" :type "sql")
   (db-directory)))

(defun factories-directory (&rest sub-dirs)
  (apply #'implementation-subdirectory "ror" "spec" "support" sub-dirs))

(defun factories-file-path (file-name &rest subdirs)
  (merge-pathnames
   (make-pathname :name file-name :type "rb")
   (apply #'factories-directory subdirs)))

;; need to get *javascript-packs* and *css-components* out of this file
(defun create-bash-scripts()
  (let ((simian-dir (pathname->shell-filename (implementation-subdirectory "ror") t))
        (install-dir (pathname->shell-filename (or *installation-directory* (implementation-subdirectory "ror")) t)))
    (with-open-file (script (merge-pathnames (make-pathname :name "clean" :type "sh")
                                             (or *installation-directory* (implementation-subdirectory "ror")))
                            :direction :output :if-exists :supersede)
      (format script "# generated file, DO NOT MODIFY as any changes will be overwritten
# script was written by create-bash-scripts in ror/general.lisp
INS_DIR=~a
BCK_DIR=$INS_DIR/backups/`date +%y%m%d.%H%M`

mkdir -p $INS_DIR/backups/
echo \"*
\" > $INS_DIR/backups/.gitignore

mkdir -p $BCK_DIR
mkdir -p $BCK_DIR/app
mkdir -p $BCK_DIR/app/controllers/
mkdir -p $BCK_DIR/app/models/
mkdir -p $BCK_DIR/app/helpers/
mkdir -p $BCK_DIR/app/javascript/packs/
mkdir -p $BCK_DIR/app/assets/stylesheets/components/
mkdir -p $BCK_DIR/app/views/
mkdir -p $BCK_DIR/config/
mkdir -p $BCK_DIR/config/initializers/
mkdir -p $BCK_DIR/config/locales/
mkdir -p $BCK_DIR/lib/tasks/
mkdir -p $BCK_DIR/db/


# create backups of last installation for recovery and debugging
cp $INS_DIR/app/controllers/*.rb $BCK_DIR/app/controllers/
cp $INS_DIR/app/models/*.rb $BCK_DIR/app/models/
cp $INS_DIR/app/helpers/application_helper.rb $BCK_DIR/app/helpers/
cp -r $INS_DIR/app/views/* $BCK_DIR/app/views/
cp -r $INS_DIR/app/javascript/packs/* $BCK_DIR/app/javascript/packs/
cp -r $INS_DIR/app/assets/stylesheets/components/* $BCK_DIR/app/assets/stylesheets/components/
cp $INS_DIR/db/schema.rb $BCK_DIR/db/
cp $INS_DIR/db/seeds.rb $BCK_DIR/db/
cp $INS_DIR/lib/tasks/* $BCK_DIR/lib/tasks/
cp $INS_DIR/config/initializers/inflections.rb $BCK_DIR/config/initializers/
cp $INS_DIR/config/locales/generated.en.yml $BCK_DIR/config/locales/
cp $INS_DIR/config/routes.rb $BCK_DIR/config/

echo \"backups created in $BCK_DIR\"

# clean up installed files unless they are sure to be overwritten later.
# routes.rb, load.rb, seeds.rb, schema.rb, inflections.rb, application_helper.rb all left alone
rm -rf $INS_DIR/app/controllers/*.rb
rm -rf $INS_DIR/app/models/*.rb
rm -rf $INS_DIR/app/views/*
#echo \"not cleaning up the views directory\"
~{rm -rf $INS_DIR/app/javascript/packs/~a/*~%~}
~{rm -rf $INS_DIR/app/assets/stylesheets/components/~a.css~%~}
" install-dir
  (mapcar #'car *javascript-packs*)
  (mapcar #'car *css-components*)))
    (with-open-file (script (merge-pathnames (make-pathname :name "install" :type "sh")
                                             (or *installation-directory* (implementation-subdirectory "ror")))
                            :direction :output :if-exists :supersede)
      (format script "# generated file, DO NOT MODIFY as any changes will be overwritten
# script was written by create-bash-scripts in ror/general.lisp
SIM_DIR=~a
INS_DIR=~a

cp -r $SIM_DIR/app/* $INS_DIR/app/
cp -r $SIM_DIR/db/* $INS_DIR/db/
cp -r $SIM_DIR/lib/tasks/* $INS_DIR/lib/tasks/
cp $SIM_DIR/config/initializers/inflections.rb $INS_DIR/config/initializers/
cp $SIM_DIR/config/locales/generated.en.yml $INS_DIR/config/locales/
cp $SIM_DIR/config/routes.rb $INS_DIR/config/
" simian-dir install-dir))
    (with-open-file (script (merge-pathnames (make-pathname :name "reinstall" :type "sh")
                                             (or *installation-directory* (implementation-subdirectory "ror")))
                            :direction :output :if-exists :supersede)
      (format script "# generated file, DO NOT MODIFY as any changes will be overwritten
# script was written by create-bash-scripts in ror/general.lisp

INS_DIR=~a

bash $INS_DIR/clean.sh~%
bash $INS_DIR/install.sh~%
bash $INS_DIR/restore.sh~%" install-dir))))

  

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
