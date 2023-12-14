;;;===========================================================================
;;;
;;;   general purpose and utility code associated with generating Rails MVC framework 
;;;
;;;===========================================================================

(in-package :ror)

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
