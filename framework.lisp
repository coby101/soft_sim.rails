;;;===========================================================================
;;; file:   framework.lisp
;;; auth:   Coby Beck
;;; date:   2021-09-14
;;; update: 
;;;---------------------------------------------------------------------------
;;;   Code for generating framework page layouts, routes and controllers
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(defun framework_controller.rb ()
  (let ((file (merge-pathnames
               (make-pathname :name "framework_controller"
                              :type "rb")
               (controller-directory)))
        (*nesting-level* 0))
    (with-open-file (cont-file file :direction :output :if-exists :supersede)
      (format-file-notice cont-file "framework_controller.rb")
      (framework-controller-definition cont-file))))

(defmethod framework-controller-definition (&optional (stream t))
  (format stream (controller-class-declaration "FrameworkController"))
  (terpri stream) (terpri stream)
  (with-nesting
    (framework-home-method stream) (terpri stream)    
    (framework-denied-method stream) (terpri stream)    
    (framework-about-method stream) (terpri stream)
    (framework-help-method stream) (terpri stream))
  (format stream "end~%"))

(defun framework-home-method (&optional (stream t))
  (princ (make-indent) stream)
  (format stream (ruby:unparse-method "home" nil)))

(defun framework-about-method (&optional (stream t))
  (princ (make-indent) stream)
  (format stream (ruby:unparse-method "about" nil)))

(defun framework-denied-method (&optional (stream t))
  (princ (make-indent) stream)
  (format stream (ruby:unparse-method "denied" nil)))

(defun framework-help-method (&optional (stream t))
  (princ (make-indent) stream)
  (format stream (ruby:unparse-method "help" nil)))

(defun framework-layouts ()
  (let ((file (merge-pathnames
               (make-pathname :name "home.html" :type "erb")
               (implementation-subdirectory "ror" "app" "views" "framework"))))
    (with-open-file (home.erb file :direction :output :if-exists :supersede)
      (format home.erb (unparse-erb nil (format-file-notice nil "framework-layouts")))
      (write-home-layout home.erb))))

(defun unparse-application-space (&optional stream)
  (format stream
          (html:ltag nil
                      (mapcar #'(lambda (aspect)
                                  (unparse-template-link aspect :list
                                     :label (format nil "'~a'" (escape-characters (long-name (view aspect)) #\'))))
                              (mapcar #'car (mapcar #'reverse (mapcar #'aspects (views *application*))))))))

(defun unparse-space (space &optional stream)
  (let ((views (views space))
        (sub-spaces (sub-spaces space)))
    (format stream "~%~a~a" (make-indent)
            (html:button (long-name space) :class "collapsible"))
    (format stream "~%~a"
            (html:div
             (if (and (null views) (null sub-spaces))
                 (with-nesting (html:div "(this space has no content)" :class "menuitem"))
                 (with-nesting
                     (with-output-to-string (str)
                       (when views
                         (let ((fmt-str (format nil "~~{~%~a~~a~~^<br>~~}~%" (make-indent))))
                           (format str fmt-str
                                   (loop for view in views
                                         collect
                                         (unparse-template-link
                                          (car (last (aspects view))) :list
                                          :label (format nil "'~a'" (escape-characters (long-name view) #\'))
                                          :css-class "menuitem")))))
                       (when sub-spaces
                         (with-nesting
                             (dolist (sub sub-spaces)
                               (unparse-space sub str)))))))
             :class "content"))))

(defun unparse-application-space (&optional stream)
  (let ((top-level-spaces (remove-if #'parent-space (spaces *application*)))) 
    ;; (when *dev-mode*
    ;;   (setf top-level-spaces
    ;;         (append top-level-spaces
    ;;                  (list (make-developer-space)))))
    (dolist (space top-level-spaces)
      (unparse-space space stream)))
  (format stream "
<script>
var coll = document.getElementsByClassName(\"collapsible\");
var i;

for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener(\"click\", function() {
    this.classList.toggle(\"active\");
    var content = this.nextElementSibling;
    if (content.style.display === \"block\") {
      content.style.display = \"none\";
    } else {
      content.style.display = \"block\";
    }
  });
}
</script>"))

(defun application-banner ()
  (html:div (strcat "<hr>"
                     (format nil (html:heading 1 (long-name *application*)))
                     "<hr>")
             :id "app-banner"))

(defun write-home-layout (&optional (stream t))
  (format stream (unparse-erb t "image_tag \"logo_customcolor_background.png\", width: \"600px\"")))

(defun default-layout-header()
  (format nil "
<!DOCTYPE html>~%<html>~%  <head>~%    <title>~a</title>
    <meta name=\"robots\" content=\"noindex,nofollow\" />
    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
    <meta name=\"app-timestamp\" content=\"~a\">
    <%= csrf_meta_tags %>~%    <%= csp_meta_tag %>~%
    <%= stylesheet_link_tag 'application', media: 'all', 'data-turbolinks-track': 'reload' %>
    <%= javascript_pack_tag 'application', 'data-turbolinks-track': 'reload' %>
    <script src=\"https://maps.googleapis.com/maps/api/js?key=<%=ENV['GOOGLE_MAP_API']%>&libraries=places&callback=initMap\" defer data-turbolinks-eval=\"reload\"></script>
  </head>~%" (long-name *application*)
  (local-time:format-timestring nil (local-time:now) :format *documentation-timestamp-format*)))

(defun default-layout-footer()
  (let ((logout (if *authenticated-application?* (format nil "~%<br>~a~%<br>~a" (current-user-display) (logout-link)) "")))
    (format nil "<hr>~%~a~a~%</body>~%</html>~%" (erb-home-page-link) logout)))

(defun default-layout-sidebar()
  "<div id=\"sidebar\"></div>"
  "")



(setf *custom-application.html.erb* (format nil "
<!DOCTYPE html>
<html>
  <head>
    <title>Project Cost Management Database</title>
    <meta name=\"robots\" content=\"noindex,nofollow\" />
    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">
    <meta name=\"app-timestamp\" content=\"Thursday January 27, 2022 at 12:58:11 AEDT\">
    <%= csrf_meta_tags %>
    <%= csp_meta_tag %>
    <%= stylesheet_link_tag 'application', media: 'all', 'data-turbolinks-track': 'reload' %>
    <%= stylesheet_link_tag \"tailwind\" %>
    <%= javascript_include_tag 'application', 'data-turbolinks-track': 'reload' %>
    <%= javascript_pack_tag 'application', 'data-turbolinks-track': 'reload' %>
    <script src=\"https://maps.googleapis.com/maps/api/js?key=<%=ENV['GOOGLE_MAP_API']%>&libraries=places&callback=initMap\" defer data-turbolinks-eval=\"reload\"></script>
    <%# <link href=\"https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css\" rel=\"stylesheet\"> %>
    <%= yield :css %>
     <style>
      #first-level-nav a.text-gray-300:hover,
      #second-level-nav a.text-gray-300:hover {
        background-color: #47D7AC !important;
        border-radius: 6px !important;
        font-weight: 500 !important;
      }

      #first-level-nav a.active,
      #second-level-nav a.active {
        background: #F3F4F6;
        color: #111827 !important;
        font-weight: 500 !important;
      }

      .bg-footer-green{
        /* background: #346B5C; */
        background: var(--cloaked-sky-color);
        color: #F8F8F8;
      }

      .bg-purple-900{
        background: #5840B0 !important;
      }
    </style>
  </head>
  <body>
  <div class=\"min-h-full\">
    <%= render 'shared/first_level_header' %>
    <%= render 'shared/second_level_header' %>
    <main>
      <div class=\"max-w-7xl mx-auto py-6 sm:px-6 lg:px-8\">
        <%= render 'shared/flashes'%>
        <%= yield %>
      </div>
    </main>
    <%= render 'shared/footer' %>
  </div>
  </body>
</html>" (local-time:format-timestring nil (local-time:now) :format *documentation-timestamp-format*)))

(defun default-layouts ()
  (let ((file (merge-pathnames
               (make-pathname :name "application.html" :type "erb")
               (implementation-subdirectory "ror" "app" "views" "layouts"))))
    (with-open-file (html.erb file :direction :output :if-exists :supersede)
      (if *custom-application.html.erb*
          (format html.erb *custom-application.html.erb*)
          (progn
            (format html.erb (unparse-erb nil (format-file-notice nil "default-layouts")))
            (format html.erb (default-layout-header))
            (format html.erb "~%  <body><div class=\"container wrapper\">~%")
            (format html.erb "~a~%~a~%" (application-banner) (erb-home-page-link))
            (format html.erb "<div class=\"wrapper\">~%  <div id=\"menubar\">~%    <div id=\"menulist\">")
            (unparse-application-space html.erb)
            (format html.erb "    </div>~%  </div>")
            (format html.erb "~%  <div id=\"main\"> <%= yield %></div>~%</div>~%")
            (format html.erb (default-layout-sidebar))
            (format html.erb (default-layout-footer))))))
  (let ((file (merge-pathnames
               (make-pathname :name "mailer.txt" :type "erb")
               (implementation-subdirectory "ror" "app" "views" "layouts"))))
    (with-open-file (txt.erb file :direction :output :if-exists :supersede)
      (if *custom-mailer.txt.erb*
          (format txt.erb *custom-mailer.txt.erb*)
          (progn
            (format txt.erb (unparse-erb nil (format-file-notice nil "default-layouts")))
            (format txt.erb "
<%= yield %>
")))))
  (let ((file (merge-pathnames
               (make-pathname :name "mailer.html" :type "erb")
               (implementation-subdirectory "ror" "app" "views" "layouts"))))
    (with-open-file (html.erb file :direction :output :if-exists :supersede)
      (if *custom-mailer.html.erb*
          (format html.erb *custom-mailer.html.erb*)
          (progn
            (format html.erb (unparse-erb nil (format-file-notice nil "default-layouts")))
            (format html.erb "
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    <style>
      /* Email styles need to be inline */
    </style>
  </head>

  <body>
    <%= yield %>
  </body>
</html>
"))))))

(defun erb-home-page-link()
  "<%= link_to 'Application Home Page', root_path %>")

(defun framework-routes ()
  (append
   (if *authenticated-application?*
       (list (format nil "devise_for :~a" (schema-name (find-entity (keywordify *user-model*))))
             "root to: 'framework#home'")
       (list "get '/', to: 'framework#home', as: 'home'"))
   (list "get '/no_access', to: 'framework#denied'"
         "get '/about', to: 'framework#about', as: 'about'"
         "mount ActionCable.server => '/cable'")))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
