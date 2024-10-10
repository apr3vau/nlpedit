(in-package nlpedit)

(defvar *python-url*
  #+mswindows "https://github.com/apr3vau/nlpedit/releases/download/python/py310-win.zip"
  #+darwin "https://github.com/apr3vau/nlpedit/releases/download/python/py310-mac.zip")

(defparameter *python-packages*
  '(("stanza" . "1.9.2")
    ("transformers" . "4.45.1")
    ("numpy" . "1.25.2")))

(defun pip-install (package version)
  (let ((site (merge-pathnames #+mswindows "py/Lib/site-packages/"
                               #+darwin "py/lib/python3.10/site-packages/"
                               *resource-directory*)))
    #+mswindows
    (sys:run-shell-command
     (list "cmd" "/c" (namestring (merge-pathnames "py/python.exe" *resource-directory*))
           "-m" "pip" "install" (format nil "~A==~A" package version) "--target" (namestring site) "--upgrade"))
    #+darwin
    (sys:call-system
     (list (namestring (merge-pathnames "py/bin/python3.10" *resource-directory*)) "-m" "pip"
           "install" (format nil "~A==~A" package version) "--target" (namestring site) "--upgrade"))))

(defun install-dependencies ()
  (let (names functions)

    (let* ((python-dir (merge-pathnames "py/" *resource-directory*))
           (python.zip (merge-pathnames "python.zip" *resource-directory*))
           (site (merge-pathnames #+mswindows "Lib/site-packages/"
                                  #+darwin "lib/python3.10/site-packages/"
                                  python-dir)))
      (unless (probe-file python-dir)
        (unless (probe-file python.zip)
          (push-end "Download Python" names)
          (push-end (lambda ()
                      (dex:fetch *python-url* python.zip)
                      (probe-file python.zip))
                    functions))
        (push-end "Install Python" names)
        (push-end (lambda ()
                    (extract-zip python.zip *resource-directory*)
                    (delete-file python.zip)
                    (probe-file python-dir))
                  functions)
        (when (member :darwin *features*)
          (push-end "Install pip" names)
          (push-end (lambda ()
                      (sys:run-shell-command
                       (list (namestring (merge-pathnames "py/bin/python3.10" *resource-directory*)) "-m" "ensurepip"))
                      (probe-file (merge-pathnames "py/bin/pip3" *resource-directory*)))
                  functions)))
      (dolist (name-version *python-packages*)
        (destructuring-bind (name . version) name-version
          (let ((info-dir (merge-pathnames (format nil "~A-~A.dist-info" name version) site)))
            (unless (probe-file info-dir)
              (push-end (string-append "Install Python package: " name) names)
              (push-end (lambda () (pip-install name version) (probe-file info-dir))
                        functions))))))

    (let* ((resources (stanza-resources-json))
           (processors (if (member *language* *languages-have-mwt*)
                           '("tokenize" "mwt" "pos" "lemma" "depparse" "ner")
                         '("tokenize" "pos" "lemma" "depparse" "ner")))
           (lcode (getf *stanza-lcodes-plist* *language*))
           (model-package (getf *stanza-model-quality-name-plist* *model-quality*))
           (lang-field (cdr (assoc lcode resources :test #'string=)))
           (package-field (cdr (assoc model-package (cdr (assoc "packages" lang-field :test #'string=))
                                      :test #'string=)))
           models)
      (dolist (processor processors)
        (let* ((processor-field (cdr (assoc processor lang-field :test #'string=)))
               (model-name (cdr (assoc processor package-field :test #'string=)))
               (model-field (cdr (assoc model-name processor-field :test #'string=))))
          (loop for ((nil . dep-processor) (nil . dep-model-name))
                  in (cdr (assoc "dependencies" model-field :test #'string=))
                do (push-end-new (list dep-processor dep-model-name) models :test #'equal))
          (when model-name
            (push-end-new (list processor model-name) models :test #'equal))))
      (dolist (i models)
        (destructuring-bind (processor model-name) i
          (let* ((url (copy-seq (cdr (assoc "url" resources :test #'string=))))
                 (filename (format nil "~A/~A.pt" processor model-name))
                 (stanza-resources-dir (merge-pathnames "stanza/" *resource-directory*))
                 (destination (merge-pathnames (format nil "~A/~A" lcode filename) stanza-resources-dir)))
            (unless (probe-file destination)
              (push-end (string-append "Download model: " filename) names)
              (push-end (lambda ()
                          (ensure-directories-exist destination)
                          (setf url (ppcre:regex-replace "{lang}" url lcode)
                                url (ppcre:regex-replace "{resources_version}" url stanza-resources-dir)
                                url (ppcre:regex-replace "{filename}" url filename))
                          (dex:fetch url destination)
                          (probe-file destination))
                        functions))))))

    (if names
      (let* ((name-status-array (apply #'vector (mapcar (lambda (i) (vector i "Waiting")) names)))
             (list-panel (make-instance
                          'capi:multi-column-list-panel
                          :columns '((:title "Mission")
                                     (:title "Status"))
                          :column-function (lambda (i) (coerce i 'list))
                          :visible-min-width '(character 70) :visible-min-height '(character 10)
                          :items name-status-array))
             (process (mp:process-run-function
                       "Install Dependencies" ()
                       (lambda ()
                         (loop for i from 0
                               for func in functions
                               do (setf (aref (aref name-status-array i) 1) "🕝 Processing...")
                                  (capi:apply-in-pane-process
                                   (capi:element-interface list-panel)
                                   #'capi:redisplay-collection-item list-panel (aref name-status-array i))
                                  (capi:apply-in-pane-process
                                   (capi:element-interface list-panel)
                                   #'(setf capi:choice-selection) i list-panel)
                                  (setf (aref (aref name-status-array i) 1)
                                        (if (ignore-errors (funcall func)) "✅ Done" "❌ Failed"))
                                  (capi:apply-in-pane-process
                                   (capi:element-interface list-panel)
                                   #'capi:redisplay-collection-item list-panel (aref name-status-array i)))
                         (capi:apply-in-pane-process
                          (capi:element-interface list-panel)
                          (lambda () (capi:exit-confirmer t)))))))
        (multiple-value-bind (result okp)
            (capi:popup-confirmer
             (make-instance 'capi:interface
                            :layout (make-instance 'capi:simple-layout :description (list list-panel)))
             "Installing Dependencies"
             :title "Installing Dependencies for NLP Editor" :ok-button nil)
          (declare (ignore result))
          (if okp t
            (progn
              (when (mp:process-alive-p process)
                (mp:process-terminate process))
              (when (capi:prompt-for-confirmation
                     (format nil "Failed to install some dependencies.~%Try again?"))
                (install-dependencies))))))
      t)))