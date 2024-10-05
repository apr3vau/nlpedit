(in-package :nlpedit)

(defparameter *stanza-lcodes-plist*
  '(english "en"
    dutch "nl"
    franch "fr"
    german "de"
    simplified-chinese "zh-hans"
    traditional-chinese "zh-hant"))

(defvar *stanza-resources-directory*
  (ensure-directories-exist (merge-pathnames #P"stanza/" *resource-directory*)))

(defparameter *stanza-settings-file*
  (merge-pathnames #P"settings.sexp" *stanza-resources-directory*))
(defparameter *stanza-resource-version* "1.9.0")
(defparameter *stanza-resource-url*
  "https://raw.githubusercontent.com/stanfordnlp/stanza-resources/main")
(defvar *stanza-resources-json* nil)

(defparameter *stanza-processors-names*
  '(dependency               ("depparse" "pos" "lemma" "mwt" "tokenize")
    part-of-speech           ("pos" "mwt" "tokenize")
    named-entity-recognition ("tokenize" "mwt" "ner")))

(defparameter *stanza-model-name-package*
  '(default "default" fast "default_fast" accurate "default_accurate"))

(defvar *stanza-model-names*
  '(dependency default part-of-speech default named-entity-recognition default))

(defvar *stanza-pipelines* nil)

(defmethod model-name ((impl (eql 'stanza)) method)
  (getf *stanza-model-names* method))

(defmethod all-model-names ((impl (eql 'stanza)) method)
  '(default fast accurate))

(defmethod (setf model-name) (value (impl (eql 'stanza)) method)
  (when (ensure-model-exist method value)
    (setf (getf *stanza-model-names* method) value)
    (setf (getf *stanza-pipelines* method) nil)
    (init-analysing-method impl method)))

(defun stanza-resources-json ()
  (or *stanza-resources-json*
      (let ((json-path (merge-pathnames "resources.json" *stanza-resources-directory*)))
        (unless (probe-file json-path)
          (handler-case
              (bt:with-timeout (10)
                (dex:fetch (format nil "~A/resources_~A.json" *stanza-resource-url* *stanza-resource-version*) json-path))
            (error (e)
              (capi:prompt-with-message (format nil "Failed to download resources.json: ~A." e)))))
        (when (probe-file json-path)
          (setf *stanza-resources-json*
                (let ((json:*identifier-name-to-key* #'identity)
                      (json:*json-identifier-name-to-lisp* #'identity))
                  (json:decode-json-from-source json-path)))))))

(defun ensure-model-exist (method model-name)
  (let* ((resources (stanza-resources-json))
         (lang (cdr (assoc (getf *stanza-lcodes-plist* *language*) resources :test #'string=)))
         (processors (getf *stanza-processors-names* method)))
    (loop for processor in processors
          for package = (cdr (assoc processor
                                 (cdr (assoc (getf *stanza-model-name-package* model-name)
                                             (cdr (assoc "packages" lang :test #'string=)) :test #'string=))
                                 :test #'string=))
          always (or (probe-file (stanza-package-filename processor package))
                     (and (stanza-download-package resources processor package)
                          (loop for ((nil . processor) (nil . package))
                                  in (cdr (assoc "dependencies"
                                                 (cdr (assoc package (cdr (assoc processor lang :test #'string=)) :test #'string=))
                                                 :test #'string=))
                                unless (stanza-download-package resources processor package)
                                  do (return)
                                finally (return t)))))))

(defun stanza-package-filename (processor package)
  (merge-pathnames (format nil "~A/~A/~A.pt" (getf *stanza-lcodes-plist* *language*) processor package)
                   *stanza-resources-directory*))

(defun stanza-download-package (resources processor package)
  (let ((destination (ensure-directories-exist (stanza-package-filename processor package))))
    (if (probe-file destination) t
      (let* ((url (cdr (assoc "url" resources :test #'string=)))
             (filename (format nil "~A/~A.pt" processor package))
             (text (make-instance 'capi:title-pane :text (format nil "Downloading ~A..." filename))))
        (setf url (ppcre:regex-replace "{lang}" url (getf *stanza-lcodes-plist* *language*))
              url (ppcre:regex-replace "{resources_version}" url *stanza-resource-version*)
              url (ppcre:regex-replace "{filename}" url filename))
        (mp:process-run-function
         "Download NLP Model" ()
         (lambda ()
           (unwind-protect
               (dex:fetch url destination)
             (capi:apply-in-pane-process text #'capi:abort-dialog))))
        (capi:display-dialog
         (make-instance 'capi:interface :layout (make-instance 'capi:simple-layout :description (list text))))
        (probe-file destination)))))

;; Generic function methods

(defmethod init-analysing-method ((impl (eql 'stanza)) method)
  (ensure-python-installed)
  (pyexec "import stanza")
  (or (getf *stanza-pipelines* method)
      (and (ensure-model-exist method (getf *stanza-model-names* method))
           (setf (getf *stanza-pipelines* method)
                 (let ((table (make-hash-table :test #'equal))
                       (value (getf *stanza-model-name-package* (getf *stanza-model-names* method))))
                   (dolist (processor (getf *stanza-processors-names* method))
                     (setf (gethash processor table) value))
                   (pyeval (format nil "stanza.Pipeline('~A', model_dir='~A', processors=~A, package=None, download_method=None)"
                                   (getf *stanza-lcodes-plist* *language*)
                                   #+mswindows (ppcre:regex-replace-all "\\" (namestring *stanza-resources-directory*) "/")
                                   #+darwin (namestring *stanza-resources-directory*)
                                   (pythonize table))))))))

(defmethod analyse-sentences ((impl (eql 'stanza)) (method (eql 'dependency)) text)
  (init-analysing-method impl method)
  (let ((doc (pycall (getf *stanza-pipelines* method) text)))
    (loop for sentence across (pyslot-value doc 'sentences)
          collect (loop for word across (pyslot-value sentence 'words)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :head (pyslot-value word 'head)
                                           :upos (pyslot-value word 'upos)
                                           :deprel (pyslot-value word 'deprel))))))

(defmethod analyse-sentences ((impl (eql 'stanza)) (method (eql 'part-of-speech)) text)
  (init-analysing-method impl method)
  (let ((doc (pycall (getf *stanza-pipelines* method) text)))
    (loop for sentence across (pyslot-value doc 'sentences)
          collect (loop for word across (pyslot-value sentence 'words)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :upos (pyslot-value word 'upos))))))

(defmethod analyse-sentences ((impl (eql 'stanza)) (method (eql 'named-entity-recognition)) text)
  (init-analysing-method impl method)
  (let ((doc (pycall (getf *stanza-pipelines* method) text)))
    (loop for sentence across (pyslot-value doc 'sentences)
          collect (loop for word across (pyslot-value sentence 'tokens)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :ner (let ((val (pyslot-value word 'ner)))
                                                  (when (not (eq val 0)) val)))))))

(defmethod make-configure-layout ((impl (eql 'stanza)) method annotate)
  (let ((model-option (make-instance
                       'capi:option-pane
                       :title "NLP Model: " :title-position :left
                       :items (all-model-names impl method)
                       :print-function #'string-capitalize
                       :selected-item (model-name impl method)
                       :callback-type :data
                       :selection-callback (lambda (data) (setf (model-name impl method) data))))
        (layout (call-next-method)))
    (setf (capi:layout-description layout)
          (cons model-option (capi:layout-description layout)))))

(capi:define-interface stanza-init-interface ()
  ()
  (:panes
   (default-model-option
    capi:option-pane
    :title "Default model:"
    :items '(default fast accurate)
    :print-function #'string-capitalize
    :selected-item 'default)))

(defun stanza-save-settings ()
  (ensure-directories-exist *stanza-settings-file*)
  (with-open-file (out *stanza-settings-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (prin1 *stanza-model-names* out)))

(defmethod init-implementation ((impl (eql 'stanza)))
  (and (ensure-python-installed)
       (if (probe-file *stanza-settings-file*)
           (let ((plist (with-open-file (in *stanza-settings-file*) (read in))))
             (dolist (method (analysing-methods))
               (setf (model-name impl method) (getf plist method 'default)))
             t)
         (multiple-value-bind (r okp)
             (capi:popup-confirmer (make-instance 'stanza-init-interface) "Setup NLP Editor")
           (when okp
             (with-slots (default-model-option) r
               (stanza-save-settings)
               (dolist (method (analysing-methods))
                 (setf (getf *stanza-model-names* method) (capi:choice-selected-item default-model-option)))
               t))))))

;(ensure-python-installed)
;(init-analysing-method 'stanza 'dependency)
;(setf (model-name 'stanza 'dependency) 'default)