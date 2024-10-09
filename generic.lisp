(in-package nlpedit)

;; Variables

(defvar *language* 'english)
(defparameter *languages*
  '(english dutch french german simplified-chinese traditional-chinese))
(defvar *resource-directory* (merge-pathnames #P"NLPEdit/" (sys:get-folder-path :appdata)))
(defvar *settings-file* (merge-pathnames "settings.sexp" *resource-directory*))

(defvar *model-quality* 'default)
(defparameter *model-qualities* '(default fast accurate))

#+darwin
(setf (environment-variable "PYTHONHOME") (namestring (merge-pathnames "py/" *resource-directory*))
      (environment-variable "PYTHONPATH") (namestring (merge-pathnames "py/" *resource-directory*)))
(setf (config-var 'py4cl2:pycmd) (namestring (merge-pathnames #+mswindows "py/python.exe"
                                                              #+darwin "py/bin/python3.10"
                                                              *resource-directory*)))

(defvar *nlp-implementation* 'stanza)
(defvar *analysing-method* 'dependency)
(defvar *annotating-method* 'dependency-groups)

(defun nlp-implementations ()
  "Get all NLP backend implementations."
  (delete-duplicates
   (mapcar (lambda (i) (second (first (method-specializers i))))
           (generic-function-methods #'analyse-sentences))))

(defun analysing-methods ()
  "Get all analysing methods (Dependency parsing, part-of-speech, etc.)"
  (delete-duplicates
   (mapcar (lambda (i) (second (second (method-specializers i))))
           (generic-function-methods #'analyse-sentences))))

(defun annotating-methods (analysing-method)
  "Get all annotating methods for ANALYSING-METHOD"
  (loop for method in (generic-function-methods #'annotate-sentences)
        for spec = (method-specializers method)
        when (member (car spec) (list (find-class t) (list 'eql analysing-method))
                     :test #'equal)
          collect (second (second spec))))

(defun save-settings ()
  (with-open-file (out *settings-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (loop for sym in '(*language*
                       *model-quality* *nlp-implementation* *analysing-method* *annotating-method*
                       *font-family* *font-size* *font-weight* *font-slant*
                       *specific-types-annotating-style*
                       *specific-types-annotating-dependency-types*
                       *specific-types-annotating-part-of-speech-types*)
          nconc (list sym (symbol-value sym)) into r
          finally (prin1 r out))))

(defun restore-settings ()
  (when (probe-file *settings-file*)
    (let ((plist (with-open-file (in *settings-file*) (read in))))
      (loop for (sym val) on plist by #'cddr
            do (setf (symbol-value sym) val)))))

(defun load-settings ()
  (ensure-directories-exist *resource-directory*)
  #+darwin
  (setf (environment-variable "PYTHONHOME") (namestring (merge-pathnames "py/" *resource-directory*))
        (environment-variable "PYTHONPATH") (namestring (merge-pathnames "py/" *resource-directory*)))
  (setf (config-var 'py4cl2:pycmd) (namestring (merge-pathnames #+mswindows "py/python.exe"
                                                                #+darwin "py/bin/python3.10"
                                                                *resource-directory*)))
  (handler-case
      (if (probe-file *settings-file*)
          (restore-settings)
        (progn
          (capi:popup-confirmer
           (make-instance 'nlp-configure-interface) "Setup"
           :title "Setup NLP Editor" :cancel-button nil)
          (unless (install-dependencies)
            (capi:display-message "Dependency installation failed~%The application cannot continue.")
            (quit))
          (save-settings)
          (init-analysing-method *nlp-implementation* *analysing-method*)))
    (error (e)
      (if (delivered-image-p)
        (progn
          (capi:display-message "There is an error while application initialize:~%~A~%The application cannot continue." e)
          (quit))
        (invoke-debugger e)))))

;; Structures

(defstruct word id text head upos deprel ner foreground background)

;; Generic functions

(defgeneric init-analysing-method (implementation analysing-method)
  (:documentation "Initialization function for NLP method METHOD of IMPLEMENTATION

Doing things like loading models here"))

(defgeneric analyse-sentences (implementation analysing-method text)
  (:documentation "Parse given sentences in TEXT using ANALYSING-METHOD

Returns a list of sentences, each sentences consist of a list of WORDs"))

(defgeneric annotate-sentences (analysing-method annotating-method sentences)
  (:documentation "Annotate sentences analysed by ANALYSING-METHOD, using coloring method named COLORING-METHOD

Return the sentences which every words styled appropriately"))

(defgeneric make-configure-layout (implementation analysing-method annotating-method)
  (:documentation "Return a CAPI:LAYOUT for configuring certain method

Defining method for this to enable method-specific configuration via GUI")
  (:method (impl analysing-method annotating-method) (make-instance 'capi:column-layout)))

;; Methods

(defmethod annotate-sentences ((analysing-method (eql 'dependency)) (method (eql 'dependency-groups)) sentences)
  (dolist (words sentences)
    (let ((conses (mapcar (lambda (i) (list i)) words)))
      (loop for i in words
            for pos = (position (word-id i) conses :key (lambda (i) (word-id (car i))))
            for head-pos = (position (word-head i) conses :key (lambda (i) (word-id (car i))))
            unless (= (word-head i) 0)
              do (setf (nth head-pos conses) (nconc (nth head-pos conses) (list (nth pos conses)))))
      (let ((tree (find-if (lambda (i) (= (word-head (car i)) 0)) conses)))
        (labels ((process (tree default-color)
                   (loop for sub in (rest tree)
                         for word = (car sub)
                         for deprel = (word-deprel word)
                         do (cond ((deprel-member deprel '("nsubj" "csubj"))
                                   (setf (word-foreground word) 'emphasis-red)
                                   (process sub 'red))
                                  ((deprel-member deprel '("obj" "iobj" "obl"))
                                   (setf (word-foreground word) 'emphasis-blue)
                                   (process sub 'blue))
                                  ((deprel-member deprel '("xcomp" "ccomp"))
                                   (setf (word-foreground word) 'emphasis-yellow)
                                   (process sub 'yellow))
                                  ((deprel-member deprel '(list "parataxis" "orphan" "dislocated" "reparandum"
                                                                "compound" "flat" "fixed" "goeswith"
                                                                "conj"
                                                                "nummod" "appos" "advcl" "amod"))
                                   (setf (word-foreground word) (word-foreground (car tree)))
                                   (process sub default-color))
                                  ((deprel= deprel "punct") t)
                                  (t (setf (word-foreground word) default-color)
                                     (process sub default-color))))))
          (setf (word-foreground (car tree)) 'emphasis-yellow)
          (process tree nil)))))
  sentences)

(defmethod annotate-sentences ((analysing-method (eql 'dependency)) (method (eql 'core-dependencies)) sentences)
  (dolist (words sentences)
    (let ((conses (mapcar (lambda (i) (list i)) words)))
      (loop for i in words
            for pos = (position (word-id i) conses :key (lambda (i) (word-id (car i))))
            for head-pos = (position (word-head i) conses :key (lambda (i) (word-id (car i))))
            unless (= (word-head i) 0)
              do (setf (nth head-pos conses) (nconc (nth head-pos conses) (list (nth pos conses)))))
      (let ((tree (find-if (lambda (i) (= (word-head (car i)) 0)) conses)))
        (labels ((process (tree)
                   (loop for sub in (rest tree)
                         for word = (car sub)
                         for deprel = (word-deprel word)
                         do (cond ((deprel-member deprel '("nsubj" "csubj"))
                                   (setf (word-foreground word) 'emphasis-red)
                                   (process sub))
                                  ((deprel-member deprel '("obj" "iobj" "obl"))
                                   (setf (word-foreground word) 'emphasis-blue)
                                   (process sub))
                                  ((deprel-member deprel '("xcomp" "ccomp"))
                                   (setf (word-foreground word) 'emphasis-yellow)
                                   (process sub))
                                  ((deprel-member deprel '(list "parataxis" "orphan" "dislocated" "reparandum"
                                                                "compound" "flat" "fixed" "goeswith"
                                                                "conj"
                                                                "nummod" "appos" "advcl" "amod"))
                                   (setf (word-foreground word) (word-foreground (car tree)))
                                   (process sub))
                                  ((deprel-member deprel '("punct" "discourse")) t)
                                  (t (process sub))))))
          (setf (word-foreground (car tree)) 'emphasis-yellow)
          (process tree)))))
  sentences)

(defmethod annotate-sentences ((analysing-method (eql 'part-of-speech)) (method (eql 'all-types)) sentences)
  (dolist (words sentences)
    (dolist (word words)
      (setf (word-foreground word) (part-of-speech-default-color (word-upos word)))))
  sentences)

(defmethod make-configure-layout :around (impl analysing-method (annotating-method (eql 'all-entities)))
  (let* (fg-custom fg-option bg-custom bg-option)
    (setf fg-custom (make-instance 'color-choice-output
                                   :title "Foreground:" :title-position :left
                                   :color (getf *specific-types-annotating-style* :foreground)
                                   :callback (lambda (color)
                                               (setf (capi:choice-selected-item fg-option) 'custom
                                                     (getf *specific-types-annotating-style* :foreground) color)))
          fg-option (make-instance
                     'capi:option-pane
                     :items '(custom nil red brown yellow green cyan blue purple)
                     :print-function #'string-capitalize
                     :callback-type :data
                     :selection-callback
                     (lambda (data)
                       (case data
                         (custom
                          (setf (slot-value fg-custom 'color) (getf *specific-types-annotating-style* :foreground))
                          (gp:invalidate-rectangle fg-custom))
                         (t (let ((c (color:get-color-translation (intern (string-append (symbol-name data) "2") "NLPEDIT"))))
                              (setf (getf *specific-types-annotating-style* :foreground) c
                                    (slot-value fg-custom 'color) c)
                              (gp:invalidate-rectangle fg-custom))))))
          bg-custom (make-instance 'color-choice-output
                                   :title "Background:" :title-position :left
                                   :color (getf *specific-types-annotating-style* :background)
                                   :callback (lambda (color)
                                               (setf (capi:choice-selected-item bg-option) 'custom
                                                     (getf *specific-types-annotating-style* :background) color)))
          bg-option (make-instance
                     'capi:option-pane
                     :items '(custom nil red brown yellow green cyan blue purple)
                     :print-function #'string-capitalize
                     :callback-type :data
                     :selection-callback
                     (lambda (data)
                       (case data
                         (custom
                          (setf (slot-value bg-custom 'color) (getf *specific-types-annotating-style* :background))
                          (gp:invalidate-rectangle bg-custom))
                         (t (let ((c (color:get-color-translation (intern (string-append (symbol-name data) "2") "NLPEDIT"))))
                              (setf (getf *specific-types-annotating-style* :background) c
                                    (slot-value bg-custom 'color) c)
                              (gp:invalidate-rectangle bg-custom)))))))
    (make-instance 'capi:column-layout
                   :description (list (call-next-method)
                                      (make-instance 'capi:row-layout
                                                     :description (list fg-custom fg-option))
                                      (make-instance 'capi:row-layout
                                                     :description (list bg-custom bg-option))))))

(defmethod annotate-sentences ((analysing-method (eql 'named-entity-recognition)) (method (eql 'all-entities)) sentences)
  (dolist (words sentences)
    (dolist (word words)
      (when (word-ner word)
        (setf (word-foreground word) (getf *specific-types-annotating-style* :foreground)
              (word-background word) (getf *specific-types-annotating-style* :background)))))
  sentences)

;; Specific types annotation

(defvar *specific-types-annotating-part-of-speech-types* '("NOUN"))
(defvar *specific-types-annotating-dependency-types* '("nsubj" "ccubj" "ccomp" "xcomp" "obj" "iobj"))

(defvar *specific-types-annotating-style*
  '(:foreground light-red
    :background nil))

(defmethod make-configure-layout :around (impl analysing-method (annotating-method (eql 'specific-types)))
  (let* (fg-custom fg-option bg-custom bg-option)
    (setf fg-custom (make-instance 'color-choice-output
                                   :title "Foreground:" :title-position :left
                                   :color (getf *specific-types-annotating-style* :foreground)
                                   :callback (lambda (color)
                                               (setf (capi:choice-selected-item fg-option) 'custom
                                                     (getf *specific-types-annotating-style* :foreground) color)))
          fg-option (make-instance
                     'capi:option-pane
                     :items '(custom nil red orange yellow green cyan blue purple)
                     :print-function #'string-capitalize
                     :callback-type :data
                     :selection-callback
                     (lambda (data)
                       (case data
                         (custom
                          (setf (slot-value fg-custom 'color) (getf *specific-types-annotating-style* :foreground))
                          (gp:invalidate-rectangle fg-custom))
                         (t (let ((c (color:get-color-translation (intern (string-append (symbol-name data) "2") "NLPEDIT"))))
                              (setf (getf *specific-types-annotating-style* :foreground) c
                                    (slot-value fg-custom 'color) c)
                              (gp:invalidate-rectangle fg-custom))))))
          bg-custom (make-instance 'color-choice-output
                                   :title "Background:" :title-position :left
                                   :color (getf *specific-types-annotating-style* :background)
                                   :callback (lambda (color)
                                               (setf (capi:choice-selected-item bg-option) 'custom
                                                     (getf *specific-types-annotating-style* :background) color)))
          bg-option (make-instance
                     'capi:option-pane
                     :items '(custom nil red brown yellow green cyan blue purple)
                     :print-function #'string-capitalize
                     :callback-type :data
                     :selection-callback
                     (lambda (data)
                       (case data
                         (custom
                          (setf (slot-value bg-custom 'color) (getf *specific-types-annotating-style* :background))
                          (gp:invalidate-rectangle bg-custom))
                         (t (let ((c (color:get-color-translation (intern (string-append (symbol-name data) "2") "NLPEDIT"))))
                              (setf (getf *specific-types-annotating-style* :background) c
                                    (slot-value bg-custom 'color) c)
                              (gp:invalidate-rectangle bg-custom)))))))
    (make-instance 'capi:column-layout
                   :description (list (call-next-method)
                                      (make-instance 'capi:row-layout
                                                     :description (list fg-custom fg-option))
                                      (make-instance 'capi:row-layout
                                                     :description (list bg-custom bg-option))))))

(defmethod make-configure-layout (impl (analysing-method (eql 'dependency)) (annotating-method (eql 'specific-types)))
  (make-instance
   'capi:column-layout
   :description
   (list (make-instance
          'capi:column-layout
          :title "Dependencies to highlight" :title-position :frame              
          :description
          (loop for (type . lst) in *universal-dependency-tags*
                collect (make-instance
                         'capi:check-button-panel
                         :title (string-append type ":") :title-position :left
                         :items (mapcar #'car lst) :test-function #'equal
                         :selected-items (intersection (mapcar #'car lst) *specific-types-annotating-dependency-types*
                                                       :test #'equal)
                         :callback-type :element
                         :selection-callback (lambda (self)
                                               (setf *specific-types-annotating-dependency-types*
                                                     (capi:choice-selected-items self)))
                         :retract-callback (lambda (self)
                                             (setf *specific-types-annotating-dependency-types*
                                                   (capi:choice-selected-items self)))))))))

(defmethod make-configure-layout (impl (analysing-method (eql 'part-of-speech)) (annotating-method (eql 'specific-types)))
  (make-instance
   'capi:column-layout
   :description
   (list (make-instance
          'capi:check-button-panel
          :title "Part-of-speech Type" :title-position :frame
          :items (mapcar #'car *universal-pos-tags*) :test-function #'equal
          :selected-items (intersection (mapcar #'car *universal-pos-tags*)
                                        *specific-types-annotating-part-of-speech-types*
                                        :test #'equal)
          :layout-class 'capi:grid-layout :layout-args '(:columns 5)
          :callback-type :element
          :selection-callback (lambda (self)
                                (setf *specific-types-annotating-part-of-speech-types*
                                      (capi:choice-selected-items self)))
          :retract-callback (lambda (self)
                              (setf *specific-types-annotating-part-of-speech-types*
                                    (capi:choice-selected-items self)))))))

(defmethod annotate-sentences ((analysing-method (eql 'dependency)) (method (eql 'specific-types)) sentences)
  (dolist (words sentences)
    (dolist (word words)
      (when (deprel-member (word-deprel word) *specific-types-annotating-dependency-types*)
        (setf (word-foreground word) (getf *specific-types-annotating-style* :foreground)
              (word-background word) (getf *specific-types-annotating-style* :background)))))
  sentences)

(defmethod annotate-sentences ((analysing-method (eql 'part-of-speech)) (method (eql 'specific-types)) sentences)
  (dolist (words sentences)
    (dolist (word words)
      (when (member (word-upos word) *specific-types-annotating-part-of-speech-types* :test #'equal)
        (setf (word-foreground word) (getf *specific-types-annotating-style* :foreground)
              (word-background word) (getf *specific-types-annotating-style* :background)))))
  sentences)