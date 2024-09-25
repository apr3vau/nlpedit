(in-package nlpedit)

;; Variables

(defvar *nlp-implementation* 'stanza)
(defvar *analysing-method* 'dependency)
(defvar *annotating-method* 'dependency-groups)

(defun nlp-implementations ()
  "Get all NLP backend implementations."
  (delete-duplicates
   (mapcar (lambda (i) (second (car (method-specializers i))))
           (generic-function-methods #'parse-sentences))))

(defun analysing-methods ()
  "Get all analysing methods (Dependency parsing, part-of-speech, etc.)"
  (delete-duplicates
   (mapcar (lambda (i) (second (second (method-specializers i))))
           (generic-function-methods #'parse-sentences))))

(defun annotating-methods (analysing-method)
  "Get all annotating methods for ANALYSING-METHOD"
  (loop for method in (generic-function-methods #'annotate-sentences)
        for spec = (method-specializers method)
        when (member (car spec) (list (find-class t) (list 'eql analysing-method))
                     :test #'equal)
          collect (second (second spec))))

;; Structures

(defstruct word id text head upos deprel foreground background)

;; Generic functions

(defgeneric init-analysing-method (implementation analysing-method)
  (:documentation "Initialization function for NLP method METHOD of IMPLEMENTATION

Doing things like loading models here"))

(defgeneric parse-sentences (implementation analysing-method text)
  (:documentation "Parse given sentences in TEXT using ANALYSING-METHOD

Returns a list of sentences, each sentences consist of a list of WORDs"))

(defgeneric annotate-sentences (analysing-method annotating-method sentences)
  (:documentation "Annotate sentences analysed by ANALYSING-METHOD, using coloring method named COLORING-METHOD

Return the sentences which every words styled appropriately"))

(defgeneric make-configure-layout (analysing-method annotating-method)
  (:documentation "Return a CAPI:LAYOUT for configuring certain method

Defining method for this to enable method-specific configuration via GUI")
  (:method (analysing-method annotating-method) (make-instance 'capi:column-layout)))

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
                                   (setf (word-foreground word) 'red2)
                                   (process sub 'red1))
                                  ((deprel-member deprel '("obj" "iobj" "obl"))
                                   (setf (word-foreground word) 'blue1)
                                   (process sub 'blue2))
                                  ((deprel-member deprel '("xcomp" "ccomp"))
                                   (setf (word-foreground word) 'yellow2)
                                   (process sub 'yellow1))
                                  ((deprel-member deprel '(list "parataxis" "orphan" "dislocated" "reparandum"
                                                                "compound" "flat" "fixed" "goeswith"
                                                                "conj"
                                                                "nummod" "appos" "advcl" "amod"))
                                   (setf (word-foreground word) (word-foreground (car tree)))
                                   (process sub default-color))
                                  ((deprel= deprel "punct") t)
                                  (t (setf (word-foreground word) default-color)
                                     (process sub default-color))))))
          (setf (word-foreground (car tree)) 'yellow2)
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
                                   (setf (word-foreground word) 'red2)
                                   (process sub))
                                  ((deprel-member deprel '("obj" "iobj" "obl"))
                                   (setf (word-foreground word) 'blue1)
                                   (process sub))
                                  ((deprel-member deprel '("xcomp" "ccomp"))
                                   (setf (word-foreground word) 'yellow2)
                                   (process sub))
                                  ((deprel-member deprel '(list "parataxis" "orphan" "dislocated" "reparandum"
                                                                "compound" "flat" "fixed" "goeswith"
                                                                "conj"
                                                                "nummod" "appos" "advcl" "amod"))
                                   (setf (word-foreground word) (word-foreground (car tree)))
                                   (process sub))
                                  ((deprel-member deprel '("punct" "discourse")) t)
                                  (t (process sub))))))
          (setf (word-foreground (car tree)) 'yellow2)
          (process tree)))))
  sentences)

(defmethod annotate-sentences ((analysing-method (eql 'part-of-speech)) (method (eql 'all-types)) sentences)
  (dolist (words sentences)
    (dolist (word words)
      (setf (word-foreground word) (part-of-speech-default-color (word-upos word)))))
  sentences)

;; Specific types annotation

(defvar *specific-types-annotating-part-of-speech-types* '("NOUN"))
(defvar *specific-types-annotating-dependency-types* '("nsubj" "ccubj" "ccomp" "xcomp" "obj" "iobj"))

(defvar *specific-types-annotating-style*
  '(:background red2
    :foreground nil))

(defmethod make-configure-layout :around (analysing-method (annotating-method (eql 'specific-types)))
  (let* (fg-custom fg-option bg-custom bg-option)
    (setf fg-custom (make-instance 'color-choice-output
                                   :title "Foreground:" :title-position :left
                                   :color (getf *specific-types-annotating-style* :foreground)
                                   :callback (lambda (color)
                                               (setf (capi:choice-selected-item fg-option) 'custom
                                                     (getf *specific-types-annotating-style* :foreground) color)))
          fg-option (make-instance
                     'capi:option-pane
                     :items '(custom red brown yellow green cyan blue purple)
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

(defmethod make-configure-layout ((analysing-method (eql 'dependency)) (annotating-method (eql 'specific-types)))
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

(defmethod make-configure-layout ((analysing-method (eql 'part-of-speech)) (annotating-method (eql 'specific-types)))
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