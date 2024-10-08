(in-package :nlpedit)

(defparameter *stanza-lcodes-plist*
  '(english "en"
    dutch "nl"
    franch "fr"
    german "de"
    simplified-chinese "zh-hans"
    traditional-chinese "zh-hant"))

(defvar *stanza-method-processors-plist*
  '(
    dependency ("depparse" "pos" "lemma" "mwt" "tokenize")
    part-of-speech ("pos" "mwt" "tokenize")
    named-entity-recognition ("tokenize" "mwt" "ner")))

(defparameter *stanza-model-quality-name-plist*
  '(default "default" fast "default_fast" accurate "default_accurate"))

(defparameter *stanza-resources-directory*
  (merge-pathnames "stanza/" *resource-directory*))
(defparameter *stanza-resource-version* "1.9.0")
(defparameter *stanza-resource-url*
  "https://raw.githubusercontent.com/stanfordnlp/stanza-resources/main")
(defvar *stanza-resources-json* nil)

(defun stanza-resources-json ()
  (let ((json-path (merge-pathnames (format nil "resources_~A.json" *stanza-resource-version*)
                                        *stanza-resources-directory*)))
    (unless (probe-file json-path)
      (ensure-directories-exist json-path)
      (handler-case
          (bt:with-timeout (10)
            (dex:fetch (format nil "~A/resources_~A.json" *stanza-resource-url* *stanza-resource-version*) json-path))
        (error (e)
          (capi:prompt-with-message (format nil "Failed to download resources.json: ~A.~%Please check your network and try again." e))))
      (copy-file json-path (merge-pathnames "resources.json" *stanza-resources-directory*)))
    (or *stanza-resources-json*
        (setf *stanza-resources-json*
              (let ((json:*identifier-name-to-key* #'identity)
                    (json:*json-identifier-name-to-lisp* #'identity))
                (json:decode-json-from-source json-path))))))

(defvar *stanza-pipelines* nil)

(defstruct pipeline method quality object)

(defun call-pipeline (pipeline text)
  (pycall (pipeline-object pipeline) text))

;; Generic function methods

(defmethod init-analysing-method ((impl (eql 'stanza)) method)
  (let ((pipeline (find method *stanza-pipelines* :key #'pipeline-method)))
    (when pipeline
      (unless (eq (pipeline-quality pipeline) *model-quality*)
        (delete pipeline *stanza-pipelines*)
        (setq pipeline nil)))
    (unless pipeline
      (pyexec "import stanza")
      (let ((table (make-hash-table :test #'equal))
            (value (getf *stanza-model-quality-name-plist* *model-quality*)))
        (dolist (processor (getf *stanza-method-processors-plist* method))
          (setf (gethash processor table) value))
        (push (make-pipeline
               :method method :quality *model-quality*
               :object (pyeval (format nil "stanza.Pipeline('~A', model_dir='~A', processors=~A, package=None, download_method=None)"
                                       (getf *stanza-lcodes-plist* *language*)
                                       #+mswindows (ppcre:regex-replace-all "\\" (namestring *stanza-resources-directory*) "/")
                                       #+darwin (namestring *stanza-resources-directory*)
                                       (pythonize table))))
              *stanza-pipelines*)))))

(defmethod analyse-sentences ((impl (eql 'stanza)) (method (eql 'dependency)) text)
  (init-analysing-method impl method)
  (let ((doc (call-pipeline (find method *stanza-pipelines* :key #'pipeline-method) text)))
    (loop for sentence across (pyslot-value doc 'sentences)
          collect (loop for word across (pyslot-value sentence 'words)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :head (pyslot-value word 'head)
                                           :upos (pyslot-value word 'upos)
                                           :deprel (pyslot-value word 'deprel))))))

(defmethod analyse-sentences ((impl (eql 'stanza)) (method (eql 'part-of-speech)) text)
  (init-analysing-method impl method)
  (let ((doc (call-pipeline (find method *stanza-pipelines* :key #'pipeline-method) text)))
    (loop for sentence across (pyslot-value doc 'sentences)
          collect (loop for word across (pyslot-value sentence 'words)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :upos (pyslot-value word 'upos))))))

(defmethod analyse-sentences ((impl (eql 'stanza)) (method (eql 'named-entity-recognition)) text)
  (init-analysing-method impl method)
  (let ((doc (call-pipeline (find method *stanza-pipelines* :key #'pipeline-method) text)))
    (loop for sentence across (pyslot-value doc 'sentences)
          collect (loop for word across (pyslot-value sentence 'tokens)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :ner (let ((val (pyslot-value word 'ner)))
                                                  (when (not (string= val "O")) val)))))))
