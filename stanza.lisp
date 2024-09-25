(in-package :nlpedit)

(defpymodule "stanza" t)

(defvar *stanza-dependency-parsing-pipeline* nil)
(defvar *stanza-part-of-speech-pipeline* nil)

(defun stanza-init-dependency-parsing ()
  (unless *stanza-dependency-parsing-pipeline*
    (setf *stanza-dependency-parsing-pipeline*
          (stanza:pipeline/class :lang "en" :model-dir "./stanza/" :processors "tokenize,mwt,pos,lemma,depparse" :download-method nil))))

(defmethod init-analysing-method ((impl (eql 'stanza)) (method (eql 'dependency)))
  (stanza-init-dependency-parsing))

(defmethod parse-sentences ((impl (eql 'stanza)) (method (eql 'dependency)) text)
  (stanza-init-dependency-parsing)
  (let ((obj (pycall *stanza-dependency-parsing-pipeline* text)))
    (loop for sentence across (pyslot-value obj 'sentences)
          collect (loop for word across (pyslot-value sentence 'words)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :head (pyslot-value word 'head)
                                           :upos (pyslot-value word 'upos)
                                           :deprel (pyslot-value word 'deprel))))))

(defun stanza-init-part-of-speech ()
  (unless *stanza-part-of-speech-pipeline*
    (setf *stanza-part-of-speech-pipeline*
          (stanza:pipeline/class :lang "en" :model-dir "./stanza/" :processors "tokenize,mwt,pos" :download-method nil))))

(defmethod init-analysing-method ((impl (eql 'stanza)) (method (eql 'part-of-speech)))
  (stanza-init-part-of-speech))

(defmethod parse-sentences ((impl (eql 'stanza)) (method (eql 'part-of-speech)) text)
  (stanza-init-part-of-speech)
  (let ((obj (pycall *stanza-part-of-speech-pipeline* text)))
    (loop for sentence across (pyslot-value obj 'sentences)
          collect (loop for word across (pyslot-value sentence 'words)
                        collect (make-word :id (pyslot-value word 'id)
                                           :text (pyslot-value word 'text)
                                           :upos (pyslot-value word 'upos))))))
