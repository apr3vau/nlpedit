(in-package :nlpedit)

(defvar *universal-pos-tags*
  '(("ADJ" . "adjective")
    ("ADP" . "adposition")
    ("ADV" . "adverb")
    ("AUX" . "auxiliary")
    ("CCONJ" . "coordinating conjunction")
    ("DET" . "determiner")
    ("INTJ" . "interjection")
    ("NOUN" . "noun")
    ("NUM" . "numeral")
    ("PART" . "particle")
    ("PRON" . "pronoun")
    ("PROPN" . "proper noun")
    ("PUNCT" . "punctuation")
    ("SCONJ" . "subordinating conjunction")
    ("SYM" . "symbol")
    ("VERB" . "verb")
    ("X" . "other")))

(defvar *universal-dependency-tags*
  '(("Core dependents"
     ("nsubj" . "nominal subject; nominal core argument which is the syntactic subject (or pivot) of a predicate")
     ("csubj" . "clausal syntactic subject of a predicate")
     ("obj" . "object; the core argument nominal which is the most basic core argument that is not the subject, typically the most directly affected participant")
     ("iobj" . "indirect object; nominal core argument of a verb that is not its subject or (direct) object")
     ("ccomp" . "clausal complement of a verb or adjective without an obligatorily controlled subject")
     ("xcomp" . "clausal complement of a verb or adjective with an obligatorily controlled subject"))
    ("Noun dependents"
     ("nummod" . "numeric modifier; numeral in a nominal")
     ("nmod" . "nominal modifier; a nominal modifying another nominal")
     ("appos" . "appositional modifier; a nominal used to define, name, or describe the referent of a preceding nominal")
     ("acl" . "adnominal clause; finite or non-finite clause modifying a nominal")
     ("amod" . "adjectival modifier of a nominal")
     ("det" . "determiner (article, demonstrative, etc.) in a nominal"))
    ("Case-marking"
     ("case" . "links a case-marking element (preposition, postposition, or clitic) to a nominal"))
    ("Non-core dependents"
     ("obl" . "oblique; a nominal functioning as a non-core (oblique) modifier of a predicate")
     ("advcl" . "adverbial clause modifying a predicate or modifier word")
     ("advmod" . "adverb or adverbial phrase modifying a predicate or modifier word"))
    ("Compounding and unanalyzed"
     ("compound" . "any kind of word-level compounding (noun compound, serial verb, phrasal verb)")
     ("fixed" . "fixed multiword expression; links elements of grammaticalized expressions that behave as function words or short adverbials")
     ("flat" . "flat multiword expression; links elements of headless semi-fixed multiword expressions like names")
     ("goeswith" . "links parts of a word that are separated but should go together according to standard orthography or linguistic wordhood"))
    ("Loose joining relationships"
     ("list" . "links elements of comparable items interpreted as a list")
     ("orphan" . "links orphaned dependents of an elided predicate")
     ("parataxis" . "links constituents placed side by side with no explicit coordination or subordination")
     ("dislocated" . "a peripheral (initial or final) nominal in a clause that does not fill a regular role of the predicate but has roles such as topic or afterthought")
     ("reparandum" . "repair of a (normally spoken language) disfluency"))
    ("Special clausal dependents"
     ("vocative" . "nominal directed to an addressee")
     ("aux" . "auxiliary; links a function word expressing tense, mood, aspect, voice, or evidentiality to a predicate")
     ("mark" . "marker; links a function word marking a clause as subordinate to the predicate of the clause")
     ("discourse" . "discourse element (interjection, filler, or non-adverbial discourse marker)")
     ("expl" . "expletive; links a pronominal form in a core argument position but not assigned any semantic role to a predicate")
     ("cop" . "copula; links a function word used to connect a subject and a nonverbal predicate to the nonverbal predicate"))
    ("Coordination"
     ("cc" . "links a coordinating conjunction to the following conjunct")
     ("conj" . "conjunct; links two elements which are conjoined"))
    ("Other"
     ("root" . "root of the sentence")
     ("clf" . "(numeral) classifier; a word reflecting a conceptual classification of nouns linked to a numeric modifier or determiner")
     ("dep" . "unspecified dependency, used when a more precise relation cannot be")
     ("punct" . "punctuation attached to the head of its clause or phrase"))))

(defun deprel= (tag1 tag2)
  "Test if two Universal Dependency tags are equal, ignoring sub-clause splited by colon"
  (or (string= tag1 tag2)
      (string= (car (split-sequence '(#\:) tag1))
               (car (split-sequence '(#\:) tag2)))))
(defun deprel-member (tag lst)
  "Test if a tag is member of list of Universal Dependency tags, ignoring sub-clause splited by colon"
  (or (member tag lst :test #'string=)
      (member (car (split-sequence '(#\:) tag)) lst :test #'string=)))

(defun draw-grid (port c1 c2 grid-width) 
  (capi:with-geometry port
    (gp:draw-rectangle port 0 0 capi:%width% capi:%height% :foreground c1 :filled t)
    (loop for y from 0 to (/ capi:%height% grid-width)
          nconc (loop for x from 0 to (/ capi:%width% grid-width)
                      when (oddp (+ x y))
                        nconc (list (* x grid-width) (* y grid-width) grid-width grid-width))
            into rects
          finally (gp:draw-rectangles port rects :foreground c2 :filled t))))

(defclass color-choice-output (capi:output-pane)
  ((color :initarg :color
          :initform nil)
   (callback :initarg :callback))
  (:default-initargs
   :visible-min-width '(character 8) :height '(character 1)
   :display-callback (lambda (self &rest args)
                       (declare (ignore args))
                       (capi:with-geometry self
                         (draw-grid self :white :grey38 5)
                         (gp:draw-rectangle self 0 0 capi:%width% capi:%height%
                                            :foreground (or (slot-value self 'color) :transparent) :filled t)))
   :input-model `(((:button-1 :release)
                   ,(lambda (self x y)
                      (declare (ignore x y))
                      (setf (slot-value self 'color) (capi:prompt-for-color "Select a color"))
                      (funcall (slot-value self 'callback) (slot-value self 'color))
                      (gp:invalidate-rectangle self))))))

(defmethod editor::text-pane-background-dark-p ((pane color-choice-output))
  (capi:top-level-interface-dark-mode-p (capi:element-interface pane)))
