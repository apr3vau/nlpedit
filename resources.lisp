(in-package nlpedit)

;; Images

(defun register-images ()
  (loop for sym in '(file-new file-open file-save edit-cut edit-copy edit-paste settings)
        for name in '(add-file open-file save cut copy paste setting)
        do (gp:register-image-translation
            sym
            (gp:read-external-image
             (merge-pathnames (format nil "res/icons8-~A-~A.png"
                                      (string-downcase name)
                                      #+windows 16
                                      #+unix 48)
                              (get-working-directory))))))

;; Colors

(defun hex-to-spec (hex)
  (destructuring-bind (b g r &optional a)
      (loop with str = (string-trim '(#\# #\Space) hex)
            with deno = (if (> (length str) 4) 255 16)
            for i downfrom (1- (length str)) above 0 by 2
            collect (/ (float (parse-integer (subseq str (1- i) (1+ i)) :radix 16))
                       deno))
    (color:make-rgb r g b (when a (+ (- a) 1)))))

(color:define-color-alias 'dark-red1 (hex-to-spec "#620F0E"))
(color:define-color-alias 'dark-blue1 (hex-to-spec "#0E1362"))
(color:define-color-alias 'dark-green1 (hex-to-spec "#064419"))
(color:define-color-alias 'dark-yellow1 (hex-to-spec "#62510E"))
(color:define-color-alias 'dark-cyan1 (hex-to-spec "#003E49"))
(color:define-color-alias 'dark-brown1 (hex-to-spec "#492100"))
(color:define-color-alias 'dark-purple1 (hex-to-spec "#370049"))

(color:define-color-alias 'dark-red2 (hex-to-spec "#7A3635"))
(color:define-color-alias 'dark-blue2 (hex-to-spec "#373C8C"))
(color:define-color-alias 'dark-green2 (hex-to-spec "#379051"))
(color:define-color-alias 'dark-yellow2 (hex-to-spec "#8F7D37"))
(color:define-color-alias 'dark-cyan2 (hex-to-spec "#3F7681"))
(color:define-color-alias 'dark-brown2 (hex-to-spec "#72563F"))
(color:define-color-alias 'dark-purple2 (hex-to-spec "#633572"))

(color:define-color-alias 'light-red1 (hex-to-spec "#FFC8C7"))
(color:define-color-alias 'light-blue1 (hex-to-spec "#C7CAFF"))
(color:define-color-alias 'light-green1 (hex-to-spec "#C7FFD9"))
(color:define-color-alias 'light-yellow1 (hex-to-spec "#FFF4C7"))
(color:define-color-alias 'light-cyan1 (hex-to-spec "#CCFFFF"))
(color:define-color-alias 'light-brown1 (hex-to-spec "#FFDECB"))
(color:define-color-alias 'light-purple1 (hex-to-spec "#EACCFF"))

(color:define-color-alias 'light-red2 (hex-to-spec "#FA9F9D"))
(color:define-color-alias 'light-blue2 (hex-to-spec "#8B91FA"))
(color:define-color-alias 'light-green2 (hex-to-spec "#9DFABB"))
(color:define-color-alias 'light-yellow2 (hex-to-spec "#FADA5C"))
(color:define-color-alias 'light-cyan2 (hex-to-spec "#96F8FA"))
(color:define-color-alias 'light-brown2 (hex-to-spec "#FABC96"))
(color:define-color-alias 'light-purple2 (hex-to-spec "#D396FA"))

(color:define-color-alias
 'red1 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-red1 'dark-red1))
(color:define-color-alias
 'blue1 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-blue1 'dark-blue1))
(color:define-color-alias
 'green1 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-green1 'dark-green1))
(color:define-color-alias
 'yellow1 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-yellow1 'dark-yellow1))
(color:define-color-alias
 'cyan1 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-cyan1 'dark-cyan1))
(color:define-color-alias
 'brown1 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-brown1 'dark-brown1))
(color:define-color-alias
 'purple1 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-purple1 'dark-purple1))

(color:define-color-alias
 'red2 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-red2 'dark-red2))
(color:define-color-alias
 'blue2 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-blue2 'dark-blue2))
(color:define-color-alias
 'green2 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-green2 'dark-green2))
(color:define-color-alias
 'yellow2 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-yellow2 'dark-yellow2))
(color:define-color-alias
 'cyan2 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-cyan2 'dark-cyan2))
(color:define-color-alias
 'brown2 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-brown2 'dark-brown2))
(color:define-color-alias
 'purple2 (gp::create-switchable-color-with-predicate
        'editor::text-pane-background-dark-p 'light-purple2 'dark-purple2))

(defun part-of-speech-default-color (pos)
  (cond ((string= pos "NOUN") 'red2)
        ((string= pos "VERB") 'yellow2)
        ((string= pos "ADJ") 'purple2)
        ((string= pos "ADV") 'brown2)
        ((string= pos "AUX") 'yellow1)
        ((member pos '("NUM" "DET") :test #'string=) 'cyan2)
        ((member pos '("PRON" "PROPN") :test #'string=) 'blue2)
        ((member pos '("SCONJ" "CCONJ") :test #'string=) 'cyan1)
        (t nil)))