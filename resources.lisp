(in-package nlpedit)

;; Images

(eval-when (:compile-toplevel :load-toplevel)
  (loop for sym in '(file-new file-open file-save edit-cut edit-copy edit-paste settings named-entities)
        for name in '(add-file open-file save cut copy paste setting name)
        do (setf (get sym :data)
                 (alexandria:read-file-into-byte-vector
                  (merge-pathnames (format nil "res/icons8-~A-~A.png"
                                           (string-downcase name)
                                           #+windows 16
                                           #+unix 48)
                                   (asdf/system:system-source-directory 'nlpedit))))))

(defun register-images ()
  (loop for sym in '(file-new file-open file-save edit-cut edit-copy edit-paste settings named-entities)
        do (gp:register-image-translation
            sym (make-instance 'gp:external-image :data (get sym :data) :type :png))))

;; Colors

(defun hex-to-spec (hex)
  (destructuring-bind (b g r &optional a)
      (loop with str = (string-trim '(#\# #\Space) hex)
            with deno = (if (> (length str) 4) 255 16)
            for i downfrom (1- (length str)) above 0 by 2
            collect (/ (float (parse-integer (subseq str (1- i) (1+ i)) :radix 16))
                       deno))
    (color:make-rgb r g b (when a (+ (- a) 1)))))

(color:define-color-alias 'red :brown3)
(color:define-color-alias 'orange :chocolate1)
(color:define-color-alias 'yellow (editor::create-dark-background-switchable-color :darkgoldenrod1 :gold))
(color:define-color-alias 'green :darkolivegreen3)
(color:define-color-alias 'cyan :mediumaquamarine)
(color:define-color-alias 'blue :skyblue3)
(color:define-color-alias 'purple (editor::create-dark-background-switchable-color :mediumorchid4 :plum3))
(color:define-color-alias 'pink :pink2)
(color:define-color-alias 'gray (editor::create-dark-background-switchable-color :gray47 :gray71))

(color:define-color-alias 'light-red :indianred1)
(color:define-color-alias 'light-orange :orange1)
(color:define-color-alias 'light-yellow (editor::create-dark-background-switchable-color :orange1 :goldenrod1))
(color:define-color-alias 'light-green (editor::create-dark-background-switchable-color :darkgreen :darkolivegreen1))
(color:define-color-alias 'light-cyan (editor::create-dark-background-switchable-color :aquamarine4 :darkslategray1))
(color:define-color-alias 'light-blue (editor::create-dark-background-switchable-color :slategray4 :cadetblue2))
(color:define-color-alias 'light-purple :plum2)
(color:define-color-alias 'light-pink (editor::create-dark-background-switchable-color :indianred1 :rosybrown1))

(color:define-color-alias 'dark-red :brown)
(color:define-color-alias 'dark-orange :tan4)
(color:define-color-alias 'dark-yellow :tan3)
(color:define-color-alias 'dark-green :darkolivegreen4)
(color:define-color-alias 'dark-cyan :aquamarine4)
(color:define-color-alias 'dark-blue :skyblue4)
(color:define-color-alias 'dark-purple :mediumorchid4)
(color:define-color-alias 'dark-pink (editor::create-dark-background-switchable-color :lightpink4 :rosybrown))

(color:define-color-alias 'emphasis-red (editor::create-dark-background-switchable-color 'dark-red 'light-red))
(color:define-color-alias 'emphasis-orange (editor::create-dark-background-switchable-color 'dark-orange 'light-orange))
(color:define-color-alias 'emphasis-yellow (editor::create-dark-background-switchable-color 'dark-yellow 'light-yellow))
(color:define-color-alias 'emphasis-green (editor::create-dark-background-switchable-color 'dark-green 'light-green))
(color:define-color-alias 'emphasis-cyan (editor::create-dark-background-switchable-color 'dark-cyan 'light-cyan))
(color:define-color-alias 'emphasis-blue (editor::create-dark-background-switchable-color 'dark-blue 'light-blue))
(color:define-color-alias 'emphasis-purple (editor::create-dark-background-switchable-color 'dark-purple 'light-purple))
(color:define-color-alias 'emphasis-pink (editor::create-dark-background-switchable-color 'dark-pink 'light-pink))

(defun part-of-speech-default-color (pos)
  (cond ((string= pos "NOUN") 'red)
        ((string= pos "VERB") 'yellow)
        ((string= pos "ADJ") 'purple)
        ((string= pos "ADV") 'brown)
        ((string= pos "AUX") 'orange)
        ((member pos '("NUM" "DET") :test #'string=) 'cyan)
        ((member pos '("PRON" "PROPN") :test #'string=) 'blue)
        ((member pos '("SCONJ" "CCONJ") :test #'string=) 'purple)
        (t nil)))