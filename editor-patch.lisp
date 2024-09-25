(in-package editor)

(defadvice (font-lock-fontify-region nlpedit:nlpedit :around) (start end &optional verbose)
  (let ((buffer (point-buffer start)))
    (if (buffer-minor-mode buffer "NLP")
        (progn
          (when verbose
            (message "Fontifying ~s..." (buffer-name buffer)))
          (with-buffer-locked (buffer :for-modification nil)
            (let ((syntactically-function
                   (or (variable-value 'font-lock-fontify-syntactically-region-function
                                       :current buffer)
                       'lisp-font-lock-fontify-syntactically-region)))
              (funcall syntactically-function start end))
            (let ((keyword-function (or (variable-value 'font-lock-fontify-keywords-region-function
                                                        :current buffer)
                                        'lisp-font-lock-fontify-keywords-region)))
              (funcall keyword-function start end)))
          (when verbose
            (message "Fontifying ~s...done" (buffer-name buffer))))
      (call-next-advice start end verbose))))

(defadvice (font-lock-apply-highlight nlpedit:nlpedit :around) (start end face)
  (if (buffer-minor-mode (point-buffer start) "NLP")
      (put-text-property start end 'face face :modification nil)
    (call-next-advice start end face)))