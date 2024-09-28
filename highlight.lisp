(in-package nlpedit)

;; Font Lock

(defparameter *fontify-delay* 0.8
  "The delaying seconds between user make change and running analysing
method & fontify.

Prevent calling the NLP model too frequently while continueously
typing to cause unecessary evaluation.")

(defvar *fontify-process* nil
  "MP:PROCESS that serve for fontify.

Should never use this variable directly, use ENSURE-FONTIFY-PROCESS instead.")

(defun fontify-by-words (start end words)
  "Fontify region by the style of giving WORDs.

WORDs must match the content within the region."
  (editor:with-buffer-locked ((editor:point-buffer start) :for-modification nil)
    (editor:move-point end start)
    (editor:remove-text-properties start end '(face nil) :modification nil)
    (dolist (word words)
      (loop with target = (char (word-text word) 0)
            for i from 0
            for c = (editor:character-at start i)
            until (eql c target)
            finally (editor:character-offset start i))
      (editor:move-point end start)
      (editor:character-offset end (length (word-text word)))
      (editor::font-lock-apply-highlight
       start end
       (editor:make-face nil :foreground (word-foreground word) :background (word-background word)))
      (editor:move-point start end))))

(defun fontify-process-function ()
  "Function of *fontify-process*.

Listening for EDITOR:POINTs (START END) to fontify"
  (loop with mailbox = (mp:process-mailbox (mp:get-current-process))
        for (start end) = (mp:mailbox-read mailbox "Waiting for points (START END) to fontify")
        do (loop for (new-start new-end) = (mp:mailbox-read mailbox "Waiting for new points within delay" *fontify-delay*)
                 until (null new-start)
                 do (setf start (if (editor:point> start new-start) new-start start)
                          end (if (editor:point< end new-end) new-end end)))
           (when-let (sentences (parse-sentences *nlp-implementation* *analysing-method* (editor:points-to-string start end)))
             (setf sentences (annotate-sentences *analysing-method* *annotating-method* sentences))
             (fontify-by-words start end (mapcan #'identity sentences)))))

(defun ensure-fontify-process ()
  "Ensure the fontify process is running and return it.

Should always use this function to get the fontify process, don't use
the *fontify-process* directly."
  (or (and (mp:process-alive-p *fontify-process*)
           *fontify-process*)
      (setf *fontify-process*
            (mp:process-run-function
             "NLP-Edit Fontify Process" '(:mailbox t)
             'fontify-process-function))))

(defun font-lock-mark-block-function (start end)
  "Function of EDITOR::FONT-LOCK-MARK-BLOCK-FUNCTION in NLP mode

Mark single sentence each time."
  (let* ((backward-terminator-pos (loop for i downfrom -1
                                        for c = (editor:character-at start i)
                                        until (editor:character-attribute :sentence-terminator c)
                                        finally (return i)))
         (first-char-pos (loop for j from backward-terminator-pos
                               for c = (editor:character-at start j)
                               until (alphanumericp c)
                               finally (return j)))
         (forward-terminator-pos (loop for i from 0
                                       for c = (editor:character-at end i)
                                       until (editor:character-attribute :sentence-terminator c)
                                       finally (return i))))
    (editor:character-offset start first-char-pos)
    (editor:character-offset end forward-terminator-pos)))

(defun font-lock-fontify-keywords-function (start end)
  "Function of EDITOR::FONT-LOCK-FONTIFY-KEYWORDS-REGION-FUNCTION in NLP mode

Send the region to the fontify process"
  (mp:process-send (ensure-fontify-process)
                   (list (editor:copy-point start) (editor:copy-point end))))

;; Editor Mode

(editor:defmode "NLP"
  :setup-function (lambda (buffer) (setf (editor::buffer-font-lock-mode-p buffer) t))
  :vars '((editor::font-lock-fontify-keywords-region-function . font-lock-fontify-keywords-function)
          (editor::font-lock-mark-block-function . font-lock-mark-block-function)))

(editor:defcommand "NLP Mode" (p)
     "Turn on / off the NLP mode"
     "Turn on / off the NLP mode"
  (if p
      (setf (editor:buffer-minor-mode (editor:current-buffer) "NLP") (plusp p))
    (if (editor:buffer-minor-mode (editor:current-buffer) "NLP")
        (setf (editor:buffer-minor-mode (editor:current-buffer) "NLP") nil)
      (setf (editor:buffer-minor-mode (editor:current-buffer) "NLP") t))))

(editor:defcommand "NLP Configure" (p)
     "Configure NLP Mode"
     "Show NLP Configure Interface"
  (declare (ignore p))
  (capi:popup-confirmer
   (make-instance 'nlp-configure-interface)
   "Configure NLP Mode" :cancel-button nil)
  (dolist (itf (capi:collect-interfaces 'nlp-editor))
    (capi:call-editor (slot-value itf 'editor) "Font Lock Fontify Buffer")))
