(in-package nlpedit)

(defvar *font-family*
  #+windows "Arial"
  #+mac ".AppleSystemUIFont"
  #+linux "Liberation Sans"
  "Font family of NLP Editor pane")

(defparameter *font-size* 14
  "Font size of NLP Editor pane")

(defun file-new (itf) (capi:call-editor (slot-value itf 'editor) "New Buffer"))

(defun file-open (itf)
  (when-let (file (capi:prompt-for-file "Open File"
                                   :pathname (sys:get-folder-path :documents)
                                   :filter "*.txt;*.text;*.md;*.markdown"
                                   :filters '("Supporting Files" "*.txt;*.text;*.md;*.markdown")))
    (editor:process-character (list #'editor:wfind-file-command nil file)
                              (slot-value itf 'editor))))

(defun file-save (itf)
  (with-slots (editor) itf
    (if (editor:buffer-pathname (editor:window-buffer (capi:editor-window editor)))
        (capi:call-editor editor "Save File")
      (file-save-as itf))))

(defun file-save-as (itf)
  (with-slots (editor) itf
    (let ((path (capi:prompt-for-file "Save As..."
                                      :pathname (sys:get-folder-path :documents)
                                      :operation :save
                                      :filter "*.txt"
                                      :filters '("Text Files" "*.txt"
                                                 "Markdown Files" "*.md"))))
      (when (and path
                 (or (not (probe-file path))
                     (y-or-n-p "File ~a exists, overwrite it anyway? " path)))
        (editor::write-da-file (editor:window-buffer (capi:editor-window editor))
                               (or (probe-file path) (translate-logical-pathname path)))))))

(setf capi:*editor-cursor-active-style* :left-bar)
;; (setf capi:*editor-cursor-active-style* :inverse)

(capi:define-interface nlp-editor ()
  ()
  (:panes
   (editor
    capi:editor-pane
    :text "Welcome to the NLP Editor!"
    :buffer-modes '("Text" "NLP")
    :font (gp:make-font-description :family *font-family* :size *font-size*)
    :wrap-style :split-on-space
    :pane-menu nil)
   )
  (:layouts
   (main-layout
    capi:simple-layout
    '(editor)))
  (:menus
   (file-menu
    "File"
    ((:component
      (("New" :name 'file-new :callback #'file-new)
       ("Open..." :name 'file-open :callback #'file-open)
       ("Save" :name 'file-save :callback #'file-save)
       ("Save As..." :name 'file-save-as :callback #'file-save-as))
      :callback-type :interface)))
   (edit-menu
    "Edit"
    ((:component
      (("Undo" :callback #'capi:active-pane-undo
        :enabled-function #'capi:active-pane-undo-p)))
     (:component
      (("Cut" :name 'edit-cut :callback #'capi:active-pane-cut
        :enabled-function #'capi:active-pane-cut-p)
       ("Copy" :name 'edit-copy :callback #'capi:active-pane-copy
        :enabled-function #'capi:active-pane-copy-p)
       ("Paste" :name 'edit-paste :callback #'capi:active-pane-paste
        :enabled-function #'capi:active-pane-paste-p)))
     (:component
      (("Select All" :callback #'capi:active-pane-select-all
        :enabled-function #'capi:active-pane-select-all-p)
       ("Deselect All" :callback #'capi:active-pane-deselect-all
        :enabled-function #'capi:active-pane-deselect-all-p)))
     (:component
      (("Settings..." :name 'settings
        :callback (lambda (itf) (declare (ignore itf))
                    (capi:popup-confirmer
                     (make-instance 'nlp-configure-interface)
                     "Settings" :cancel-button nil))))))
    :callback-type :interface)
   (window-menu
    "Window"
    ((:component
      (("Close Window" :callback #'capi:quit-interface)
       ("Minimize" :callback (lambda (itf) (setf (capi:top-level-interface-display-state itf) :iconic))))))
    :callback-type :interface))
  (:menu-bar file-menu edit-menu window-menu)
  (:default-initargs
   :title "NLP Editor"
   :initial-constraints '(:visible-min-width (* :screen-width 0.618)
                          :visible-min-height (* :screen-height 0.6))
   :auto-menus nil
   :toolbar-items
   (loop for name in '(file-new file-open file-save edit-cut edit-copy edit-paste settings)
         collect (make-instance 'capi:toolbar-button
                                :text (or (second (split-sequence '(#\-) (string-capitalize name)))
                                          (string-capitalize name))
                                :image name
                                :remapped name))))

; (capi:display (make-instance 'nlp-editor))

(defun main ()
  (cd (directory-namestring (lisp-image-name)))
  #-win32
  (setf (environment-variable "PYTHONHOME") (truename "./py/")
        (environment-variable "PYTHONPATH") (truename "./py/"))
  (setf (py4cl2:config-var 'py4cl2:pycmd) (namestring (truename #+win32 "./py/python.exe"
                                                                #+unix "./py/bin/python3.10")))
  (register-images)
  (loop for i in (analysing-methods)
        do (init-analysing-method *nlp-implementation* i))
  (capi:display (make-instance 'nlp-editor)))

(export 'main)