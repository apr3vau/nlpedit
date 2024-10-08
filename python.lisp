;; PY4CL2 Patch

(in-package :uiop/launch-program)

(defclass process-info ()
  ((process :initform nil)
   (input-stream :initform nil)
   (output-stream :initform nil)
   (bidir-stream :initform nil)
   (error-output-stream :initform nil)
   ;; For backward-compatibility, to maintain the property (zerop
   ;; exit-code) <-> success, an exit in response to a signal is
   ;; encoded as 128+signum.
   (exit-code :initform nil)
   ;; If the platform allows it, distinguish exiting with a code
   ;; >128 from exiting in response to a signal by setting this code
   (signal-code :initform nil)))

(defun %normalize-io-specifier (specifier &optional role)
  "Normalizes a portable I/O specifier for LAUNCH-PROGRAM into an implementation-dependent
argument to pass to the internal RUN-PROGRAM"
  (declare (ignorable role))
  (typecase specifier
    (null (null-device-pathname))
    (string (parse-native-namestring specifier))
    (pathname specifier)
    (stream specifier)
    ((eql :stream) :stream)
    ((eql :interactive) nil)
    ((eql :output)
     (cond ((eq role :error-output) :output)
           (t (parameter-error "~S IO specifier invalid for ~S" specifier role))))
    (otherwise
     (parameter-error "Incorrect I/O specifier ~S for ~S"
                      specifier role))))

(defun %handle-if-exists (file if-exists)
  (when (or (stringp file) (pathnamep file))
    (ecase if-exists
      ((:append :supersede :error)
       (with-open-file (dummy file :direction :output :if-exists if-exists)
         (declare (ignorable dummy)))))))

(defun %handle-if-does-not-exist (file if-does-not-exist)
  (when (or (stringp file) (pathnamep file))
    (ecase if-does-not-exist
      ((:create :error)
       (with-open-file (dummy file :direction :probe
                              :if-does-not-exist if-does-not-exist)
         (declare (ignorable dummy)))))))

(defun launch-program (command &key
                               input (if-input-does-not-exist :error)
                               output (if-output-exists :supersede)
                               error-output (if-error-output-exists :supersede)
                               (element-type #-clozure *default-stream-element-type*
                                             #+clozure 'character)
                               (external-format *utf-8-external-format*)
                               directory
                               #+allegro separate-streams
                               &allow-other-keys)
  "Launch program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on
Windows) _asynchronously_.

If OUTPUT is a pathname, a string designating a pathname, or NIL (the
default) designating the null device, the file at that path is used as
output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*, and
under SLIME will be on your *inferior-lisp* buffer.  If it's T, output
goes to your current *STANDARD-OUTPUT* stream.  If it's :STREAM, a new
stream will be made available that can be accessed via
PROCESS-INFO-OUTPUT and read from. Otherwise, OUTPUT should be a value
that the underlying lisp implementation knows how to handle.

IF-OUTPUT-EXISTS, which is only meaningful if OUTPUT is a string or a
pathname, can take the values :ERROR, :APPEND, and :SUPERSEDE (the
default). The meaning of these values and their effect on the case
where OUTPUT does not exist, is analogous to the IF-EXISTS parameter
to OPEN with :DIRECTION :OUTPUT.

ERROR-OUTPUT is similar to OUTPUT. T designates the *ERROR-OUTPUT*,
:OUTPUT means redirecting the error output to the output stream,
and :STREAM causes a stream to be made available via
PROCESS-INFO-ERROR-OUTPUT.

IF-ERROR-OUTPUT-EXISTS is similar to IF-OUTPUT-EXIST, except that it
affects ERROR-OUTPUT rather than OUTPUT.

INPUT is similar to OUTPUT, except that T designates the
*STANDARD-INPUT* and a stream requested through the :STREAM keyword
would be available through PROCESS-INFO-INPUT.

IF-INPUT-DOES-NOT-EXIST, which is only meaningful if INPUT is a string
or a pathname, can take the values :CREATE and :ERROR (the
default). The meaning of these values is analogous to the
IF-DOES-NOT-EXIST parameter to OPEN with :DIRECTION :INPUT.

ELEMENT-TYPE and EXTERNAL-FORMAT are passed on to your Lisp
implementation, when applicable, for creation of the output stream.

LAUNCH-PROGRAM returns a PROCESS-INFO object."
  (when (some #'streamp (list input output error-output))
    (parameter-error "~S: I/O parameters cannot be foreign streams on this lisp"
                     'launch-program))
  (nest
   (progn ;; see comments for these functions
     (%handle-if-does-not-exist input if-input-does-not-exist)
     (%handle-if-exists output if-output-exists)
     (%handle-if-exists error-output if-error-output-exists))
   (let ((process-info (make-instance 'process-info))
         (input (%normalize-io-specifier input :input))
         (output (%normalize-io-specifier output :output))
         (error-output (%normalize-io-specifier error-output :error-output))
         (command
          (etypecase command
            #+os-unix (string `("/bin/sh" "-c" ,command))
            #+os-unix (list command)
            #+os-windows (string command)
            #+os-windows (list command)))))
   (with-current-directory (directory))
   (multiple-value-bind (io-or-pid err-or-nil #-lispworks7+ pid-or-nil)
     #.`(funcall
         #+os-unix ,@'('system:run-shell-command `("/usr/bin/env" ,@command))
         #+ os-windows ,@`('system:run-shell-command `(,@command))
         :input input :if-input-does-not-exist :error
         :output output :if-output-exists :append
         :error-output error-output
         :if-error-output-exists :append
         :wait nil :element-type element-type :external-format external-format
         :allow-other-keys t
         :save-exit-status t))
   (labels ((prop (key value) (setf (slot-value process-info key) value)))
     (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
       (cond
        ((or (plusp mode) (eq error-output :stream))
         (prop 'process #+lispworks7+ io-or-pid #-lispworks7+ pid-or-nil)
         (when (plusp mode)
           (prop (ecase mode (1 'input-stream) (2 'output-stream) (3 'bidir-stream))
                 io-or-pid))
         (when (eq error-output :stream)
           (prop 'error-output-stream err-or-nil)))
        ;; Prior to Lispworks 7, this returned (pid); now it
        ;; returns (io err pid) of which we keep io.
        (t (prop 'process io-or-pid)))))
   process-info))

(in-package :py4cl2)

(let ((lispworks:*redefinition-action* nil))
  (defun pystart (&optional (command (config-var 'pycmd)))
    "Start a new python subprocess
This sets the global variable *python* to the process handle,
in addition to returning it.
COMMAND is a string with the python executable to launch e.g. \"python\"
By default this is is set to (CONFIG-VAR 'PYCMD)
"
    (flet ((bash-escape-string (string) ; TODO: Better way to do things!
             ;; We want strings such as
             ;; "/user/ram-disk/test (hello''/miniconda(3')/bin/"
             ;; to be escaped correctly.
             ;; This function only exists in the context of PYSTART
             (with-output-to-string (*standard-output*)
               (iter (for ch in-string string)
                 (case ch
                   (#\' (write-string "\\'"))
                   (#\( (write-string "\\("))
                   (#\) (write-string "\\)"))
                   (#\space (write-string "\\ "))
                   (t (write-char ch)))))))
      (declare (ignorable (function bash-escape-string)))
      (let ((temp (hcl:create-temp-file :file-type "py")))
        (with-open-file (out temp
                             :direction :output
                             :if-exists :supersede)
          (princ *python-code* out))
        (loop :until (python-alive-p)
              :do (setq *python*
                        #+(or os-windows windows)
                        (uiop:launch-program
                         (format nil "set PYTHONIOENCODING=utf8 && ~A -u ~A ./" command temp)
                         :stream :lock
                         :input :stream
                         :output :stream
                         :error-output :stream)
                        #+unix
                        (uiop:launch-program
                         (concatenate 'string
                                      (bash-escape-string command) " "
                                      (bash-escape-string (namestring temp)) " "
                                      (namestring (truename "./")))
                         :stream :lock
                         :input :stream
                         :output :stream
                         :error-output :stream))
                  (sleep 0.1)
                  (unless (python-alive-p)
                    (let ((*python-startup-error* (or (ignore-errors
                                                        (read-stream-content-into-string
                                                         (uiop:process-info-error-output *python*)))
                                                      "Unable to fetch more error details on ECL")))
                      (cerror "Provide another path (setf (config-var 'pycmd) ...)"
                              'python-process-startup-error :command command))
                    (format t "~&Provide the path to python binary to use (eg python): ")
                    (let ((cmd (read-line)))
                      (setf (config-var 'pycmd) cmd)
                      (setf command cmd))))))
    (unless *py4cl-tests*
      (setq *python-output-thread*
            (bt:make-thread
             (lambda ()
               (when *python*
                 (let ((py-out (uiop:process-info-error-output *python*)))
                   (iter outer
                     (while (and *python* (python-alive-p *python*)))
                     (handler-case
                         (for char =
                              (progn
                                ;; PEEK-CHAR waits for input
                                (peek-char nil py-out nil)
                                (when *in-with-python-output*
                                  (iter (while *in-with-python-output*)
                                    (bt:wait-on-semaphore *python-output-semaphore*))
                                  (in outer (next-iteration)))
                                (read-char py-out nil)))
                       (simple-error (condition) nil
                         #|(unless (and (member :ccl *features*)
                                      (search "is private to" (format nil "~A" condition)))
                           (error "~S~%  ~A~%occured while inside *python-output-thread* ~A"
                                  condition condition *python-output-thread*))|#
                         )
                       (stream-error (condition) nil
                         #|(unless (member :abcl *features*)
                           (error "~S~%  ~A~%occured while inside *python-output-thread* ~A"
                                  condition condition *python-output-thread*))|#
                         ))
                     (when char (write-char char)))))))))
    (cond ((and (numpy-installed-p)
                (not (member :arrays *internal-features*)))
           (push :arrays *internal-features*))
          ((and (not (numpy-installed-p))
                (member :arrays *internal-features*))
           (removef *internal-features* :arrays)))
    (incf *current-python-process-id*)
    (apply #'raw-pyexec *additional-init-codes*)))