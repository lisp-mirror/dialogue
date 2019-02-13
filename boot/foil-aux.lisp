(in-package :cl-user)

(export '(start-JavaVM-Foil
          show-fref
          *ui-stream*
          *non-ui-stream*
          start-FVM
          ))

;;; ****************************************************************************
;;; 
(import '(*ui-stream*
          *non-ui-stream*
          *display*
          *main-SWT-shell*
          *default-swt-parent*
          *ui-SWT-process*
          *SWT-ui-process-name*
          ui-stream
          start-fvm
          start-JavaVM-Foil
          )
        :dialogue)

(defvar *ui-stream* nil)
(defvar *non-ui-stream* nil)

;;; the SWT display
(defvar *display* nil)

;;; should never been set (bound only)
(defvar *default-swt-parent* nil)

;;; only one at at time (one per foil VM).
;;; set in dlg::with-shell&dispatch macro if main arg is T
(defvar *main-SWT-shell* nil)

(defvar *ui-SWT-process* nil)

(defvar *SWT-ui-process-name* "")

(defun ui-stream ()
  (unless (and *ui-stream*
               (open-stream-p *ui-stream*))
    (error "*ui-stream* should be an open socket to foil server"))
  *ui-stream*)

;;; ****************************************************************************
;;; 

(defmethod foil::handle-proxy-call (method-symbol proxy &rest args)
  ;;(format t "unhandled :proxy-call ~S ~S ~S~%" method-symbol proxy args)
  nil)

;;; kill Java si existe ensuite démarre JavaVM et Foil server
(defun start-JavaVM-Foil (&key (sleep-time .3)
                               (path *application-startup-directory*))
  (msg "Starting JavaVM and Foil server")
  (with-open-stream
      (s (sys:open-pipe
          (string-append
           (namestring 
            (make-pathname :defaults path
                           :name
                           "JavaVMFoil"
                           :type "bat"))
           " "
           (namestring *application-startup-directory*))))
    (loop while
          (let ((line (read-line s nil nil)))
            (when (and line (not (string= "" line)))
              (format t "~& ~a" line))
            line)))
  (reset-FVM-variables)
  (sleep sleep-time))

          
  
;; (start-FVM :force? t)
(defun start-FVM-aux (&key (force? t))
  ;; tester fvm après arrêt de JVM,
  (cond ((and *fvm* (not force?))
         (msg "FVM already started"))
        (t
         (when *ui-stream*
           (ignore-errors (close *ui-stream*))
           (ignore-errors (close *non-ui-stream*)))
         (nilf *fvm* *ui-stream* *non-ui-stream*)
         (msg "Starting FVM")
         (setf *ui-stream*
               (comm:open-tcp-stream "localhost" 13578)
               *non-ui-stream*
               (comm:open-tcp-stream "localhost" 13579))
         (unless (and *ui-stream* *non-ui-stream*
                      (open-stream-p *ui-stream*)
                      (open-stream-p *non-ui-stream*))
           (error "can't open fvm sockets.
       JVM and Foil server are probably not running"))
         (setf *fvm* (make-instance 'foreign-vm :stream *non-ui-stream*)))))

(defun start-fvm (&key
                  ;; if T, restart FVM
                  (force? nil)
                  ;; if T, start JavaVM, restart FVM
                  (start-JavaVM? nil)
                  (script-path *application-startup-directory*))
  (when start-JavaVM?
    (setf force? T)
    (start-JavaVM-Foil :path script-path :sleep-time 2))
  (loop with started = nil
        while (not started)
        do
        (handler-case
            (progn
              (start-fvm-aux :force? force?)
              (setf started T))
          (error (condition) 
                 (declare (ignore condition))
                 (msg "condition: " condition)
                 (msg "Can't communicate with JavaVM and Foil server:
  - JavaVM and Foil server are probably not running.
  - Will start JavaVM again.")
                 (start-JavaVM-Foil :path script-path :sleep-time 5)
                 (setf force? t)))))


(defun reset-FVM-variables (&key (close-sockets t))
  (when close-sockets
    (ignore-errors (close *ui-stream*))
    (ignore-errors (close *non-ui-stream*)))
  ;; or *ui-SWT-process*
  (when-bind (SWT-process (mp:find-process-from-name *SWT-ui-process-name*))
    (mp:process-kill SWT-process))
  (nilf *fvm*
        *non-ui-stream*
        *ui-stream*
        *display*
        *main-swt-shell*
        *default-swt-parent*
        *ui-SWT-process*
        ))

;;; ****************************************************************************


(defun load-java-dumps (file-names &key recompile lisp)
  (without-redefinition-warnings
    (loop for fn in file-names
          do
          (loadb (make-pathname
                  :defaults *java-dump-dir*
                  :name fn
                  :type "lisp")
                 :recompile recompile
                 :lisp lisp))))


#|
(get-library-classnames2 (NAMESTRING *swt-jar-path*) (list "org/eclipse/swt/events/")
                         :keep '("Paint" "Verify"))
(get-library-classnames2 (NAMESTRING *swt-jar-path*) (list "org/eclipse/swt/events/")
                         :skip '("Paint" "Verify"))
;;; 220 classes
(length (get-library-classnames2 (NAMESTRING *swt-jar-path*)
                                 (list "")
                                 :keep '("internal")))
;;; all the classes :
(length (get-library-classnames2 (NAMESTRING *swt-jar-path*) (list ""))) > 457
|#
;;; skip and keep : strings to search in the classe names
(defun get-library-classnames2 (jar-or-assemby packages &key skip keep)
  (let ((subclasses (apply #'get-library-classnames jar-or-assemby packages)))
    (cond
     (keep (filter (lambda (classname)
                     (dolist (tag keep nil)
                       (when (and (search tag classname :test #'string-equal)
                                  (dolist (tag skip t)
                                    (when (search tag classname :test #'string-equal)
                                      (return nil))))
                         (return t))))
                   subclasses))
     (skip
      (filter (lambda (classname)
                (dolist (tag skip t)
                  (when (search tag classname :test #'string-equal)
                    (return nil))))
              subclasses))
     (t subclasses))))
  
(defun dump-wrapper-defs-to-file2 (jar-file
                                   &key
                                   skip
                                   keep
                                   (dump-dir *java-dump-dir*)
                                   dump-name
                                   packages
                                   classes
                                   compile?
                                   (load? t))
  "If dump-name is not provided, the dump-file name is taken from jar-file name,
dump-file type is lisp."
  (let* ((jar-file-name (pathname-name jar-file))
         (subclasses (if classes
                         classes
                       (get-library-classnames2
                        (NAMESTRING jar-file)  ; sinon plante !!!
                        packages
                        :keep keep
                        :skip skip)))
         (dump-path (make-pathname
                     :defaults dump-dir
                     :name (if dump-name dump-name jar-file-name)
                     :type "lisp"))
         (file-loaded nil))
    (dump-wrapper-defs-to-file dump-path subclasses)
    (when compile?
      (compile-file dump-path)
      (when load?
        (setf file-loaded (loadb dump-path))))
    (format t "~%** ~a classes" subclasses)
    (format t "~%** ~a" jar-file)
    (format t "~%** ~a packages" packages)
    (format t "~%** ~a lisp dump" dump-path)
    (format t "~%** ~d classes" (length subclasses))
    (when file-loaded
      (format t "~%** ~a loaded" file-loaded))
    (terpri)
    (if file-loaded file-loaded dump-path)))


#|
(dump-wrapper-defs-to-file
 "\\swt-dump-test.lisp"
 (get-library-classnames
  *swt-jar-path*
  "org/eclipse/swt/"))
|#



(defun dump&compile-java-defs ()
  (dump-wrapper-defs-to-file2
   *swt-jar-path*
   :dump-name "swtlayout"
   :skip '("form")
   :compile? T
   :packages '("org/eclipse/swt/layout"))

  (dump-wrapper-defs-to-file2
   *swt-jar-path*
   :dump-name "swtevents"
   :skip '("Paint" "Verify")
   :compile? t
   :packages '("org/eclipse/swt/events/"))

  (dump-wrapper-defs-to-file2
   *swt-jar-path*
   :dump-name "swtgraphics"
   :skip '("image" "Text" "graphics.Device" "Drawable" "Palette")
   :compile? t
   :packages '("org/eclipse/swt/graphics/"))

  (dump-wrapper-defs-to-file2
   *swt-jar-path*
   :dump-name "swtwidgets"
   :compile? t
   :packages '("org/eclipse/swt/widgets/"))

  ;;; sashform
  (dump-wrapper-defs-to-file2
   *swt-jar-path*
   :dump-name "custom"
   :keep '("sash")
   :compile? t
   :packages '("org/eclipse/swt/custom/"))

  (dump-wrapper-defs-to-file2
   *swt-jar-path*
   :dump-name "browser"
   :compile? t
   :packages '("org/eclipse/swt/browser/"))   

  )
