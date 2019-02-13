;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.00
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)

;;; ****************************************************************************

;;; example:
;;; 1. (def-SWT-test (with-shell (ok-di "Hello!")))
;;; 2. eval (:test) or push F12 to run the :test function
(defmacro def-SWT-test (&body body)
  `(:dtest
    ;; Note : it is not necessary to restart the Java VM and the Foil VM each time. The 2
    ;;        following forms can be removed.
    ;; start or restart the Java VM (killing *ui-swt-process* if exists)
    (start-JavaVM-Foil)
    (start-FVM)
    (run-swt-ui-aux
     (lambda ()
       ,@body))
    ))

;;; examples
;;; (run-SWT-ui (with-shell (ok-di "Hello!")))
;;; (run-SWT-ui (with-shell (ok-di "Hello!" :icon :warn)))
;;; Restarting JVM:
;;; (run-SWT-ui (T) (with-shell (ok-di "Hello!")))
(defmacro run-SWT-ui (&body body)
  `(progn
     (start-JavaVM-Foil)
     (start-FVM)
     (run-SWT-ui-aux
      (lambda ()
        ,@body))))

;;; LWw 5, see run-SWT-ui-aux
(defvar *stack-size* 64000)

(defun run-SWT-ui-aux (fn &key
                          (stack-size *stack-size*)
                          (process-name (concatenate 'string
                                                     "ui-SWT-proc_"
                                                     (string (gensym)))))
  (macrolet ((handle-socket-error (&body body)
               `(handler-bind (#+:lispworks
                               (comm:socket-error
                                (lambda (condition)
                                  (error "socket-error.
 JVM and Foil server are probably not running.
 To restart JVM: (start-FVM :start-javaVM? t)~2%~a" condition))))
                  ,@body)))
    (handle-socket-error
     (ensure-swt-display))
    (setf *SWT-ui-process-name* process-name)
    (setf *ui-SWT-process*
          #-:lispworks
          (error "run-SWT-ui isn't defined for this Lisp")
          #+:lispworks
          (let ((mp:*process-initial-bindings*
                 (append '(;(*terminal-io* . *terminal-io*)
                           (*trace-output* .  mp:*background-standard-output*)
                           (*standard-output* .  mp:*background-standard-output*)
                           (*error-output* . mp:*background-standard-output*)
                           ;;
                           (*default-swt-parent* . NIL)
                           ;;
                           (*display* . *display*)
                           (*fvm* . *fvm*)
                           (*thread-fvm-stream* . (ui-stream))
                           (*thread-fvm* . *fvm*))
                         mp:*process-initial-bindings*)))  
            (mp:process-run-function
             process-name
             (list :size stack-size)
             (lambda ()
               (handle-socket-error
                (funcall fn))))))))


(defun dispose-SWT-display ()
  (ignore-errors
    (with-vm-of *display*
      (display.dispose *display*)))
  (setf *display* nil))


(defun make-SWT-display (&optional (application-name "SWT - Lisp"))
  (when *display*
    (dispose-SWT-display))
  (let ((*thread-fvm* *fvm*)
        (*thread-fvm-stream* (ui-stream)))
    (setf *display* (make-new display.))
    (display.setappname application-name)
    (init-SWT-stuff)
    (setf *dialogs* nil)
    *display*))


(defvar *target-cursor* nil)
(defvar *waiting-cursor* nil)
(defvar *default-cursor*)
(defvar *hand-cursor* nil)

(defun init-cursors (&optional (device *display*))
  (setf *default-cursor*
        (cursor.new device *swt.cursor_arrow*)        
        *target-cursor*
        (cursor.new device *swt.cursor_cross*)
        *waiting-cursor*
        (cursor.new device *swt.cursor_wait*)
        *hand-cursor*
        (cursor.new device *swt.cursor_hand*)
        ))

(defun init-SWT-stuff (&optional shell)
  shell
  (init-cursors)
  )

(defun ensure-SWT-display ()
  (unless *display*
    (make-SWT-display))
  *display*)

;;; ****************************************************************************
;;; SHELL

(defun shell-show (shell &key (setactive T))
  (cond ((shell.getminimized shell)
         (shell.setminimized shell nil))
        (setactive
         (shell.setactive shell))
        (t (shell.setvisible shell t)
           (shell.moveabove shell nil))))

;;; open a shell using with-shell&dispatch and CLOSE it after the execution of body
(defmacro with-shell (&body body)
  `(with-shell&dispatch (sh)
    ,@body
    (shell.close sh)))

;;; SEE shell-style for modality and trimmings args
;;; If main is T , set *main-SWT-shell* to the newly created shell
;;; Bind *default-swt-parent* to the newly created shell
;;; readanddispatch the events while the new shell remains open
(defmacro with-shell&dispatch ((var &key
                                    ;; call open on the shell (make it visible and active)
                                    (open? t)
                                    (move-above? nil)
                                    parent
                                    ;; :application :parent :system :none
                                    (modality :application)
                                    ;; a keyword or a list of ~
                                    ;; :border :close :min :max :resize :title :no-trim
                                    ;; :shell-trim :dialog-trim
                                    ;; :on-top :tool
                                    (trimmings :dialog-trim)
                                    ;; matches *swt.on_top* , can be in trimmings
                                    top-level?
                                    (maximized nil maximized-s)
                                    (text "Algorithme")
                                    background
                                    ;; - T to define a gridlayout
                                    ;; - a list of arguments to gridlayout.new
                                    gridlayout
                                    ;; type of the filllayout: :vertical , :horizontal or an
                                    ;; integer value  (*SWT.VERTICAL*...)
                                    filllayout
                                    top
                                    left
                                    ;; a list of 2 integers, width and height
                                    size
                                    location
                                    font
                                    ;; a 1 argument function to be called (argument: the shell),
                                    ;; after the call to shell.pack
                                    ;; useful to shell.setsize , etc
                                    (init-function nil)      
                                    ;; only one top main shell per foil VM
                                    main)
                               &body body)
  (with-gensyms (parent-rt shell-error? shell-disposed? display-error?)
    `(let ((,parent-rt (if ,parent ,parent *display*)))    ; Run-Time value
       (when (and ,main
                  *main-SWT-shell*
                  ;; ignore-errors au cas ou
                  ;; - *main-SWT-shell* résidu session précédente...
                  ;; - doit etre fait dans le bon thread
                  (ignore-errors (not (shell.isdisposed *main-SWT-shell*))))
         (cerror "" "A main shell already exists
You should probably close it first" ))
       (let* ((,var (make-new shell. ,parent-rt
                              (dialogue::shell-style ,modality ,trimmings
                                                     ,top-level?)))
              (*default-swt-parent* ,var)
              (,shell-disposed? nil)
              (,display-error? nil)
              (,shell-error? nil))
         (unwind-protect
             (progn 
               (when ,main
                 (setf *main-SWT-shell* ,var))
               (when ,font
                 (ignore-errors+ (progn (warn "can't set shell font: ~s" ,font) )
                   (shell.setfont ,var ,font)))
               (when ,text
                 (shell.settext ,var ,text))
               (when ,maximized-s
                 (shell.setmaximized ,var ,maximized))
               (when ,background
                 (shell.setbackground ,var
                                      (if (keywordp ,background)
                                          (system-color ,background)
                                        ,background)))
               (cond ((and ,location (neq ,location :nice))
                      (shell.setlocation ,var ,location))
                     ((or ,top ,left)
                      (cond ((xor ,top ,left)
                             (warn "BOTH top and left sould be given to with-shell&dispatch")
                             (shell.setlocation ,var
                                                (if ,left ,left 100)
                                                (if ,top ,top 100)))
                            (t (shell.setlocation ,var ,left ,top))))
                     (t nil))
               (cond (,gridlayout
                      (shell.setlayout
                       ,var (apply #'gridlayout.new
                                   (if (eq t ,gridlayout)
                                       nil
                                     ,gridlayout) )))
                     (,filllayout
                      (shell.setlayout
                       ,var
                       (filllayout.new
                        (case ,filllayout
                          (:vertical *swt.vertical*)
                          (:horizontal *swt.horizontal*)
                          (t (if (keywordp  ,filllayout)
                                 (error "unknown keyword : ~s" ,filllayout)
                               ,filllayout)))))))
               ;; attention doit se trouver avant de préférence (pour le cas
               ;; où des fonctions dans body font du resize)
               (when ,size
                 (apply #'shell.setsize ,var ,size))
               (when (and (eq ,location :nice)
                          (not (or ,top ,left)))
                 (shell.setlocation ,var (nice-location ,var)))
               ,@body
               (if (shell.isdisposed ,var)
                   (setf ,shell-disposed? t)
                 (progn
                   (shell.pack ,var)
                   (when ,init-function
                     (funcall ,init-function ,var))
                   (cond (,move-above?
                          (shell.setvisible ,var)
                          (shell.moveabove ,var nil))
                         (,open?
                          (shell.open ,var))
                         (t nil))
                   (handler-case 
                       (while (progn (multiple-value-setq (,shell-disposed? ,shell-error?)
                                         (ignore-errors (shell.isdisposed ,var)))
                                (not (or ,shell-disposed? ,shell-error?)))
                         (unless (display.readanddispatch *display*)
                           (display.sleep *display*)))
                     (error (c)
                            (error c)
                            (setf ,display-error? T))))))
           ;; clean-up forms
           (cond (,shell-error?)
                 (,display-error?)
                 ((not ,shell-disposed?)
                  (ignore-errors (shell.close ,var)))
                 (t nil))
           (when ,main
             (setf *main-SWT-shell* nil)))))))



;;; attention: location of shell is relative to the display
;;; calcule une position proche du cursor mais en évitant que le dialogue ne
;;; déborde de l'écran (de l'écran pas du shell parent)
(defun nice-location (shell)
  (let* ((point (display.getcursorlocation *display*))
         (size (shell.getsize shell))
         (w (point.x size))
         (h (point.y size))
         ;; expected position:
         (x (- (point.x point) 10))
         (y (- (point.y point) (round h 3)))
         (rect (display.getclientarea *display*))
         ;; nb : using (display.getprimarymonitor *display*) and
         ;;      (monitor.getbounds monitor) or (monitor.getclientarea monitor)
         ;;      gives the same rectangle with 1 monitor,  f.e. : 0 0 1280 1024
         (rectx (rectangle.x rect))
         (recty (rectangle.y rect))
         (rectw (rectangle.width rect))
         (recth (rectangle.height rect)))
    (setf (point.x point)
          (max rectx
               (min (max x rectx)
                    (+ rectx rectw (- w))))
          (point.y point)
          (max recty
               (min (max y recty)
                    (+ recty recth (- h)))))
    point))
           
                 


