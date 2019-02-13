;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)


;;; ****************************************************************************
;;; misc

;;; example, keyword is :button:
;;; if java-ref? is nil, this function returns button. else the value of button. that is :
;;; |org.eclipse.swt.widgets|::|Button|
(defun java-class-name (keyword &optional (java-ref? t))
  (let ((lisp-class-symbol
         (case keyword
           (:label 'label.)
           (:button 'button.)
           (:text 'text.)
           (:list 'list.)
           (:combo 'combo.)
           (t
            (let ((symbol (find-symbol
                           (concatenate  'string
                                         (symbol-name keyword)
                                         "."))))
              (if symbol
                  symbol
                (error "unknown widget class keyword: ~s" keyword)))))))
    (if java-ref?
        (symbol-value lisp-class-symbol)
      lisp-class-symbol)))

;; (get-k-type 'button.) -> :button
(defun get-k-type (j-class-name)
  (let ((name (string j-class-name)))
    (intern (subseq name 0 (1- (length name))) 'keyword)))

;;; examples
;;; (get.java.method.name 'shell. :setfont)
;;;=> shell.setfont
;;; (get.java.method.name :shell "SETFONT")
;;;=> shell.setfont
;;; (get.java.method.name <shell> "SETFONT")
;;;=> shell.setfont
;;; (get.java.method.name <shell> :setfont)
;;;=> shell.setfont
;;; (get.java.method.name <rowlayout> :marginbottom)
;;;=> rowlayout.marginbottom
;;; j-object-or-type : a java-ref, Foil type (like button. )
;;;                 or a k-type (like :shell, :button,...)
;;; method : a string, a keyword or a symbol (like setfont, :setfont or  "SETFONT")
(defun get.java.method.name (j-object-or-type method)
  (let* ((method-name (if (symbolp method)
                          (symbol-name method)
                        method))
         (fun-string-name
          (typecase j-object-or-type
            (keyword 
             (concatenate 'string
                          (symbol-name j-object-or-type)
                          "."
                          method-name))
            (symbol (concatenate 'string
                                 (symbol-name j-object-or-type)
                                 method-name))
            (t (ensure-typed-ref j-object-or-type)
               (concatenate 'string
                            (symbol-name (type-of j-object-or-type))
                            method-name))))
         (fun-name (find-symbol fun-string-name)))
    (unless (and fun-name (fboundp fun-name))
      (error  "can't find function definition of ~a (~s) for ~s object"
              (string-downcase fun-string-name) method j-object-or-type))
    fun-name))

;;; method :
;;;   - string, a keyword or a symbol (like setfont, :setfont or  "SETFONT")
;;;   - or the method itself
;;; j-object : foil object reference
;;; See get.java.method.name and def-java-generic
(defun apply-j-method (method j-object &rest args)
  (apply (if (or (symbolp method)
                 (stringp method))
             (fdefinition (get.java.method.name j-object method))
           method)
         j-object
         args))


;;; options : (:font <font> :size ...)
;;;           (:setfont <font> :setSize ...)
;;; for composite and tab-items object
(defun apply-setX-options (object options)
  (loop for (keyword value) on options by #'cddr
        do
        (unless (keywordp keyword)
          (error "Malformed options list: ~s" options))
        (cond ((eq keyword :prompt) nil)
              ((or (eq keyword :layout-data) 
                   (eq keyword :layout))
               (set-layout-data object value))
              (t (let* ((kn (symbol-name keyword))
                        ;; setXxx function
                        (function-string-name (if (string= "SET" (subseq kn 0 3))
                                                  kn
                                                (concatenate 'string
                                                             "SET"
                                                             kn)))
                        (function-name (get.java.method.name object function-string-name)))
                   (if (and value (listp value))
                       (apply (fdefinition function-name) object value)
                     (funcall (fdefinition function-name) object value)))))))


;;; ****************************************************************************
;;;  styles
;;; the first style in each list is the default

;;; to find or make a style constant for swt widget class 
;;; exemple:
;;; k-type: :label 
;;; +label-styles+
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-style-plist-constant (k-type &optional (make? nil))
    (let ((constant-name 
           (concatenate 'string
                        "+" (symbol-name k-type ) "-STYLES+")))
      (with-package :dialogue
                    (if make?
                        (intern constant-name)
                      (find-symbol constant-name))))))


(defconstant +common-styles-exclusions+
    '((:horizontal :vertical)
      (:left :right :center)))  

(defvar *styles-exclusions* nil)


;;; to define a style constant for a swt widget class 
;;; example:
;;; EXAMPLE
#|
(def-style-constant
 :label 
 '(:left :wrap :center :right :separator :horizontal :vertical
   :shadow_in :shadow_out :shadow_none)
 '((:shadow_in :shadow_out :shadow_none)))

define the +label-styles+ constant as:
(:left 16384 :wrap 64 :center 16777216 :right 131072 :separator 2 :horizontal 256 :vertical 512 :shadow_in 4 :shadow_out 8 :shadow_none 32 :none 0)
AND
(setf (getf *styles-exclusions* :label) '((:shadow_in :shadow_out :shadow_none)))

|#
;;; REMARK
;;; add the style :NONE to k-styles
(defmacro def-style-constant (k-type k-styles &optional exclusions)
  (let ((constant (find-style-plist-constant k-type t)))
    `(progn
       (setf (getf *styles-exclusions* ,k-type) ,exclusions)
       ;; defconstant, empêche 2 compilation successive !
       (defvar ,constant
         (loop for k in (append ,k-styles (list :none))
               collect k
               collect 
               (let ((style-name (concatenate 'string
                                              "*SWT." (symbol-name k) "*")))
                 (if-bind (s (find-symbol style-name))
                          ;; (macroexpand '*swt.left*)
                          (eval s)
                          (error "In def-style-constants,
can't find ~a style for ~s" style-name k))))))))


(defun get-styles-exclusions (k-type)
  (getf *styles-exclusions* k-type nil))


;(combine-styles :label nil)
;(combine-styles :label '(:separator :horizontal)
;;; some-k-styles: nil, a k-style or a list of k-styles
;;; if some-k-styles is nil, default to *swt.none*
;;; (combine-styles :menu :pop_up) - *swt.pop_up*
(defun combine-styles (k-type some-k-styles &optional only-one?)
  (let* ((constant (find-style-plist-constant k-type))
         (constant-value (symbol-value constant)))
    (cond (some-k-styles
           (unless (listp some-k-styles)
             (setf some-k-styles (list some-k-styles)))
           (if only-one?
               (when (> (length some-k-styles) 1)
                 (error "Only one style allowed for ~a class (~s)"
                        (java-class-name k-type nil)
                        some-k-styles))
             (when (> (length some-k-styles) 1)
               (check-styles-exclusion k-type some-k-styles)))
           (apply  #'logior
                   (loop for k in some-k-styles
                         as style = (getf constant-value k)
                         if style
                         collect style
                         else
                         do (error "unknown ~s keyword in ~s~% Class: ~a"
                                   k some-k-styles 
                                   (java-class-name k-type nil)))))
          (t *swt.none*))))

;;; (get-style :menuitem :cascade)
(defun get-style (k-type k-style)
  (if k-style
      (let* ((constant (find-style-plist-constant k-type))
             (constant-value (symbol-value constant))
             (style (getf constant-value k-style)))
        (if style
            style
          (error "unknown ~s style keyword -  Class: ~a"
                 k-style
                 (java-class-name k-type nil))))
    *swt.none*))


; (check-styles-exclusion :label '(:left :right))
(defun check-styles-exclusion (k-type some-k-styles
                                      &optional
                                      (style-exclusions
                                       (append (get-styles-exclusions k-type)
                                               +common-styles-exclusions+)))
  (loop for ex in style-exclusions
        when (> (length (intersection some-k-styles ex :test #'eq))
                1)
        do (error "Style specification not allowed: ~s~% Class: ~a"
                  some-k-styles 
                  (java-class-name k-type nil))))


; (shell-style :parent '(:shell-trim))
; (shell-style :parent '(:shell-trim :dialog-trim))
; (shell-style :parent '(:shell-trim :dialog-trim))
; (shell-style nil '(:shell-trim))
(defun shell-style (modality trimmings &optional (top-level? nil))
  (unless (listp trimmings)
    (setf trimmings (list trimmings)))
  (check-styles-exclusion :shell trimmings
                          '((:no-trim :shell-trim :dialog-trim)))
  (apply #'logior
         (case modality
           (:parent *swt.primary_modal*)
           (:application *swt.application_modal*)
           (:system *swt.system_modal*)
           ((:none nil) *swt.modeless*)
           (t (error "unknown shell modality: ~s" modality)))
         (if trimmings
             (loop for g in trimmings
                   collect
                   (case g
                     (:border *swt.border*)
                     (:close *swt.close*)
                     (:min *swt.min*)
                     (:max *swt.max*)
                     (:resize *swt.resize*)
                     (:title *swt.title*)
                     (:no-trim *swt.no_trim*)
                     (:shell-trim *swt.shell_trim*)
                     (:dialog-trim *swt.dialog_trim*)
                     (:on-top *swt.on_top*)
                     (:tool *swt.tool*)
                     (t (error "unknown trimming item (shell style): ~s" g))))
           (list (if top-level? *swt.shell_trim* *swt.dialog_trim*)))))


;;; ****************************************************************************
;;;

(defvar +event-types+
  (let ((result nil))
          (dolist (k '(:activate :close :collapse :deactivate :defaultselection
                       :deiconify :dispose :expand :focusin :focusout :help :hide
                       :iconify :keydown :keyup :modify :mousedoubleclick :mousedown
                       :mouseenter :mouseexit :mousehover :mousemove :mouseup :move
                       :paint :resize :selection :show :verify)
                     (nreverse result))
            (let ((event-name (concatenate 'string
                                           "*SWT." (symbol-name k) "*")))     
              (push k result)
              (push (eval (find-symbol event-name)) result)))))

(defvar +menuitem-event-types+
  (let ((result nil))
          (dolist (k '(:arm  :help :selection)
                     (nreverse result))
            (let ((event-name (concatenate 'string
                                           "*SWT." (symbol-name k) "*")))     
              (push k result)
              (push (eval (find-symbol event-name)) result)))))


;;; (get-any-event-type :arm )
;;; (get-any-event-type 'mousedoubleclick)
(defun get-any-event-type (k-event-type &key (errorp T))
  (let ((event-name (concatenate 'string
                                 "*SWT." (symbol-name k-event-type) "*")))
    (let ((symbol (find-symbol event-name)))
      (cond ((and symbol
                  (symbol-macro? symbol))
             (eval symbol))
            (errorp
             (error "can't find event type for ~s" k-event-type))
            (t nil)
             ))))

(defun get-event-type (k-event-type &key
                                    (widget-k-type :control)
                                    (errorp T))
  (let ((event-type
         (if widget-k-type
             (case widget-k-type
               (:control (getf +event-types+ k-event-type))
               (:menuitem (getf +menuitem-event-types+ k-event-type))
               (errorp (error "~s" widget-k-type))
               (t nil))
           (or (getf +event-types+ k-event-type)
               (getf +menuitem-event-types+ k-event-type)))))
    (if event-type
        event-type
      (if errorp
          (error "can't find event type for ~s" k-event-type)
        nil))))

;;; ****************************************************************************
;;; 