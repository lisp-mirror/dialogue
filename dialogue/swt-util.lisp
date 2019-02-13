;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)

(defmacro without-redraw (control &body body)
  `(unwind-protect
       (progn
         (control.setredraw ,control nil)
         ,@body)
     (control.setredraw ,control t)))

;;; ****************************************************************************
;;; 

(defun right-button? (event)
  (= 3 (event.button event)))
(defun middle-button? (event)
  (= 2 (event.button event)))
(defun left-button? (event)
  (= 1 (event.button event)))

(defun button-down? (stateMask)
  (not (= 0 (logand *swt.button_mask* stateMask))))

(defun button1-down? (stateMask)
  (not (= 0 (logand *swt.button1* stateMask))))

(defun button2-down? (stateMask)
  (not (= 0 (logand *swt.button2* stateMask))))

(defun button3-down? (stateMask)
  (not (= 0 (logand *swt.button3* stateMask))))

(defun control-down? (stateMask)
  (not (= 0 (logand *swt.control* stateMask))))

(defun alt-down? (stateMask)
  (not (= 0 (logand *swt.alt* stateMask))))

(defun shift-down? (stateMask)
  (not (= 0 (logand *swt.shift* stateMask))))

(defun modifier-key-down? (stateMask)
  (not (= 0 (logand *swt.modifier_mask* stateMask))))
    
;;; ****************************************************************************
;;;


;;; returns 3 values
;;; extent-x, extent-y and a plist
;;; If string-or-strings-or-length is a list, it must be a list of strings: textextent
;;; computes the greatest horizontal extent.
;;; see "GC example snippet: measure a string"
;;;     and table.computeSize getClientArea
(defun textextent (string-or-strings-or-length
                   &key
                   font
                   ;; to store the scrollbar-width in the third value (plist)
                   scrollbar-width
                   border-width
                   )
  (let* ((shell (make-new shell. *display* *swt.no_trim*))
         (label (label.new shell *swt.none*)))
    (when font
      (shell.setfont shell font)
      (label.setfont label font))
    (let ((gc (gc.new label))
          extent-x extent-y
          (plist nil)
          (string
           (cond ((integerp string-or-strings-or-length)
                  (make-string string-or-strings-or-length :initial-element #\m))
                 ((stringp string-or-strings-or-length) string-or-strings-or-length)
                 (t nil))))
      (cond (string
             (let ((point (gc.textextent gc string)))
               (setf extent-x (point.x point)
                     extent-y (point.y point))))
            (t (loop with point.x.max = 0
                     for string in string-or-strings-or-length
                     as point = (gc.textextent gc string)
                     as point.x = (point.x point)
                     when (> point.x point.x.max)
                     do
                     (setf point.x.max point.x)
                     finally (setf extent-x point.x.max
                                   extent-y (point.y point)))))
      (when (or scrollbar-width border-width)
        (let* ((table (table.new shell *swt.border*)))
          (when scrollbar-width
            (setf plist (cons :scrollbar-width
                              (cons (point.x (scrollbar.getsize (table.getverticalbar table)))
                                    plist))))
          (when border-width
            (setf plist (cons :border-width
                              (cons (table.getborderwidth table) plist))))
          ))
      ;; really needed? Doesn't hurt.
      (gc.dispose gc)
      (shell.close shell)
      (values extent-x extent-y plist))))

;;; ****************************************************************************

(defun beep (&optional (display *display*))
  (display.beep display))

(defun flush-events ()
  (while (display.readanddispatch *display*)))
  

;;;***************************************************************************************

;;; to init these variables, see init-j-stuff-with-FVM
(defvar +system-colors+
  (loop for k in
        ;; pseudo : :background-gray and :light-gray = :widget_light_shadow
        ;;
        '(:black :white :yellow :blue :cyan
          :gray
          :green :magenta :red
          :dark_blue :dark_cyan
          :dark_gray
          :dark_green
          :dark_magenta :dark_red :dark_yellow
                  
          :info_background ; = :white (MS)
          :info_foreground
          :list_background :list_foreground :list_selection :list_selection_text
                  
          :title_background :title_background_gradient :title_foreground
                  
          :title_inactive_background  ;; = :dark_gray (MS)
          :title_inactive_background_gradient
          :title_inactive_foreground
                  
          :widget_background :widget_border :widget_dark_shadow
          :widget_foreground
          :widget_highlight_shadow   ; blanc MSw
          :widget_light_shadow       ; gris clair MSw (plus clair que :gray) !!
          :widget_normal_shadow      ; = :dark_gray (MS)
          )        
        collect k
        collect
        (let ((color-name (concatenate 'string
                                       "*SWT.COLOR_"
                                       (symbol-name k)
                                       "*")))
          (if-bind (s (find-symbol color-name))
                   (eval s)
                   (error "In system-color,
can't find ~a color for ~s" color-name k)))))

(defun system-color (colorConstant-or-keyword)
  (let ((colorConstant
         (cond ((keywordp colorConstant-or-keyword)
                (if-bind (cc
                          (case colorConstant-or-keyword
                            ((:background-gray :light-gray)
                             (getf +system-colors+ :widget_light_shadow))
                            (t (getf +system-colors+ colorConstant-or-keyword))))
                         cc
                         (error 
                          "can't find color for ~s" colorConstant-or-keyword)))
               (t colorConstant-or-keyword))))
    (display.getsystemcolor *display*  colorConstant)))

#+never
(defun read-only-background-color ()
  (system-color :widget_light_shadow))

;;; ****************************************************************************
;;; 

(defun set-sash-weights (sash weights)
  (ignore-errors
    (sashform.setweights sash (apply #'box-vector :int weights))))

