;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)

;;; list of dialog name and dialog-data pairs
(defvar *dialogs* nil)
    
  
(defstruct (dialog-data (:conc-name "DD-"))
  ;; a keyword, should be unique
  name
  ;; name of the last control where a value was rejected
  (value-rejected-by nil)
  ;; list of control-name and control-data pairs
  (control-data-list nil)
  ;; list of control-name and fref pairs
  (other-control-data-list nil)
  ;; list of the control-data of the controls whose cd-writer has been applied
  ;; See update-data and data-written-back?
  (written-back-controls nil)
  ;; java ref (shell)
  dialog
  ;; see result argument in dialogue function
  result
  ;; a property list to be used by client functions
  plist
  )

(defmethod print-object ((i dialog-data) stream)
  (format stream "<dialog-data: ~S~a>"
          (DD-NAME i) (if (shell.isdisposed (dd-dialog i)) " DISPOSED" ""))
  i)

;;; see dialogue function definition
;;; At present time not for label...
(defstruct (control-data (:conc-name "CD-"))
  ;; keyword, should be unique
  name
  ;; java ref
  control
  ;; keyword: :text, :combo,... see +basic-controls+
  k-type
  (k-style nil)   ; always a list of k-style
  ;; lisp values, for combo and list controls
  ;; see get-control-value (get-basic-control-value), get-range, ...
  (range nil)
  ;; for list and read-only combo controls, to set the initial value(s) if it is not in
  ;; the range 
  ;; Set the value using the
  ;; ! not implemented
  (reset-initial-value? nil)
  ;; pour list control
  (single? nil)
  number
  data
  ;; data in this control is the default dialog data
  (data-default? T)
  ;; initialized at dialog creation time an set when the new value is written back on the Lisp side
  (initial-lisp-value nil)
  ;; the lisp value that has been written back on the Lisp side
  ;; see dd-written-back-controls and update-data
  (new-lisp-value nil)
  ;; to read the value from data. Takes on argument, data
  (reader nil)
  (read-converter nil)
  ;; to convert the control value into the Lisp value
  ;; default to identity
  write-converter
  writer
  ;; to compare Lisp values (not the control values)
  ;; default to value-equal
  equality-test
  edit
  message
  ;; set each time the control gains and keeps focus (:focusin event)
  ;; see set-event-handlers
  old-control-value
  value-accepter
  (dialog-data nil)
  )


(defmethod print-object ((i control-data) stream)
  (format stream "<control-data: ~S>"
          (CD-NAME i))
  i)

;;; close dialogs whose names are in names or all the dialogs if names is NIL
(defun close-dialogs (&key names keep)
  (let ((dialogs nil))
    (loop for sub on *dialogs* by #'cddr
          as n = (first sub)
          if (and (not (eq n keep))
                  (or (not names)
                      (memberq n names)))
          do
          (ignore-errors+
              (warn "Can't dispose ~s: " n)
            (shell.dispose (dd-dialog (second sub))))
          else
          do
          (push (second sub) dialogs)
          (push n dialogs))
    (setf *dialogs* dialogs)
    ))


;;; thing: see find-dialog-data
(defmacro do-written-back-controls ((var thing &key filter) &body body)
  "Macro to loop in the control-data of the controls whose value has been written
back to the Lisp side.
Filter should be a predicate of one argument
to be applied to the control-data of each control.
If a predicate is defined and returns NIL, the control is skipped"
  `(loop for ,var in (dd-written-back-controls (find-dialog-data ,thing))
         when (or (not ,filter)
                  (funcall ,filter ,var))
         do
         ,@body))


(defmacro do-controls ((var thing &key filter) &body body)
  "Macro to loop in the control-data of the controls.
Filter should be a predicate of one argument
to be applied to the control-data of each control.
If a predicate is defined and returns NIL, the control is skipped"
  (with-gensyms (sub)
    `(do* ((,sub (dd-control-data-list (find-dialog-data ,thing))
                 (cddr ,sub)))
          ((not ,sub))
       (let ((,var (second ,sub)))
         (when (or (not ,filter)
                   (funcall ,filter ,var))
           ,@body)))))


;;; fref : nil or foil reference
;;; fclass : class name (ex. : shell.)
(defun typep_foil (fref fclass)
  (ensure-typed-ref fref)
  (typep fref fclass))


(defun type-of_foil (fref)
  (ensure-typed-ref fref)
  (type-of fref))

;;; ****************************************************************************

(defconstant +layout-types+
  '(:fill-h :fill-v :row :column :grid :2c-grid :form))

(defconstant +composite-types+
  '(:composite :tabfolder :group :sashform))


;;; k-style : a list of
(defun text-button? (k-style)
  (intersection k-style
                '(:push :toggle)
                :test #'eq))

(defun state-button? (k-style)
  (intersection k-style
                '(:radio :check :toggle)
                :test #'eq))

(defun layout? (item)
  (and (listp item)
       (let ((type (first item)))
         (if (memberq type +layout-types+)
             type
           nil))))

(defun composite? (item)
  (cond ((listp item)
         (let ((type (first item)))
           (if (memberq type +composite-types+)
               type
             nil)))
        (t (typep_foil item 'composite.))))

(defun compound? (item)
  (or (layout? item)
      (composite? item)))

(defun make-dummy-widget (parent)
  (let ((wi (make-new label. parent *swt.none*)))
    ;; to see the dummy widgets, uncomment these 2 lines and comment the 2 last:
    ; (label.settext wi " ")
    ; (label.setbackground wi (asystem-color :red))
      (label.setenabled wi nil)
      (label.setvisible wi nil)))

(defconstant +basic-controls+
   '(:label :text :button :list :combo))

(defun basic-control? (thing)
  (cond ((keywordp thing)
         (memberq thing +basic-controls+))
        ((control-data-p thing)
         (memberq (cd-k-type thing) +basic-controls+))
        (t (error "basic-control? ..."))))

(defun combo? (cd)
  (eq (cd-k-type cd) :combo))

(defun read_only-combo? (cd)
  (and (combo? cd)
       (memberq :read_only (cd-k-style cd))))

(defmethod editable-combo? ((k-type symbol) k-style)
  (and (eq k-type :combo)
       (not (memberq :read_only k-style))))

(defmethod editable-combo? ((cd control-data) ignore)
  (and (combo? cd)
       (not (memberq :read_only (cd-k-style cd)))))
     
;;; convert the combo or list lisp-range in a vector of strings to be passed
;;; to combo.setitems and list.setitems
(defun make-combo-vector (cd)
  (loop with converter = (cd-read-converter cd)
        with lisp-range = (cd-range cd)
        with vector = (make-new-vector string. (length lisp-range))
        for val in lisp-range
        as i = 0 then (1+ i)
        finally (return vector)
        do
        (setf (vref vector i)
              (funcall converter val cd))))


(defvar *trim-text-control-value?* t)

(defun trim-text-control-value (string)
  (if *trim-text-control-value?*
      (string-trim '(#\space #\tab) string)
    string))


;;; ****************************************************************************
;;;



;;; name: the name (keyword) of a control
;;; thing: see find-dialog-data
(defun find-control-data (name thing &optional (errorp t))
  ;; dd: Dialog-Data  -    cd: Control-Data
  (let ((dd (find-dialog-data thing)))
    (if-bind (cd (getf (dd-control-data-list dd) name))
             cd
             (if errorp
                 (error "can't find control data: ~s" name)
               nil))))

;;; thing :
;;; - the name (keyword) of a dialog
;;; - any control-data or control (java-ref) in the dialog
;;; - the Dialog-Data it self.
(defun find-dialog-data (thing &key (errorp t))
  (let ((dd
         (cond ((keywordp thing)
                (getf *dialogs* thing))
               ((control-data-p thing)
                (cd-dialog-data thing))
               ((dialog-data-p thing)
                thing)
               #+dev???
               ((typep_foil thing 'shell.)
                (error "Thing can't be a shell - Use shell name in ~s" (object.tostring thing)))
               ;; control, get the name of the dialog with getdata - (control.setdata )
               (t (getf *dialogs* (get-dialog-name thing))))))
    (when (and errorp (not dd))
      (error "can't find dialog-data: ~s" thing))
    dd))

;;; fast version of get-dialog:
;;; - argument must be the name of the di
;;; - no check
(defun dialog-exists? (di-name)
  (getf *dialogs* di-name))

(defun get-dialog-name (control-or-shell)
  ;; (cond ((typep_foil control 'shell)        (loop for 
  ;; autre solution (getparent control) et parcours de *dialogs* sans doute
  ;; plus efficace
  (find-symbol (object.tostring (getdata control-or-shell))
               :keyword))

(defun get-dialog (thing &key (errorp nil))
  (let ((Dialog-Data (find-dialog-data thing :errorp errorp)))
    (cond (Dialog-Data
           (let ((di (dd-dialog Dialog-Data)))
             (if (shell.isdisposed di)
                 (progn
                   (when (keywordp thing)
                     (remf *dialogs* thing))
                   (when errorp
                     (error "can't find dialog: ~s" thing))
                   nil)
               di)))
          (t nil))))
    

;;; name:the name (keyword) of a control
;;; thing: usualy the dialog name (keyword), see find-dialog-data
(defun find-control (name thing &optional (errorp t) (look-in-other-control-data-list? nil))
  (let* ((dd (find-dialog-data thing))
         (cd (getf (dd-control-data-list dd) name))
         (control (if cd
                      (cd-control cd)
                    (when look-in-other-control-data-list?
                      (getf (dd-other-control-data-list dd) name)))))
    (if (and (not control) errorp)
        (error "can't find control: ~s" name)
      control)))

(defun add-other-control (dialog-name control-name control)
  (let ((dd (find-dialog-data dialog-name)))
    (setf (getf (dd-other-control-data-list dd) control-name)
          control)))


(defun get-control-name (control &key (errorp t))
  (let* ((dd (find-dialog-data control))
         (dd-control-data-list (dd-control-data-list dd)))
    (loop for sub on dd-control-data-list by #'cddr
          as c = (cd-control (second sub))
          when (equals control c)
          do
          (return (first sub))
          finally (if errorp
                      (error "can't find control name of ~s" control)
                    (return nil)))))

