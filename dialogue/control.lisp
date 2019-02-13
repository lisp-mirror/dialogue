;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm


(in-package :dialogue)


;;; control-spec : a list of alternating keywords and values
;;; data : default
;;; edit : default
;;; parent : the shell (dialog)
(defun process-control-spec (control-spec parent &key data edit)
  (unless control-spec
    (return-from process-control-spec (make-dummy-widget parent)))
  (setf control-spec (convert-simple-item-spec control-spec))
  (when (getf control-spec :ignore)
    (return-from process-control-spec nil))
  (let* (methods
         (item nil)
         (data-unspecified? T)
         (font *dialogue-default-font*)
         name 
         prompt
         accessor 
         (reader :%unspecified% )
         (writer :%unspecified% )
         ;; ex button. , :button , |org.eclipse.swt.widgets|::|Button| 
         control-type k-control-type Jcontrol-type
         range
         style k-style
         value-type
         (initial-value :%unspecified%)
         message
         (read-converter :%unspecified%)
         (write-converter :%unspecified%)
         (equality-test #'value-equal)
         (reader-or-writer-name nil)
         layout-data-options
         event-handlers
         (read-only nil)                   ; means Not Enabled
         (value-accepter :%unspecified%)
         cd)
    (loop for (keyword value) on control-spec by #'cddr
          do
          (unless (keywordp keyword)
            (error "not a proper control specification: ~s" control-spec))
          (case keyword
            (:methods (setf methods value))
            (:item (cond ((atom value) ; could be NIL
                          (setf item value))
                         (T
                          (setf item (first value))
                          (let ((length (length value)))
                            (cond ((= 2 length) 
                                   (setf edit (second value)))
                                  ((= 1 length) nil)
                                  (T (error "Unvalid :item spec: ~S" 
                                            control-spec)))))))
            (:font (setf font value))
            (:edit (setf edit value))
            (:data (setf data value data-unspecified? nil))
            (:name (setf name value))
            (:prompt (setf prompt value))
            (:accessor (setf accessor value))
            (:reader (setf reader value))
            (:writer (setf writer value))
            (:type
             (cond ((keywordp value)
                    (setf k-control-type value))
                   (t (setf control-type value
                            k-control-type (get-k-type value)))))
            (:range (setf range value))
            (:style (setf k-style (cond ((listp value) value)
                                        ((integerp value) value)
                                        (t (list value)))))
            (:value-type (setf value-type value))
            (:initial-value (setf initial-value value))
            (:read-converter
             (setf read-converter value))
            (:write-converter
             (setf write-converter value))
            (:equality-test
             (when value (setf equality-test value)))
            ((:layout-data :layout) (setf layout-data-options value))
            (:event (setf event-handlers value))
            (:value-accepter (setf value-accepter value))
            (:enabled (setf read-only (not value)))
            (:read-only (setf read-only value))
            (:message (setf message value)
             message
             (error ":message option is not implemented"))
            (:ignore nil)
            (T (error "Unknown keyword in control specification: ~s"
                      keyword))))
    (when read-only
      (setf writer nil write-converter nil))
    (when (or (not write-converter)
              (not value-accepter))
      (setf write-converter nil value-accepter nil))
    (setf edit (ensure-edit edit))
    (setf reader-or-writer-name (reader-or-writer-name accessor reader writer))
    (when (eq *layout-type :2c-grid)
      (setf prompt (ensure-prompt prompt item edit reader-or-writer-name)))
    (setf reader (ensure-reader reader accessor edit item name))
    (setf initial-value (ensure-initial-value initial-value reader data item edit))
    (unless read-only
      (setf writer (ensure-writer writer accessor edit item name)))
    (setf value-type (ensure-value-type value-type edit item initial-value data))
    (multiple-value-setq (Jcontrol-type control-type k-control-type k-style)
        (ensure-control-type control-type k-control-type value-type k-style))
    (setf name (ensure-name name k-control-type edit item reader-or-writer-name prompt))
    (setf style (compute-style k-style control-type k-control-type))
    (when (basic-control? k-control-type)
      (setf read-converter
            (ensure-read-converter read-converter k-control-type k-style))
      (unless (or read-only
                  (eq k-control-type :label))
        (setf write-converter
              (ensure-write-converter write-converter k-control-type k-style
                                      value-type range name))
        (setf value-accepter
              (ensure-value-accepter value-accepter k-control-type value-type))))
    (let* ((basic-control? (basic-control? k-control-type))
           (control (make-new Jcontrol-type parent style)))
      (when read-only
        ;(apply-j-method :setbackground control (read-only-background-color))
        (apply-j-method :setenabled control nil)
        )
      (setdata control (symbol-name *dialog-name))
      (when (or *make-any-control-data?*
                (not (memberq k-control-type '(:label))))
        (setf cd (make-control-data
                  :name name
                  :control control
                  :k-type k-control-type
                  :k-style k-style
                  :range range
                  :single? (and (eq k-control-type :list)
                                (memberq :single k-style))
                  :number *control-number
                  :data data
                  :data-default? data-unspecified?
                  :initial-lisp-value initial-value
                  :edit edit
                  :reader reader
                  :writer writer
                  :read-converter read-converter
                  :write-converter write-converter
                  :equality-test equality-test
                  :value-accepter value-accepter
                  :dialog-data *dialog-data))
        (push cd (dd-control-data-list *dialog-data))
        (push name (dd-control-data-list *dialog-data)))
      ;; must be called before store-initial-control-value...  :setItems ...
      (apply-methods control parent k-control-type methods cd)
      (when font (setfont control font))
      (when basic-control?
        (store-initial-control-value control k-control-type initial-value
                                     read-converter cd))
      (when layout-data-options
        (set-layout-data control layout-data-options name))
      ;; (object.tostring (getdata control))
      (when (or event-handlers
                (basic-control? k-control-type))
        (set-event-handlers k-control-type event-handlers control cd))
      (incf *control-number)
      control)))


(defun ensure-value-accepter (value-accepter k-control-type value-type)
  (when (eq value-accepter :%unspecified%)
    (setf value-accepter
          (if (memberq k-control-type '(:list :button))
              (lambda (new cd)
                (declare (ignore new cd))
                T)
            ;; toujours T sauf si value-type est spécifié
            (lambda (new cd)
              (cond ((read_only-combo? cd) t)
                    (value-type (typep new value-type))
                    (t t))))))
  value-accepter)

(defun ensure-write-converter (write-converter k-control-type k-style
                                               value-type range &optional name)
  (let ((unspecified? (eq :%unspecified% write-converter)))
    (cond ((or (eq k-control-type :text)
               (editable-combo? k-control-type k-style))
           (cond (unspecified?
                  (if value-type
                      (cond ((subtypep value-type 'string)
                             (lambda (control-value cd)
                               (declare (ignore cd))
                               (trim-text-control-value control-value)))
                            ((subtypep value-type 'keyword)
                             (lambda (control-value cd)
                               (declare (ignore cd))
                               (intern (string-upcase (trim-text-control-value control-value))
                                       'keyword)))
                            (t (lambda (control-value cd)
                                 (declare (ignore cd))
                                 (read-from-string (trim-text-control-value control-value)))))
                    (lambda (control-value cd)
                      (declare (ignore cd))
                      (read-from-string control-value))))
                 (write-converter (ensure-fdefinition write-converter))
                 ;; specified to NIL
                 (t nil)))
          ((eq k-control-type :button)
           (cond (unspecified?
                  (lambda (control-value cd)
                    (declare (ignore cd))
                    control-value))
                 (write-converter (ensure-fdefinition write-converter))
                 (t nil)))
          ;; a :read_only combo, lisp-value is a integer
          ;; *dialogs*
          ((eq k-control-type :combo)
           (cond (unspecified?
                  (lambda (value cd)
                    (declare (ignore value))                   
                    (with-error- (:write-converter :detail name)                     
                      ;; indice is -1 if no selected item                                         
                      (let ((indice (combo.getselectionindex (cd-control cd))))
                        (if (>= indice 0)
                            (nth indice (cd-range cd))
                          ;; ??
                          nil)))))
                 (write-converter
                  (when range
                    (warn "no need to define a write-converter for a :read_only combo"))
                  (lambda (indice cd)
                    (funcall (ensure-fdefinition write-converter)
                             indice cd))
                  nil)
                 (t nil)))
          ((eq k-control-type :list)
           (cond (unspecified?
                  (lambda (value control-data)
                    (declare (ignore value))
                    (let ((control (cd-control control-data)))
                      (cond ((cd-single? control-data)
                             (let ((pos (list.getselectionindex control)))
                               (if (>= pos 0)
                                   (nth pos (cd-range control-data))
                                 nil)))
                            (t (let ((vec (list.getselectionindices control))
                                     result)
                                 (do-j-vector (i vec)
                                   (push (nth i (cd-range control-data))
                                         result))
                                 result))))))                 
                 (write-converter
                  (lambda (indice cd)
                    (funcall (ensure-fdefinition write-converter)
                             indice cd)))
                 (t nil)))
          (t nil))))


;; pour :list et combo appliqué à chaque valeur du range - voir apply-methods - 
(defun ensure-read-converter (read-converter k-control-type k-style)
  (let ((unspecified? (eq :%unspecified% read-converter)))
    (cond ((or (memberq k-control-type '(:text :label :combo :list))
              ; (editable-combo? k-control-type k-style)
               )
           (cond (unspecified?
                  (lambda (lisp-value cd)
                    (declare (ignore cd))
                    (if (typep lisp-value 'string)
                        lisp-value
                      (if lisp-value
                          (princ-to-string lisp-value)
                        ""))))
                 (read-converter (ensure-fdefinition read-converter))
                 ;; read-converter specified to NIL
                 (t nil)))
          ((eq k-control-type :button)
           (cond (unspecified?
                  (if (text-button? k-style)
                      (lambda (lisp-value cd)
                        (declare (ignore cd))
                        lisp-value)
                    (lambda (lisp-value cd)   ;; 
                      (declare (ignore cd))
                      (ensure-boolean-value lisp-value))))
                 (read-converter (ensure-fdefinition read-converter))
                 (t nil)))
          (t (error "not implemented")))))



(defun set-event-handlers (k-control-type event-handlers control control-data)
  (setf event-handlers
        (add-acceptation-event-handlers-specification event-handlers
                                                      control-data))
  (loop for (k-event-type handler) on event-handlers by #'cddr
        do
        ;; attention sans cette sous-fonction, il faudrait
        ;; (let ((k-event-type k-event-type) (handler handler) ...)
        (set-event-handler k-event-type handler k-control-type control control-data)))



(defun set-event-handler (k-event-type handler k-control-type control
                                       control-data)
  (let (#+lispworks (dspec:*redefinition-action* nil))
    (funcall
     ;; e.g. button.addlistener, to add (registering) a listener to the widget
     ;;  for the specifier event (e.g. *swt.selection*)
     (get.java.method.name k-control-type "ADDLISTENER")
     control
     (get-event-type k-event-type)   ; e.g. the value of *swt.selection*, ...
     (new-proxy p +marshall-id+ 0
                (listener.      ; Java interface
                 (handleevent   ; Java abstract method (listener.handleevent)
                  (event)
                  (funcall (ensure-fdefinition handler)
                           event control-data)))))))

#|
Differences between activation and focus events:
1)  A parent (by parent I mean any parent in the parent hierarchy not just 
the immediate parent) will receive an activation event the first time one of 
its children gets focus. (As focus moves between children of the same parent 
additional activation events are not sent).
2) It is not neccessary for focus to be assigned for the activation event to 
occur - if a user clicks on the background of a Composite or Canvas or on a 
widget such as a Label that does not take focus, an activation event will 
still be sent

Note: focusin event occurs before a possible selection event
|#

(defun add-acceptation-event-handlers-specification (event-handlers
                                                     control-data)
  (when control-data                
    (let ((user-focusin-handler (getf event-handlers :focusin)))
      (when user-focusin-handler
        (setf user-focusin-handler (ensure-fdefinition user-focusin-handler))
        (remf event-handlers :focusin))
      (push (lambda (event control-data)
              (when user-focusin-handler
                (funcall user-focusin-handler event control-data))
              (let* ((dd (cd-dialog-data control-data))
                     (cn (dd-value-rejected-by dd)))
                ;; if cn, new value in cn has been rejected
                (cond (cn
                       (setf (dd-value-rejected-by dd) nil)
                       (beep)
                       ; (flush-events)
                       ;;? doesn't work if user uses tab instead of mouse or
                       ;;  or return key
                       (setfocus (find-control cn dd)))
                      (t (setf (cd-old-control-value control-data)
                               (get-basic-control-value (cd-control control-data)
                                                        control-data))))))
            event-handlers)
      (push :focusin event-handlers))    
    (when (and (basic-control? (cd-k-type control-data))
               (cd-write-converter control-data)
               (cd-value-accepter control-data))
      (let ((user-focusout-handler (getf event-handlers :focusout)))
        (when user-focusout-handler
          (setf user-focusout-handler (ensure-fdefinition user-focusout-handler))
          (remf event-handlers :focusout))
        (push (acceptation-function user-focusout-handler)
              event-handlers)
        (push :focusout event-handlers))))
  event-handlers)

;;; applied at focusout time
(defun acceptation-function (user-focusout-handler)
  (lambda (event control-data)
    (when user-focusout-handler
      (funcall user-focusout-handler event control-data))
    (let* ((old-control-value (cd-old-control-value control-data))
           (new-control-value
            (get-basic-control-value (cd-control control-data)
                                     control-data))
           (equal? (basic-control-value-equal
                    (cd-k-type control-data)
                    old-control-value
                    new-control-value)))
      (unless equal?
        (let ((accept?
               (let ((new-lisp-value
                      (funcall (cd-write-converter control-data)
                               new-control-value control-data)))
                 (funcall (cd-value-accepter control-data)
                          new-lisp-value
                          control-data))))
          ;; new value rejected
          (unless accept?
            (restore-basic-control-value control-data)
            (beep)
            (setf (dd-value-rejected-by (cd-dialog-data control-data))
                  (cd-name control-data))           
            ))))))
                                                     
(defun set-layout-data (control layout-data-options &optional name)
  ;;(m :*layout-type *layout-type)
  (let ((layout-data
         (case *layout-type
           (:grid     ; :2c-grid est ramené à :grid
            (when-bind (value (getf layout-data-options :width))
              (setf (getf layout-data-options :widthHint) value)
              (remf layout-data-options :width))
            (when-bind (value (getf layout-data-options :height))
              (setf (getf layout-data-options :heightHint) value)
              (remf layout-data-options :height))
            (make-new griddata.))                 
           ((:row :column) (make-new rowdata.))                 
           ;(:form (make-new formdata.))
           ((:fill-h :fill-v)
            (warn "Layout data ignored (~s control).
SWT doesn't define layout data for ~s layout type (FillLayout)"
                  name *layout-type)
            nil)
           ((nil) (warn "Layout data ignored (~s control).
Layout data options has been specified
but there is no layout for this control: ~s" name)
            nil)
           (t (error "Unknown layout type: ~s" *layout-type)))))
    (when layout-data
      (set-layout-data-options layout-data layout-data-options)
      (setlayoutdata control layout-data))
    layout-data))

;;; initial-value : lisp value(s)
(defun store-initial-control-value (control k-control-type initial-value
                                            read-converter cd)
  (when read-converter
    (let ((control-value
           (unless (memberq k-control-type '(:list))
             (funcall read-converter initial-value cd))))
      (when cd
        (case k-control-type
          ;; combo comme un text (settext) ? pourquoi pas comme un list control (selection)
          ((:text :label :combo)
           (unless (typep control-value 'string)
             (warn "control-value should be a string: ~s in ~a control"
                   control-value k-control-type)
             (setf control-value (princ-to-string control-value)))
           (when (and initial-value  ;; peut être nil..
                      (read_only-combo? cd)
                      (not (member initial-value (cd-range cd)
                                   :test (lambda (m n)
                                           (funcall (cd-equality-test cd) m n)))))
             #+never
             (let ((range (cd-range cd)))
               ;; always nil , not implemented...
               (if (cd-reset-initial-value? cd)
                   (progn
                     (write-data/lisp (first range) cd)
                     (setf control-value (if range (princ-to-string (first range)) "")))
                 ))
             ;;
             (error "in READ-ONLY ~s combo, initial-value is not in the range.
value: ~s
range: ~s"
                    (cd-name cd) initial-value (cd-range cd)))
           (settext control control-value))
          (:button
           (let ((k-style (cd-k-style cd)))
             (cond ((and (text-button? k-style) (stringp control-value))
                    (button.settext control control-value))
                   ((state-button? k-style)
                    (button.setSelection control (ensure-boolean-value control-value)))
                   (t nil))))
          (:list
           (cond ((null initial-value) t)
                 ((cd-single? cd)
                  (when (listp initial-value)
                    (error "~s single list initial value should not be a list"
                           initial-value)))
                 (t (unless (listp initial-value)
                      (error "~s list initial value should be a list"
                             initial-value))))
           (cond
            ((listp initial-value)
             (let ((indices
                    (loop for item in initial-value
                          as i = (position item (cd-range cd)
                                           :test (cd-equality-test cd))
                          collect i
                          do (unless i
                               (error "in ~s list control, initial-value is not in the range: ~s"
                                      (cd-name cd) item)))))
               (list.setselection (cd-control cd)
                                  (make-j-vector indices :type :int))))
            (t (let ((pos (position initial-value
                                    (cd-range cd)
                                    :test (cd-equality-test cd))))
                 (if pos
                     (list.setselection control pos)
                   (error "in ~s list control, initial-value is not in the range: ~s"
                          (cd-name cd) initial-value)))))
           )
          (t nil))
        (values)))))


(defun reader-or-writer-name (accessor reader writer)
  (cond ((symbolp+ accessor) accessor)
        ((and (symbolp+ reader) (not (eq reader :%unspecified%))) reader)
        ((and (symbolp+ writer) (not (eq writer :%unspecified%))) writer)
        (T nil)))

;;; object : the control
(defun apply-methods (object shell k-control-type method-and-arg-pairs cd)
  (when (memberq k-control-type '(:combo :list))
    ;; forbid use of :setitems
    (let ((items (getf method-and-arg-pairs :setitems)))
      (when items
        (error "don't use :setitems in :combo or a :list control to set the range.
Use the :range argument instead and specify a range of lisp values"))
      ;; convert the range of lisp values (cd-range) to an array of string. to be given to
      ;; combo.setitems or list.setitems
      (setf (getf method-and-arg-pairs :setitems)            
            (make-combo-vector cd))))
  (loop for (method value) on method-and-arg-pairs by #'cddr
        do
        (cond
         ((eq method :setLayoutData)
          (warn ":setLayoutData method will be ignored.
 Please use :layout-data control option instead."))
         (t
          (let* ((function-string-name (symbol-name method))
                 (function-name (get.java.method.name object
                                                      function-string-name)))
            (when (functionp value)
              (setf value (funcall value object cd)))
            (when value
              (setf value
                    (cond ((listp value)
                           (substitute object
                                       :control
                                       (substitute shell :shell value :test #'eq)
                                       :test  #'eq))
                          ((eq value :control) object)
                          ((eq value :shell) shell)
                          (t value))))
            (if (and value (listp value))
                (apply (fdefinition function-name) object value)
              (funcall (fdefinition function-name) object value)))))))


(defun k-control-type (control-type)
    (let ((symbol-name (symbol-name control-type)))
      (intern (subseq symbol-name 0 (1- (length symbol-name)))
                    :keyword)))

;;; k-styles-or-integer: a list of keywords or an integer
(defun compute-style (k-styles-or-integer control-type k-control-type)
  (when (and (integerp k-styles-or-integer)
             (memberq k-control-type '(:text :label :combo :list :button)))
    (error "Keyword control type: ~s
Control style: ~d
If control type is a keyword and one of :text, :label, :combo, :list, or :button,
:style option must be a keyword or a list of keywords.
"
           k-control-type k-styles-or-integer))
  (let ((style
         (cond ((integerp k-styles-or-integer) k-styles-or-integer)
               (k-control-type
                (combine-styles k-control-type k-styles-or-integer))
               (control-type
                (let ((k-control-type (k-control-type control-type)))
                  (combine-styles k-control-type k-styles-or-integer)))
               (t nil))))
    (if style
        style
      *swt.none*)))


(defun ensure-control-type (control-type k-control-type value-type style)
  (let (Jcontrol-type)
    (cond (control-type
           (unless (boundp control-type)
             (error "not a valid control type: ~s" control-type))
           (unless k-control-type
             (setf k-control-type (k-control-type control-type)))
           (setf Jcontrol-type (symbol-value control-type)))
          (k-control-type
           (setf control-type (java-class-name k-control-type nil))
           (setf Jcontrol-type (symbol-value control-type)))
          ((eq value-type 'boolean)
           (setf style '(:check))
           (setf control-type 'button
                 k-control-type :button
                 Jcontrol-type button.))
          (t (setf control-type 'text.
                   k-control-type :text
                   Jcontrol-type text.)))
    (values Jcontrol-type control-type k-control-type style)))


(defun ensure-edit (edit)
  (cond ;; ((eq :lv edit) :lexical-variable)
        ((eq :sv edit)
         :special-variable)
        ((memberq edit '(:plist :clos :special-variable nil))
         edit)
        ((memberq edit '(:structure :struct))
         (error ":structure edit-specifier is not implemented"))
        ((eq :struct edit) :structure)
        (T (error "Unknown edit-specifier : ~S" edit))))

(defun ensure-prompt (prompt item edit reader-or-writer-name &key mnemonic?)
  (if (eq prompt :%unspecified%)
      (let ((p
             (cond (reader-or-writer-name
                    (string-capitalize reader-or-writer-name))
                   ((and item
                         (not (listp item))
                         (memberq edit '(:clos :special-variable :structure)))
                    (string-capitalize item))
                   (T nil))))
        (if (and p mnemonic?)
            (concatenate 'string "&" p)
          p))
    prompt))


;;; The initial Lisp value. If not specified will be computed from the reader (if one
;;; has been specified directly or implicitely). If no reader is available the initial
;;; value is token form the :item option if specified and :edit option is
;;; :lexical-variable 
(defun ensure-initial-value (default reader data item edit)
  (cond ((eq default :%unspecified%)
         (cond (reader
                (funcall reader data))
               ((eq edit :lexical-variable)
                item)
               (T nil)))
        (t default)))

(defun ensure-writer (writer accessor edit item name)
  (cond ;;((and name (eq edit :plist)) nil)
        ((eq writer :%unspecified%)
         (setf writer NIL))
        ((not writer)
         (return-from ensure-writer NIL)))
  (let ((prop-name (or accessor name)))
    (cond (writer
           (ensure-fdefinition writer))
          ((and prop-name (eq edit :plist))
           (lambda (new-value data)
             (setf (getf data prop-name) new-value)))
          (accessor
           (fdefinition (list 'setf accessor)))
          ((and item (eq edit :CLOS))
           (lambda (new-value data)
             ;; item should be the slot name
             (setf (slot-value data item) new-value)))
          ((eq edit :structure)
           (lambda (new-value data)
             (declare (ignore data new-value))
             (error "Not implemented")))
          ((and (eq edit :special-variable)
                item)
           (lambda (new-value data)
             (declare (ignore data))
             ;; item should be the special variale name
             (set item new-value)))
          ;; nil 
          (T NIL))))


(defun ensure-fdefinition (fun-name-or-object)
   (if (functionp fun-name-or-object) 
      fun-name-or-object
      (fdefinition fun-name-or-object)))


(defun ensure-reader (reader accessor edit item name)
  (cond ;;((and name (eq edit :plist)) (setf reader NIL))
        ((eq reader :%unspecified%)
         (setf reader NIL))
        ((not reader)
         (return-from ensure-reader NIL)))
  (let* ((prop-name (or accessor name))
         (reader-fn
          (cond (reader
                 (ensure-fdefinition reader))
                ((and prop-name (eq edit :plist))
                 (lambda (data)
                   (getf data prop-name)))
                (accessor
                 (fdefinition accessor))
                ((and item (eq edit :CLOS))
                 (lambda (data)
                   ;; item should be the slot name
                   (slot-value data item)))
                ((eq edit :structure)
                 ;; item should be the accessor name
                 (ensure-fdefinition item))
                ((and (eq edit :special-variable)
                      item)
                 (lambda (data)
                   (declare (ignore data))
                   ;; item should be the special variale name
                   (symbol-value item )))
                ;; nil or
                (T NIL))))
    (when reader-fn
      (lambda (data)
        (funcall reader-fn data)))))


(defun ensure-name (name k-control-type edit item reader-or-writer-name prompt)
  (let ((unique? nil))
    (flet ((new-name (e)
             (format nil "~a.~d" e *control-number)))
      (let ((name 
             (cond (name
                    (check-type name symbol)
                    name)
                   (T (intern (string-upcase 
                               (cond ((and item 
                                           (symbolp item)
                                           (memberq edit '(:CLOS :special-variable)))
                                      item)
                                     (reader-or-writer-name reader-or-writer-name)
                                     ((and prompt (not (eq prompt :%unspecified%)))
                                      prompt)
                                     (T (setf unique? T)
                                        (new-name k-control-type))))
                              :keyword)))))
        (cond (unique? name)
              ((find-control-data name *dialog-data nil)
               (intern (string-upcase (new-name name))
                       :keyword))
              (t name))))))


(defun ensure-value-type (type edit item initial-lisp-value data)
  (cond (type type)
        ((and (eq edit :CLOS)
              ;; item : slot name
              item
              data
              (slot-definition-type+ item data)))
        (initial-lisp-value
         (cond ((stringp initial-lisp-value)
                    'string)
                   ((numberp initial-lisp-value)
                    'number)
                   ((keywordp initial-lisp-value)
                    'keyword)
                   ((symbolp initial-lisp-value)
                    'symbol)
                   (T NIL)))
          (T NIL)))

;;; simple-item-spec : atomic-control | (atomic-control [:edit edit-value])
(defun convert-simple-item-spec (a)
  (if (listp a)
      (if (keywordp (first a))
          a
        (cons :item a))
    (list :item a)))