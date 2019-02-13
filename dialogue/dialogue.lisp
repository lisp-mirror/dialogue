;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm


(in-package :dialogue)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim (cons 'special '(*control-number
                             *dialog-name
                             *dialog-data
                             *layout-type *layout-info
                             *exit-controls
                             *default-button-name
                             *column-in-row-count
                             ))))

(defun dialogue
       (;; specification of the controls and layouts
        ;; see syntax in the documentation
        compound
        &key
        ;; make the shell visible and active
        (open? t)
        ;; make the shell visible and in front of all other shells (not active)
        (move-above? nil)
        ;; usually a shell (makes a dialog shell)
        ;; If a display, makes a top-level shell
        (parent (or *default-swt-parent* *display*))
        ;; Dialog (shell) modality
	;; one of : :parent, :application, :system , :none (or nil)
	(modality :parent)
        ;; Dialog (shell) trimmings
        ;; an unordered list of keywords to choose in: :border, :close, :min, :max,
        ;; :resize, :title, :no-trim and the standard trimmings :shell-trim and :dialog-trim.
        ;; If nil, defaults to :shell-trim or :dialog-trim (to :shell-trim if the parent is
        ;; a display, see parent argument)
        ;; Note:
        ;; :dialog-trim : same as (:title :border :close)
        ;; :shell-trim : same sa (:title :close :min :max :resize)
        (trimmings *dialogue-default-trimmings*)
        ;; see with-shell&dispatch
        (main nil)
        ;; a string (shell.settext)
        (title nil)
        ;; Dialog (shell) options: a list of keyword and value pairs.
        ;; see documentation about the composite options (syntax of a composite)
        ;; Note: instead of :setText you can use Title argument. Title argument has
        ;; precedence.
        (options nil)
        ;; dialog name (must be a symbol and is usually unique, see reuse? and
        ;; dispose-existing-shell? below. Notice if name is not unique, a control can't
        ;; be found on the dialog name using find-control - use *dialog-data, etc. instead.
        ;; If the name is not provided will be a keyword made from the title, text options
        ;; or gensym
        (name nil)
        ;; if reuse? is nil, dispose an existing shell with same name
        (dispose-existing-shell? t)
        ;; If T, reuse and make visible the dialog if it exists. If doesn't exist, make and
        ;; open the dialog (open = visible and active)
        (reuse? T)
        ;; one of: :minimized :maximized :previous :normal nil
        (state-on-reuse :normal)
        ;; to open the shell (make it visible and active) or just make it visible
        (on-reuse-open? nil)
        ;; a list of exit controls specifications or
        ;; T to get 2 ok and cancel default buttons (NIL: no buttons).
        ;; See cancel-control, ok-control and make-exit-controls-composite-spec functions.
        (exit-controls T)
        ;; if there is a button named :ok, set it as the default button
        (default-button-name :ok)
        ;; the name of the control that should have the focus at opening
        ;; time
        focused-control-name
        ;; One of nil, :nice or a foreign reference to a SWT point (to set
        ;; the top-left corner of the shell).
        ;; If :nice, compute the location from the cursor location and the size
        ;; of the dialog, unless one of the left or top arguments is given
        (location :nice)
        left top
        ;; to center the dialog on the screen (not implemented)
        (center NIL)
        ;; ~~~ Init and closing functions: ~~~
        ;; a 1 argument function to be called at creation time (argument: the shell) 
        (creation-init-function nil)
        ;; a 1 argument function to be called at opening time (argument: the shell), after
        ;; the call to shell.pack. If the shell is reused, called before the call
        ;; to shell.setvisible
        (init-function nil)
        ;; a 2 arguments function to be called on the close event (arguments : shell and event)
        ;; If the function returns T, the shell is disposed as well (default). If it returns
        ;; NIL the shell is not disposed (possibly this is not working on all OS).
        ;; On the close event, the :close property of the property list returned
        ;; by dialogue are set to T (see result parameter below).
        ;; Note : the close event is sent when the UI session is ending, before
        ;; SWT runs the dispose method (and possibly the dispose event is sent).
        (close-function nil)
        ;; a plist that can be accessed using (dd-plist (find-dialog-data <thing>))
        (plist nil)
        ;; ~~~  A property list returned by dialogue as second value.
        ;; :ok property value is set to T in the selection handler of the default OK button.
        ;; :cancel property value is set to T in the default Cancel button selection handler.
        ;; See close-function parameter above.
        ;; Note: first value returned by dialogue is the value of the :ok property.
        (result (list :ok nil :cancel nil :close nil))
        ;; ~~~ Defaults for the control specifications: ~~~
        (font *dialogue-default-font*)
        ;; the default edit-specifier which may be:
        ;; :CLOS , :sv (or :special-variable) , :plist, NIL , :structure (not implemented)
        (edit *edit-specifier*)
        ;; the default object (CLOS instance, plist, structure) to possibly be accessed by
        ;; each widget via the 
        ;; reader and the writer . NIL means ignore this argument.
        (data nil))
  (declare (ignore center))
  (unless modality (setf modality :none))
  (let ((top-level? (typep_foil parent 'display.))
        *dialog-name
        *dialog-data
        (*default-button-name default-button-name)
        (*exit-controls (if (eq t exit-controls)
                            (list (ok-control) (cancel-control))
                          exit-controls))
        (*control-number 0)
        (*dialogue-default-font* font))
    (declare (special *control-number *dialog-name *dialog-data
                      *default-button-name *exit-controls
                      *dialogue-default-font*))
    ;; clean-up - voir aussi get-dialog qui nettoie de même
    (setf *dialogs* (remove-disposed-instances *dialogs*
                                               :k-type :shell
                                               :key #'dd-dialog))
    (setf *dialog-name (ensure-dialog-name name title options)) 
    (let ((existing-shell (get-dialog *dialog-name)))
      ;; *dialogs*
      (cond (reuse?
             (cond ((not existing-shell)
                    (setf reuse? nil))
                   (t nil)))
            ((and existing-shell dispose-existing-shell?)
             (shell.dispose existing-shell))
            (t nil))
      (cond
       (reuse? nil) ;; (setf *DLG-dialog* existing-shell))
       (t                     
        (with-shell&dispatch (shell
                              :open? open?
                              :move-above? move-above?
                              :parent parent
                              :modality modality
                              :trimmings trimmings
                              :top-level? top-level?
                              :location location
                              :left left
                              :top top
                              :main main
                              :init-function init-function)
          (setdata shell (symbol-name *dialog-name))
          (when font
            (ignore-errors+ (progn
                              (warn "Unvalid font: ~s" font)
                              (setf font nil *dialogue-default-font* nil))
              (shell.setfont shell font)))
          (apply-setX-options shell options)
          (when title (settext shell title))
          (setf *dialog-data (make-dialog-data
                              :name *dialog-name
                              :dialog shell
                              :result result
                              :plist plist))
          ;; ... et remplacement éventuel de l'ancien di (fermé plus haut) par le nouveau
          ;;(setf (getf *dialogs* *dialog-name) *dialog-data)
          ;; pour des dialogues de même nom
          (setf *dialogs* (cons *dialog-name (cons *dialog-data *dialogs*)))
          (process-compound compound shell edit data)
          (when default-button-name
            (when-bind (b (find-control default-button-name *dialog-data nil))
              (apply-j-method :setdefaultButton shell b)))
          (when focused-control-name
            (when-bind (c (find-control focused-control-name *dialog-data nil))
              (apply-j-method :setfocus c)))
          (set-close-function shell close-function *dialog-data)
          (when creation-init-function
            (funcall creation-init-function shell))
          )))
      (cond (reuse?
             (when init-function
               (funcall init-function existing-shell))
             (case state-on-reuse
               (:minimized (shell.setminimized existing-shell t))
               (:maximized (shell.setmaximized existing-shell t))
               (:previous (shell.setmaximized existing-shell nil))
               (:normal
                (cond ((shell.getminimized existing-shell)
                       (shell.setminimized existing-shell nil))
                      ((shell.getmaximized existing-shell)
                       (shell.setmaximized existing-shell nil))
                      (t (do-shell existing-shell move-above? on-reuse-open?)))
                ;;(shell.setsize existing-shell (shell.getsize existing-shell))
                )
               (t (when state-on-reuse
                    (error "Unknow state-on-reuse: ~s" state-on-reuse))))
             (do-shell existing-shell move-above? on-reuse-open?))
            (t
             (let ((plist (dd-result *dialog-data)))
               (values (getf plist :ok) plist))))
      ) ))

(defun do-shell (shell move-above? on-reuse-open?)
  (cond (move-above?
         (shell.setvisible shell T)
         (shell.moveabove shell nil))
        (on-reuse-open? (shell.open shell))
        (t (shell.setvisible shell T)))
  )

(defun set-close-function (shell close-function dialog-data)
  (shell.addshelllistener
   shell
   (new-proxy
    p +MARSHALL-ID+ 0
    (shelllistener.
     (shellclosed
      (event)
      (setf (getf (dd-result dialog-data) :close) t)        
      (let ((doit (if close-function (funcall close-function shell event) t)))
        (ensure-boolean-value doit)
        (setf (shellevent.doit event) doit)
        doit)))))
  #+NEVER   ;; marche pas !
  (shell.addlistener
   shell
   *swt.close*
   (new-proxy
    p +MARSHALL-ID+ 0
    (listener.
     (handleevent
      (event)
      (let ((doit (funcall close-function shell event)))
        (ensure-boolean-value doit)
        (setf (event.doit event) doit))
      nil)))))


;;; k-type (ex. :menu or :shell), more efficient if all the instances are of the
;;; same type
;; (fdefinition (get.java.method.name :menu :isdisposed))  *dialogs*
(defun remove-disposed-instances (plist &key k-type key)
  (let ((method (if k-type
                    (fdefinition (get.java.method.name k-type :isdisposed))
                  "ISDISPOSED")))
    (nreverse (loop for sublist on plist by #'cddr
                    as item = (second sublist)
                    as i = (if key
                               (funcall key item)
                             item)
                    ;; collect if not disposed
                    unless (handler-case
                               (apply-j-method method i)
                             (t (c)
                                T))
                    collect item
                    and
                    collect (first sublist)))))



(defun ensure-dialog-name (name title options)
  (cond (name
         (check-type name symbol)
         name)
        (t (intern (string-upcase
                    (or title
                        (getf options :text)
                        (getf options :settext (gensym))))
                   :keyword))))

; (trace process-compound process-layout)
(defun process-compound (compound shell edit data)
  (cond ((composite? compound)
         ;; default to :column layout
         (let ((layout-type :column))
           (warn "no layout for the shell. Shell layout-type is set to ~s" layout-type)
           (setf compound (list layout-type (list compound)))))
        ;; default to :2c-grid layout - convenience syntax:  compound : ( {layout-component}* )
        ((not (layout? compound))
         (setf compound (list :2c-grid compound))))
  (process-layout compound shell edit data))


(defun process-control-or-composites (control-or-composites
                                      parent edit data)
  (loop for control-or-composite in control-or-composites
        do (process-control-or-composite control-or-composite
                                         parent edit data)))

;;; ****************************************************************************
;;;

;;; (:column ((:text...)))   
;;; parent : the shell
(defun process-layout (layout parent edit data)
  (let ((*layout-type (first (memberq (first layout) +layout-types+)))
        layout-instance)
    (declare (special *layout-type))
    (multiple-value-bind (options components)
        (split-kv-pairs-and-things (rest layout))
      (setf components (first components))
      ;; (m :*layout-type *layout-type)
      (case *layout-type
        (:grid
         (setf layout-instance
               (process-grid-layout components options parent edit data)
               ))
        (:2c-grid
         (process-2c-grid-layout options components parent edit data))
        ((:row :column)
         (setf layout-instance (make-new rowlayout. (if (eq :row *layout-type)
                                                        *swt.horizontal*
                                                      *swt.vertical*)))
         (set-layout-options layout-instance options)
         (setlayout parent layout-instance)
         (process-control-or-composites (possibly-add-exit-controls components)
                                        parent edit data)
         )
        ((:fill-h :fill-v)
         (setf layout-instance (make-new filllayout. (if (eq :fill-h *layout-type)
                                                         *swt.horizontal*
                                                       *swt.vertical*)))
         (set-layout-options layout-instance options)
         (setlayout parent layout-instance)
         (process-control-or-composites (possibly-add-exit-controls components)
                                        parent edit data))
        (t (error "layout-type is unknown or not implemented: ~s"
                  *layout-type)))
      layout-instance)))

(defun process-grid-layout (grid-rows options parent edit data)
  (let (layout-instance
        (*layout-info nil))
    (declare (special *layout-info))
    (unless grid-rows
      (error "process-layout: no components in :grid layout."))
    (let* ((columns-number
            (if-bind
             (cn (getf options :numColumns nil))
             cn
             ;; weak? This works iff there is at least one row that has a number of control
             ;; specifications equal to the number of columns. Could be improved...
             ;; see example and :span tag use in ex\grid.lisp
             (if grid-rows
                 (apply #'max (mapcar #'length grid-rows))
               0)))    
           (rows-number (length grid-rows))
           (v-span-correction (make-array rows-number :initial-element 0))
           (row-i 0))
      ;;(m :process-grid-layout :columns-number columns-number (getf options :numColumns nil))
      (setf *layout-info (list :columns-number columns-number
                               :v-span-correction v-span-correction))
      (setf layout-instance (make-new gridlayout. columns-number
                                      (getf options :makeColumnsEqualWidth nil)))
      (setf options (filter-key&value-pairs options
                                            '(:numColumns
                                              :makeColumnsEqualWidth)))
      (set-layout-options layout-instance options)
      (setlayout parent layout-instance)
      (dolist (grid-row grid-rows)
        ;;(m :row-i row-i)
        (setf (getf *layout-info :row-i) row-i)
        (let (;; *column-in-row-count: to count the cells using true controls specifications
              ;; (not :span) and including the :horizontalspan value in layout-data
              ;; See update in set-layout-data.
              ;; Only to add dummy-widgets to complete each row (see below)
              (*column-in-row-count 0))
          (declare (special *column-in-row-count))
          (dolist (control-or-composite grid-row)
            ;; :span is ignored
            (unless (eq :span control-or-composite)
              (incf *column-in-row-count)
              (process-control-or-composite control-or-composite
                                            parent edit data)))
          ;; complete row with dummy widgets:
          (dotimes (i (- columns-number
                         *column-in-row-count
                         (svref v-span-correction row-i)))
            (make-dummy-widget parent))
          (incf row-i)))
      ;; bug! see grid.lisp example file
      (when *exit-controls
        (process-composite (exit-composite-spec) parent edit data))
      layout-instance)))


;;; layout-data : fref
;;; options : a list like (:HORIZONTALSPAN 3 :HORIZONTALALIGNMENT 4)
;;; Used in: (set-layout-data control layout-data-options)
;;; - Update some values in layout-data-options
;;; - set the options in the fref layout-data
(defun set-layout-data-options (layout-data options)
  (loop for (keyword value) on options by #'cddr
        do
        (unless (keywordp keyword)
          (error "Malformed options list: ~s" options))
        (cond
         ((eq :horizontalSpan keyword)
          (when (eq :all value)
            (let ((columns-number (getf *layout-info :columns-number)))
              (unless columns-number
                (ok-di
                 (format nil "unvalid use of the :all keyword:~% ~s" options)))
              ;; il faudrait aussi décompter v-span-correction mais sans importance si
              ;; value est trop grand
              (setf value (1+ (- columns-number
                                 *column-in-row-count)))))
          (when (boundp '*column-in-row-count)
            (incf *column-in-row-count (1- value))))
         ((eq :verticalSpan keyword)
          (let ((v-span-correction (getf *layout-info :v-span-correction))
                (row-i (getf *layout-info :row-i))
                (h-span (getf options :horizontalSpan 1)))
            (loop for i from (1+ row-i) below (+ row-i value)
                  do
                  (incf (svref v-span-correction i) h-span))))
         (t nil))
        (let ((fdefinition
               (fdefinition (list 'setf (get.java.method.name layout-data keyword)))))
          (with-nice-error-msg
              (format nil "unvalid layout option or value:
keyword: ~s
value: ~s" keyword value)
            (if (and value (listp value))
                (apply fdefinition value layout-data)
              (funcall fdefinition value layout-data))))))



(defun possibly-add-exit-controls (components &key
                                              layout-data)
  (if-bind (exit-composite-spec (exit-composite-spec :layout-data layout-data))
           (append components
                   (list exit-composite-spec))
           components))


;;; define a composite control for the exit-controls
;;; The exit-controls are in a row layout
(defun exit-composite-spec (&key
                            (exit-controls-spec *exit-controls)
                            ;; default layout data option for :grid
                            (layout-data
                             `(;; 4 ou plus, peu improte si il y a moins de cols
                               :horizontalspan 4
                               :horizontalalignment ,*swt.fill*))
                            (margintop 20))
  (when *exit-controls
    (prog1
        (list
         :composite :layout-data layout-data
         (list :row
               :justify t :pack nil :margintop margintop :marginbottom 0
               (if (= (display.getdismissalalignment *display*)
                      *swt.left*)
                   exit-controls-spec
                 (rotate exit-controls-spec))))
      (setf *exit-controls nil))))


(defun process-2c-grid-layout (options components parent edit data)
  (let ((exit-controls
         (if *exit-controls
             (list
              ;; grid-row of one composite that spans on 2 columns
              (list
               (exit-composite-spec)))
           nil)))
    (process-layout
     (nconc (cons :grid
                  options)
            (list
             (nconc
              (mapcar (lambda (cc)  ; control-or-composite
                        (let ((cc (convert-simple-item-spec cc)))
                          (list (list
                                 :type :label
                                 :ignore (getf cc :ignore)
                                 :initial-value (compute-2cg-prompt cc edit)
                                 :layout-data (list :horizontalalignment
                                                    *prompt-alignemnt*))
                                (update-control-spec-in-2cg-layout cc))))
                      components)
              exit-controls)))
     parent
     edit
     data)))


;;; if CC is a control without :horizontalalignment layout option specified,
;;; add :horizontalalignment and *swt.fill* in the layout specification
;;; etc.
(defun update-control-spec-in-2cg-layout (cc)  ; cc: control-or-composite
  (unless (composite? cc)
    (let ((layout-short-name? nil))
      (multiple-value-bind (layout-data present?)
          (getf+ cc :layout-data)
        (unless present?
          (multiple-value-setq (layout-data present?)
              (getf+ cc :layout))
          (when present?
            (setf layout-short-name? t)))
        (let ((grab-hv? (or (and (eq :text (getf cc :type))
                                 (memberq :multi (getf cc :style)))
                            (eq :list (getf cc :type)))))
          #+never
          (unless layout-data
            (remf cc (if layout-short-name? :layout :layout-data))
            (setf present? nil))
          (cond (present?
                 (unless (prop-present? layout-data :horizontalalignment)
                   (setf (getf layout-data :horizontalalignment) *swt.fill*))
                 (unless (prop-present? layout-data :grabexcesshorizontalspace)
                   (setf (getf layout-data :grabexcesshorizontalspace) T))
                 (when grab-hv?
                   (unless (prop-present? layout-data :verticalalignment)
                     (setf (getf layout-data :verticalalignment) *swt.fill*))
                   (unless (prop-present? layout-data :grabexcessverticalspace )
                     (setf (getf layout-data :grabexcessverticalspace) T)))
                 (setf (getf cc (if layout-short-name? :layout :layout-data)) layout-data)
                 )
                (t (setf (getf cc :layout-data)
                         (nconc
                          (list :horizontalalignment *swt.fill*
                                :grabexcesshorizontalspace t)
                          (if grab-hv?
                              (list
                               :verticalalignment *swt.fill*
                               :grabexcessverticalspace t))))))
          ))))
  cc)
  

(defun compute-2cg-prompt (cc edit)   ; cc: control-or-composite
  (when (atom cc) (setf cc (convert-simple-item-spec cc)))
  (when (composite? cc)
    (return-from compute-2cg-prompt
      (getf-secure (rest cc) :prompt)))
  (when-bind (prompt (getf cc :prompt))
    (return-from compute-2cg-prompt prompt))
  (let ((prompt :%unspecified%)
        (edit (getf cc :edit edit))
        (accessor (getf cc :accessor))
        (reader (getf cc :reader))
        (writer (getf cc :writer))
        (item (getf cc :item))
        reader-or-writer-name)
    (setf edit (ensure-edit edit))
    (setf reader-or-writer-name (reader-or-writer-name accessor reader writer))
    (ensure-prompt prompt item edit reader-or-writer-name :mnemonic? t)))
    

;;; a layout or a data-layout object
(defun set-layout-options (layout options)
  (loop for (keyword value) on options by #'cddr
        do
        (unless (keywordp keyword)
          (error "Malformed options list: ~s" options))
        (let ((fdefinition
               (fdefinition (list 'setf (get.java.method.name layout keyword)))))
          (with-nice-error-msg
              (format nil "unvalid layout option or value:
keyword: ~s
value: ~s" keyword value)
            (if (and value (listp value))
                (apply fdefinition value layout)
              (funcall fdefinition value layout))))))


(defun process-control-or-composite (control-or-composite parent edit data)
  (cond ((not control-or-composite)
         (make-dummy-widget parent))
        ((composite? control-or-composite)
         (process-composite control-or-composite parent edit data))
        (t (process-control-spec control-or-composite
                                 parent :edit edit :data data))))


(defun process-composite (composite parent edit data)
  (let ((type (first composite))
        cd)
    (multiple-value-bind (options components)
        (split-kv-pairs-and-things (rest composite))
      (let* ((styles (or (getf options :styles nil)
                         (getf options :style nil)))
             (name (getf options :name nil))
             (options (filter-key&value-pairs options
                                              '(:style :styles
                                                :name)))
             (parent (make-new (java-class-name type)
                               parent
                               (if styles
                                   (combine-styles type styles)
                                 *swt.none*)))
             (first-component (first components)))
        (setf name (ensure-name name type edit nil nil nil))
        (when (or *make-composite-control-data?*
                  ;; need to find a sashform by name to set the sash weights at creation time
                  (memberq type '(:sashform)))
          (setf cd (make-control-data
                    :name name
                    :control parent
                    :k-type type
                    :k-style styles
                    :number *control-number
                    :data data
                    :dialog-data *dialog-data))
          (push cd (dd-control-data-list *dialog-data))
          (push name (dd-control-data-list *dialog-data)))
        (incf *control-number)
        (apply-setX-options parent options)
        (cond ((eq :tabfolder type)
               (process-tab-folder components parent edit data))
              ((eq :sashform type)
               (process-sashform first-component parent edit data))
              ((layout? first-component)
               (process-layout first-component parent edit data))
              (t
               ;; layout default
               (process-layout (cons :2c-grid components)
                               parent edit data)))
        parent))))

;;; parent : the sashform
(defun process-sashform (control-or-composite-items parent edit data)
  (dolist (cc control-or-composite-items)
    (process-control-or-composite cc parent edit data)))


(defun process-tab-folder (tab-item-specs tabfolder-object edit data)
  (dolist (item-spec tab-item-specs)
    (let ((tabItem-object (tabitem.new tabFolder-object *swt.none*)))
      (multiple-value-bind (options control-or-composites)
          (split-kv-pairs-and-things (rest item-spec))
        (tabitem.settext tabItem-object (first item-spec))
        (apply-setX-options tabItem-object options)
        (dolist (control-or-composite control-or-composites)        
          (let ((cc-object
                 (process-control-or-composite control-or-composite
                                               tabFolder-object edit data)))
            (tabitem.setcontrol tabItem-object cc-object)
            ))))))
