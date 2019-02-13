;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

;;;; Abbreviations:
;;;; dd: Dialog-Data (defstruct)
;;;; cd: Control-Data (defstruct)

(in-package :dialogue)


;; (find-control-data :PREVIOUS-SEARCH :search-dialog)
;;; Thing argument can be unspecified if name-or-control-or-control-data is a control-data
;;; Get the (lisp) value of a control
(defun get-control-value (name-or-control-or-control-data
                          &optional thing &key (errorp t))
  (let (dd name cd)
    (cond ((control-data-p name-or-control-or-control-data)
           (setf cd name-or-control-or-control-data))
          (t
           (unless thing
             (error "in get-control-value, thing argument must be given: ~s"
                    name-or-control-or-control-data))
           (setf dd (find-dialog-data thing))
           (cond ((keywordp name-or-control-or-control-data)
                  (setf name name-or-control-or-control-data))
                 ;; name-or-control-or-control-data is a control
                 (t (setf name (get-control-name name-or-control-or-control-data))))
           (setf cd  (getf (dd-control-data-list dd) name))))
    (cond (cd 
           (let* ((control (cd-control cd))
                  (control-value (get-basic-control-value control cd))
                  (data (cd-data cd))
                  (write-converter (cd-write-converter cd)))
             (if (and data write-converter)
                 (funcall write-converter control-value cd)
               control-value)))
          (errorp (error "can't find control: ~s" name))
          (t nil))))

(defun get-controls-values (names thing &key (errorp t))
  (values-list
   (loop for n in names
         collect (get-control-value n thing :errorp errorp))))

;;; Value: control value (not lisp)
;;; If a list control, value may be a string, an integer (indice) or a list of ~ (possibly empty)
;;;!? add an argument to optionaly update the control-data too ?
(defun set-control-value (control-name value thing &key (errorp t))
  (let ((cd (find-control-data control-name thing errorp)))
    (set-basic-control-value cd value)))

(defun set-controls-values (controlName-value-pairs thing &key (errorp t))
  (loop for (name value) on controlName-value-pairs by #'cddr
        do
        (set-control-value name value thing :errorp errorp)))


(defun set-control-enabled (control-name-or-names value thing &key  (errorp t))
  (ensure-boolean-value value)
  (dolist (control-name (if (listp control-name-or-names) control-name-or-names
                          (list control-name-or-names)))
    (let ((control (find-control control-name thing errorp)))
      (control.setenabled control value))))


(defun reset-combo-or-list (control-name thing)
  (set-range control-name thing nil :value ""))


(defun get-range (control-name thing &key
                               (lisp-range? t)
                               (errorp t))
  (let* ((cd (find-control-data control-name thing errorp))
         (control (cd-control cd))
         (k-type (cd-k-type cd))
         (range (j-vector-to-list
                 (if (eq k-type :combo)
                     (combo.getitems control)
                   (list.getitems control))))
         (write-converter (cd-write-converter cd)))
    (if lisp-range?
        (if (eq k-type :combo)
            (if write-converter
                (loop for i in range collect (funcall write-converter i cd))
              (cd-range cd))
          (cd-range cd))
      (j-vector-to-list
       (if (eq k-type :combo)
           (combo.getitems control)
         (list.getitems control))))))


;;; to set the range in list and combo controls
;;; thing: see find-dialog-data
;;; range: a list of control values
;;; lisp-range: corresponds to range (lisp values)
(defun set-range (control-name thing range &key
                               (errorp t)
                               (lisp-range range lisp-range-p)
                               ;; a control value (not lisp) to set the control
                               (value nil value-p)
                               ;; if value is not provided and first-value? is T
                               ;; set the control to the first value of range
                               (first-value? nil)
                               (maximum-length nil))
  (cond (maximum-length
         (let ((length (length range)))
           (when (> length maximum-length)
             (setf range (subseq range 0 maximum-length)
                   lisp-range (if lisp-range-p
                                  (subseq lisp-range 0 maximum-length)
                                range)))))
        (t (let ((length (length range))
                 (limit (- call-arguments-limit 3)))
             (when (> length limit)
               (ok-di (format nil "To many items in range: ~d
Range will be shortened to ~d" length limit))
               (setf range (subseq range 0 limit)
                     lisp-range (if lisp-range-p
                                    (subseq lisp-range 0 limit)
                                  range))))))               
  (let* ((cd (find-control-data control-name thing errorp))
         (control (cd-control cd))
         (k-type (cd-k-type cd))
         (write-converter (cd-write-converter cd))
         ;; for the combo only
         (lisp-range
          (if lisp-range-p
              lisp-range
            (if (eq k-type :combo)
                (if write-converter
                    (loop for i in range collect (funcall write-converter i cd))
                  range)
              lisp-range))))
    (case k-type
      (:list
       (list.setitems control (funcall #'make-j-vector range))
       (setf (cd-range cd) lisp-range))
      (:combo
       (combo.setitems control  (funcall #'make-j-vector range))
       (setf (cd-range cd) lisp-range))
      (t (error "meaningless to set the range in a ~s control" k-type)))
    (when-bind (value (if value-p
                          (if value value "")
                        (when first-value?
                          (if-bind (f (first range))
                                   f
                                   ""))))
      (set-control-value control-name value thing))))


;;; to add to the range in list and combo controls
;;; thing: see find-dialog-data
;;; range: a list of control values to add
;;; lisp-range: corresponds to range (lisp values)
(defun add-to-range (control-name thing range
                                  &key
                                  (errorp t)
                                  (lisp-range range lisp-range-p)
                                  ;; a control value (not lisp) to set the control
                                  (value nil value-p)
                                  ;; if value is not provided and first-value? is T
                                  ;; set the control to the first value of range
                                  (first-value? nil)
                                  (remove-duplicates? t)
                                  ;;(maximum-length nil)
                                  (control-value-test #'equalp)
                                  )
  (let ((length (length range))
        (limit (- call-arguments-limit 3)))
    (when (> length limit)
      (ok-di (format nil "To many items in range: ~d
Range will be shortened to ~d" length limit))
      (setf range (subseq range 0 limit)
            lisp-range (if lisp-range-p
                           (subseq lisp-range 0 limit)
                         range))))             
  (let* ((cd (find-control-data control-name thing errorp))
         (old-lisp-range (cd-range cd))
         (control (cd-control cd))
         (k-type (cd-k-type cd))
         (old-range (j-vector-to-list
                     (if (eq k-type :combo)
                         (combo.getitems control)
                       (list.getitems control))))
         (write-converter (cd-write-converter cd))
         (lisp-range
          (if lisp-range-p
              lisp-range
            (if (eq k-type :combo)
                (if write-converter
                    (loop for i in range collect (funcall write-converter i cd))
                  range)
              lisp-range)))
         (new-lisp-range (append old-lisp-range lisp-range))
         (new-range (append old-range range)))
    (when remove-duplicates?
      (setf new-lisp-range (remove-duplicates new-lisp-range :test #'equal))
      (setf new-range (remove-duplicates new-range :test control-value-test)))
    (case k-type
      (:list
       (list.setitems control (funcall #'make-j-vector new-range)))
      (:combo
       (combo.setitems control (funcall #'make-j-vector new-range)))
      (t (error "meaningless to set the range in a ~s control" k-type)))
    (setf (cd-range cd) new-lisp-range)
    (when-bind (value (if value-p
                          (if value value "")
                        (when first-value?
                          (if-bind (f (first new-range))
                                   f
                                   ""))))
      (set-control-value control-name value thing))))


(defun remove-from-range (control-name thing range
                                       &key
                                       (errorp t)
                                       (lisp-range range lisp-range-p)
                                       ;; a control value (not lisp) to set the control
                                       (value nil value-p)
                                       ;; if value is not provided and first-value? is T
                                       ;; set the control to the first value of range
                                       (first-value? nil)
                                       ;;(maximum-length nil)
                                       )
  (let ((length (length range))
        (limit (- call-arguments-limit 3)))
    (when (> length limit)
      (ok-di (format nil "To many items in range: ~d
Range will be shortened to ~d" length limit))
      (setf range (subseq range 0 limit)
            lisp-range (if lisp-range-p
                           (subseq lisp-range 0 limit)
                         range))))             
  (let* ((cd (find-control-data control-name thing errorp))
         (old-lisp-range (cd-range cd))
         (control (cd-control cd))
         (k-type (cd-k-type cd))
         (old-range (j-vector-to-list
                     (if (eq k-type :combo)
                         (combo.getitems control)
                       (list.getitems control))))
         (write-converter (cd-write-converter cd))
         (lisp-range
          (if lisp-range-p
              lisp-range
            (if (eq k-type :combo)
                (if write-converter
                    (loop for i in range collect (funcall write-converter i cd))
                  range)
              lisp-range)))
         (new-lisp-range (set-difference old-lisp-range lisp-range :test #'equal))
         (new-range (set-difference old-range range :test #'equalp)))
    (case k-type
      (:list
       (list.setitems control (funcall #'make-j-vector new-range)))
      (:combo
       (combo.setitems control (funcall #'make-j-vector new-range)))
      (t (error "meaningless to set the range in a ~s control" k-type)))
    (setf (cd-range cd) new-lisp-range)
    (when-bind (value (if value-p
                          (if value value "")
                        (when first-value?
                          (if-bind (f (first new-range))
                                   f
                                   ""))))
      (set-control-value control-name value thing))))

;;; ****************************************************************************

;;; For each basic control that has a writer and write-converter
;;; apply the writer iff the new value is different from  the
;;; initial value
;;;
;;; 1. get the new control-value (see get-basic-control-value function)
;;; 2. convert to the new lisp-value (:write-converter option)
;;; 3. compare the new lisp-value and the initial-lisp-value
;;;     (:equality-test option)
;;; 4. if they are different, using the new value:
;;;   - call the writer function (:writer option) 
;;;   - set cd-new-lisp-value and cd-new-lisp-value to
;;; 
(defun update-data (dialog-data &key after-hook)
  ;;? (setf (dd-written-back-controls dialog-data) nil)
  (do-controls (cd dialog-data :filter #'basic-control?)
    (when-bind (writer (cd-writer cd))
      (when-bind (write-converter (cd-write-converter cd))
        (let* ((control (cd-control cd))
               (control-value (get-basic-control-value control cd))
               (data (cd-data cd))
               (new-lisp-value (funcall write-converter control-value cd)))
          (m :dialog-name (dd-name dialog-data) (cd-name cd) (cd-initial-lisp-value cd)new-lisp-value)
          (unless (funcall (cd-equality-test cd)
                           new-lisp-value
                           (cd-initial-lisp-value cd))
            (funcall writer new-lisp-value data)
            (setf (cd-new-lisp-value cd) new-lisp-value)
            (setf (cd-initial-lisp-value cd) new-lisp-value)
            (push cd (dd-written-back-controls dialog-data))
            )))))
  (when after-hook
    (funcall after-hook dialog-data)))

(defun write-data (control-value cd)
  (when-bind (writer (cd-writer cd))
    (when-bind (write-converter (cd-write-converter cd))
      (let (;;(control (cd-control cd))
            (data (cd-data cd))
            (new-lisp-value (funcall write-converter control-value cd)))
        ;; (m (cd-name cd) (cd-initial-lisp-value cd)new-lisp-value)
        (funcall writer new-lisp-value data)
        (setf (cd-new-lisp-value cd) new-lisp-value)
        (setf (cd-initial-lisp-value cd) new-lisp-value)
        ))))

(defun write-data/lisp (lisp-value cd)
  (when-bind (writer (cd-writer cd))
    (let (;;(control (cd-control cd))
          (data (cd-data cd)))
      (funcall writer lisp-value data)
      (setf (cd-new-lisp-value cd) lisp-value)
      (setf (cd-initial-lisp-value cd) lisp-value)
      )))
  

(defun data-written-back? (control-data &optional dialog-data)
  (unless dialog-data
    (setf dialog-data (find-dialog-data control-data)))
  (memberq control-data (dd-written-back-controls dialog-data)))

;;; control : a basic control
#+old
(defun get-basic-control-value (control control-data)
  (let ((k-control-type (cd-k-type control-data)))
    (case k-control-type
      ((:text :label :combo)
       (gettext control))
      (:button
       (button.getSelection control))
      (:list
       ;; an array of indices
              ;(apply #'list.getselectionindices control)
       (error "not implemented: get-basic-control-value for list control"))
      (t (error "not a basic control type: ~s" k-control-type)))))

(defun get-basic-control-value (control control-data)
  (let ((k-control-type (cd-k-type control-data)))
    (case k-control-type
      ((:text :label :combo)
       (gettext control))
      (:button
       (button.getSelection control))
      (:list
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
                  result))))
      (t (error "not a basic control type: ~s" k-control-type)))))


(defun restore-basic-control-value (control-data)
  (let (;(control (cd-control control-data))
        ;(k-control-type (cd-k-type control-data))
        (old-control-value (cd-old-control-value control-data)))
    (set-basic-control-value control-data old-control-value)))



;;; value : control value
(defun set-basic-control-value (control-data value)
  (let ((control (cd-control control-data))
        (k-control-type (cd-k-type control-data)))
    (setf (cd-old-control-value control-data) value)
    (case k-control-type
      ((:text :label :combo)
       (settext control value))
      (:button
       ;; only useful for state buttons
       (button.setSelection control value))
      (:list
       (unless (or (not value)
                   (if (listp value)
                       (integerp (first value))
                     (integerp value)))
         ;; pourrait aussi convertir en utilisant list.setitems
         #+never
         (error "set-basic-control-value 
 In a list control, value should be NIL or a list of integers.
 Value: ~s" value))
       (cond
        ((not value)
         ;;?
         ;;(list.setselection control -1)
         (list.setselection control
                            (make-j-vector nil :type :int)))
        (T (setf value (if (listp value) value (list value)))
           (let ((string? (stringp (first value))))
             ;;(cd-single? control-data)
             (cond (string?
                    (list.setselection control
                                       (make-j-vector value)))
                   (t (list.setselection control
                                         (make-j-vector value :type :int))))))))
      (t (error "not a basic control type: ~s" k-control-type)))))

(defun basic-control-value-equal (k-control-type old new)
  (case k-control-type
    ((:text :label :combo)
     (string= old new))
    (:button (eq old new))
    (:list (equalp old new))
    (t (error "not a basic control type: ~s" k-control-type))))
  