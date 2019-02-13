(in-package :dialogue)

;;; to see the dummy widgets
(defun make-dummy-widget (parent)
  (let ((wi (make-new label. parent *swt.none*)))
    (label.settext wi " ")
    (label.setbackground wi (system-color :red))
    ;(label.setenabled wi nil)
    ;(label.setvisible wi nil))
    ))

;;; ****************************************************************************
;;; :horizontalSpan

(def-SWT-test
  (dialogue
   `(:grid   ; :makecolumnsequalwidth t
     (
      ;; 1st ligne
      ((:type :label :initial-value "first")
       (:type :label :initial-value "second")
       (:type :label :initial-value "3 3 3 3 3 3 3 3 3 3 3 3 3 3 "))
      ))
   :options `(:setbackground ,(system-color :yellow) ) ))


(def-SWT-test
  (dialogue
   `(:grid   ; :makecolumnsequalwidth t
     (
      ;; 1st ligne
      ((:type :label :initial-value "first")
       (:type :label :initial-value "second")
       (:type :label :initial-value "3 3 3 3 3 3 3 3 3 3 3 3 3 3 "))
      ;; 2nd line
      ((:type :label :initial-value "first"
        :layout (:horizontalSpan 2
                 :horizontalalignment ,*swt.fill*
                 ;; :grabexcesshorizontalspace t
                 ))
       (:type :label :initial-value "3")
       )
      ;; 3rd line
      (
       (:type :label :initial-value "first label"
        :layout-data (:horizontalSpan :ALL :horizontalalignment ,*swt.fill*)
        )
       )))
   :options `(:setbackground ,(system-color :yellow) ) ))

(def-SWT-test
  (dialogue
   `(:grid  ; :makecolumnsequalwidth t
     (
      ;; 1st ligne
      ((:type :label :initial-value "1")
       (:type :label :initial-value "second")
       (:type :label :initial-value "3 3 3 3 3 3 3 3 3 3 3 3 3 3 "))
      ;; 2nd line
      ((:type :label :initial-value "first")
       (:type :label :initial-value "second"
        :layout-data (:horizontalSpan 2
                      :horizontalalignment ,*swt.fill*
                      ;; :grabexcesshorizontalspace t
                      ))
       )
      ;; 3ird line
      ((:type :label :initial-value "first label"
        :layout (:horizontalSpan :ALL :horizontalalignment ,*swt.fill*)
        )
       )))
   :options `(:setbackground ,(system-color :yellow) ) ))


;;; ****************************************************************************
;;; :verticalSpan

(def-SWT-test
  (dialogue
   `(:grid
     (
      ;; 1st ligne
      ((:type :label :initial-value "1")
       (:type :label :initial-value "second")
       (:type :label :initial-value "3 3 3 3 3 3 3 3 3 3 3 3 3 3 "))
      ;; 2nd line
      ((:type :label :initial-value "first")
       (:type :label :initial-value "second"
        :layout (:verticalSpan 2
                 :verticalalignment ,*swt.fill*
                 :horizontalalignment ,*swt.fill*))
       )
      ;; 3rd line
      ((:type :label :initial-value "first label")
       (:type :label :initial-value "3")
       )))
   :options `(:setbackground ,(system-color :yellow))))

;;; ****************************************************************************
;;; - :horizontalspan and :verticalSpan

;;; a :group control that takes 4 cells
(def-SWT-test
  (dialogue
   `(:grid
     (
      ;; 1st line
      ((:type :label :initial-value "control 1")
       (:type :label :initial-value "c 2")
       (:type :label :initial-value "c 3")
       (:type :label :initial-value "control 4")
       )
      ((:type :label :initial-value "first")
       (:group :text "Group"
        :layout (:verticalSpan 2
                 :horizontalspan 2
                 :verticalalignment ,*swt.fill*
                 :horizontalalignment ,*swt.fill*)
        (:column
         ((:type :button :style :radio
           :methods (:settext "Full"))
          (:type :button :style :radio
           :methods (:settext "Partial")))))
       (:type :label :initial-value "4"))
      ;; 3rd 
      ((:type :label :initial-value "1")
       )
      ((:type :label :initial-value "1st control in 4th row "
        :layout (:horizontalspan :all
                 :verticalalignment ,*swt.fill*
                 :horizontalalignment ,*swt.fill*)))
      (,(make-exit-controls-composite-spec)
       )
      ))
   :exit-controls nil
   :options `(:setbackground ,(system-color :yellow))))



|#
If the SWT :numColumns layout option is not used, Dialogue counts the number of columns with
  (apply #'max (mapcar #'length grid-rows))
This works iff there is at least one row that has a number of control specifications
equal to the number of columns.

In the following 4 columns grid example, it is necessary:
- either to state de number of columns (:numColumns 4)
- either to explicitely add an dummy widget in the first line (nil)
- either using :span tags in the 3rd row for example.

Remarks:
0. notice :verticalSpan 2 in the 2nd widget of the 2nd line.
1. :span tags are ignored except in the counting of columns.
2. use the following defun of make-dummy-widget to see the dummy widgets:
(defun make-dummy-widget (parent)
  (let ((wi (make-new label. parent *swt.none*)))
    (label.settext wi " ")
    (label.setbackground wi (system-color :red))))
|#
(def-SWT-test
  (dialogue
   `(:grid ; :numColumns 4
     (
      ;; 1st line
      ((:type :label :initial-value "1")
       (:type :label :initial-value "second")
       (:type :label :initial-value "3 3 3 3 3 3 3 3 3 3 3 3 3 3 ")
       ;; nil
       )
      ;; 2nd line
      ((:type :label :initial-value "first")
       (:type :label :initial-value "second"
        :layout (:verticalSpan 2
                 :horizontalspan 2
                 :verticalalignment ,*swt.fill*
                 :horizontalalignment ,*swt.fill*))
       )
      ;; 3rd line
      ((:type :label :initial-value "first label")
       :span
       :span
       (:type :label :initial-value "X")
       )))
   :options `(:setbackground ,(system-color :yellow))))


;;; ****************************************************************************
;;; BUG in :exit-controls (layout problem)

;;BUG EXIT-CONTROLS !!
(def-SWT-test
  (dialogue
   `(:grid
     (
      ((:type :label :initial-value "first in first line")
       (:group :text "Group"
        :layout (:verticalSpan 2)
        (:column ())))
      ((:type :label :initial-value "first in 2nd line")
       :span
       )
      ))
   :options `(:setbackground ,(system-color :yellow))))

;; WORKAROUND !
(def-SWT-test
  (dialogue
   `(:grid
     (
      ((:type :label :initial-value "first in first line")
       (:group :text "Group"
        :layout (:verticalSpan 2)
        (:column ())))
      ((:type :label :initial-value "first in 2nd line")
       :span
       )
      (,(make-exit-controls-composite-spec)
       )
      ))
   :exit-controls nil  ; !!!
   :options `(:setbackground ,(system-color :yellow))))


;;; ****************************************************************************
;;;

(def-SWT-test
  (dialogue
   `(:grid
     (
      ;; 1st ligne
      ((:type :label :initial-value "1")
       (:type :label :initial-value "second")
       (:type :label :initial-value "3 3 3 3 3 3 3 3 3 3 3 3 3 3 ")
       ;; DUMMY LABEL:
       NIL)
      ;; 2nd line
      ((:type :label :initial-value "first")
       (:group :text "Group"
        :layout (:verticalSpan 2
                 :horizontalspan 2
                 :verticalalignment ,*swt.fill*
                 :horizontalalignment ,*swt.fill*)
        (:column
         ((:type :button :style :radio
           :methods (:settext "Full"))
          (:type :button :style :radio
           :methods (:settext "Partial")))))
       )
      ;; 3rd line
      ((:type :label :initial-value "first label")
       :span
       :span
       (:type :label :initial-value "X")
       )
      ;; 4 
      ((:type :label :initial-value "first label")
       nil
       (:type :label :initial-value "X")
       )
      ))
   :exit-controls nil
   :options `(:setbackground ,(system-color :yellow))))


;;; ****************************************************************************
;;; misc

(def-SWT-test
  (dialogue
   `(:grid
     ;:makecolumnsequalwidth t
     :horizontalSpacing 22
     :marginHeight 44
     :verticalSpacing 55
     (
      ;; 1ère ligne
      ((:type :label :initial-value "Name" :layout (:width 444 :height 44))
       (:type :label :initial-value "Email")
       (:type :label :initial-value "Email")
       (:type :label :initial-value "Email"))
      ;; 2
      (nil (:type :label :initial-value "Em"))
      ;; 3
      ((:type :label :initial-value "Em 333"
        :layout-data (:horizontalSpan 3
                      :width 250
                      :horizontalAlignment ,*swt.center*)))
      ;; 4
      ((:type :label :initial-value "Name")
       (:type :label :initial-value "Email")
       )))
   :options `(:setbackground ,(system-color :yellow)
              ;:maximized t
              )
   ))

(def-SWT-test
  (flet ((hide-salary?(employee)
           (member (name employee) '("Noé" "Anne") :test #'string-equal)))
    (dialogue
     `(:grid
       :horizontalSpacing 15
       :verticalSpacing 10
       ,(cons
         '((:type :label :initial-value "Name")
           (:type :label :initial-value "Salary")
           (:type :label :initial-value "Email")
           (:type :label :initial-value "FT")
           (:type :label :initial-value "Departement"))
         (mapcar (lambda (employee)
                   `((:accessor name :data ,employee
                      :layout-data (:horizontalalignment ,*swt.fill*))
                     ,(if (hide-salary? employee)
                          ()
                        `(:accessor salary :data ,employee
                          :layout-data (:horizontalalignment ,*swt.fill*)))
                     (:accessor email :data ,employee
                      :layout-data (:horizontalalignment ,*swt.fill*))
                     (:accessor full-time? :data ,employee
                      :type :button :style :check)
                     (:accessor department :data ,employee
                      :type :combo :style :read_only
                      :range ,*departments*)))
                 *employees*)))
     :main t
     :title "Grid")))

