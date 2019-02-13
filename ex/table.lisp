(in-package :dialogue)

;;;; About :
;;;; - Table control
;;;; - :column layout

(def-SWT-test
  (let ((fill? nil)
        ;; useless if fill? is T
        (button-width 200))
    (dialogue
     `(:column :fill ,fill?
       :spacing 6
       :margintop 10 :marginheight 5  :marginwidth 15
       ((:type :label
         :initial-value
         "Try double click, right click and shift right click on a table item (line)")
        ;; a dummy control (to make room)
        nil
        (:type table.
         :name :table
         :style ,(logior *swt.multi* *swt.border* *swt.full_selection*))
        (:type :button
         :initial-value "Remove first line"
         :style :push
         :layout-data (:width ,button-width)
         :event (:selection
                 ,(lambda (event control-data)
                    (declare (ignore event))
                    (let ((table (find-control :table control-data)))
                      (if (> (table.itemcount table) 0)
                          (table.remove table 0)
                        (swttools:ok-di "No more items"))))))
        (:type :button
         :initial-value "Select first and third lines"
         :style :push
         :layout-data (:width ,button-width)
         :event (:selection
                 ,(lambda (event control-data)
                    (declare (ignore event))
                    (let ((table (find-control :table control-data)))
                      (table.setselection table (make-j-vector (list 0 2)))))))
        (:type :button
         :initial-value "Get selection indices"
         :style :push
         :layout-data (:width ,button-width)
         :event (:selection
                 ,(lambda (event control-data)
                    (declare (ignore event))
                    (let ((table (find-control :table control-data)))
                      (user::ok-di
                       (format nil "~a"
                               (swttools::j-vector-to-list
                                (table.getselectionindices table))))))))))
     :creation-init-function 
     (lambda (shell) (table-test-di-creation-init-function shell))
     :main t
     :name :test-di
     :title "Table control and column layout")))

(defun table-test-di-creation-init-function (shell)
  (let* ((table (find-control :table :test-di))
         (col-number 5)
         (rows-number 5)
         (columns
          (loop for c from 0 below col-number
                as col = (tablecolumn.new table *swt.none*)
                collect col
                do
                (tablecolumn.settext col (format nil "~d" c))
                ;; useless if tablecolumn.pack is applied (see below)
                (tablecolumn.setwidth col 150) )))
    (table.setlinesvisible table t)
    (table.setheadervisible table t)
    (loop with i = -1
          for r from 0 below rows-number
          do
          (let ((ti (tableitem.new table *swt.none*))) ; tableitem = row
            (dotimes (c col-number)
              (tableitem.settext ti c (format nil "r:~d  c:~d  ~d" r c (incf i)))
              (when (= r c)
                (tableitem.setbackground ti c (system-color :yellow))))))
    (dolist (column columns) (tablecolumn.pack column))
    ;; useless because the layout  
    ;; (table.setsize table 1200 200)  (table.pack table)
    ;; -
    (install-table-test-di-listeners table shell)))


(defun install-table-test-di-listeners (table shell)
  (declare (ignore shell))
  (table.addlistener
   table
   *swt.mousedoubleclick*
   (new-proxy
    p +MARSHALL-ID+ 0
    (listener.
     (handleevent
      (event)
      (let* ((x (event.x event))
             (y (event.y event))
             (point (point.new x y)))
        (if-bind (item (table.getitem table point))
                 (progn
                   (multiple-value-bind (ri ci)
                       (get-current-cell table x y)
                     (swttools:ok-di
                      (format nil "Row: ~d
Cell: ~d (row),~d (column)"
                              (table.indexof table item) ; same as ri
                              ri
                              ci))))
                 ;; if the user clicks outside the real table (in the space for scrollbars)...
                 (swttools:ok-di
                  (format nil "No table item at : ~d,~d"(event.x event)(event.y event))))
        )))))

  (table.addlistener
   table
   *swt.mousedown*
   (new-proxy
    p +MARSHALL-ID+ 0
    (listener.
     (handleevent
      (event)
      (when (right-button? event)
        (user::ok-di
         (format nil "Selection indices: ~a"
                 (user::j-vector-to-list (table.getselectionindices table)))))))))

  ;;! truc , devrait pouvoir inhiber la selection autrement !
  (table.addlistener
   table
   *swt.selection*
   (new-proxy
    p +MARSHALL-ID+ 0
    (listener.
     (handleevent
      (event)event
      ;; would unselect
      ;; (table.setselection table (make-new-vector :int 0))
      )))))

;;; returns the row and columm indices
(defun get-current-cell (table x y &key
                               (rows-number (table.getitemcount table))
                               (columns-number (table.getcolumncount table)))
  (loop for ri from 0 below rows-number
        as item = (table.getitem table ri)
        do
        (loop for ci from 0 below columns-number
              as cell-bounds = (tableitem.getbounds item ci)
              as cell-bounds-x = (rectangle.x cell-bounds)
              do
              (cond ((< x cell-bounds-x)
                     (return nil))
                    ((rectangle.contains cell-bounds x y)
                     (return-from get-current-cell (values ri ci)))
                    (t nil))))
  ;; no current cell:
  ;; there could be some space oustside the real table for the vertical scrollbar which
  ;; is include in what table.getclientarea returns...
  nil)

              
        