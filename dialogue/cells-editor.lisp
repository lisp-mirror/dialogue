;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)


(defvar *CED-info* nil)

(defun CED-settext (table-item column-i text)
  (tableitem.settext table-item
                     ;; 1+ because the first zero-sized column
                     (1+ column-i)
                     text))

(defun CED-gettext (table-item column-i)
  ;; (tableitem.gettext (table.getitem table ri) (1+ column-i))
  (tableitem.gettext table-item
                     ;; 1+ because the first zero-sized column
                     (1+ column-i)))

(defun CED-setimage (table-item column-i image)
  (tableitem.setimage table-item
                     ;; 1+ because the first zero-sized column
                     (1+ column-i)
                     image))

;;; Set text of cell CI in table-item if New-Content is different from the current content
;;; Pack columm if needed.
(defun CED-settext-and-pack (new-content cells-editor-name ci table-item)
  (let ((cell-content (CED-gettext table-item ci)))
    (unless (string= new-content cell-content)
      (CED-settext table-item ci new-content)
      (when (> (textextent new-content)
               ;; instead use (textextent cell-content) - will always work but to often
               (- (tablecolumn.width (CED-getcolumn cells-editor-name ci))
                  ;;? on my PC, 12 is the difference between the width of a columm that
                  ;; contains "visited" and (textextent "Visited")
                  12))
        (CED-pack cells-editor-name ci)
        T))))

#|
- (aref array ri ci)  - RowIndice ColumnIndice 
- empty cell : the value of a slot in the array can be :empty-cell
  Dans ce cas, la cell apparaît avec un background gris et tout est pris en charge
  par le cells-editor (intialisation du text, on-cell-mouse-down,...)
  See on-empty-cell-mouse-down
REMARKS
- the first column in the table is zero-sized (not visible and not used): a trick to
  allow the alignement to be changed in the first (true) column.
  See CED-settext function definition.
  It allows also to disable the row selection in a table: by default, to
  select a row, the user has to click in the first columm cell.
|#
(defun cells-editor
       (array
        &key
        ;; dialogue arguments
        ;; ~~~
        (name :cells-editor)
        (title (string-capitalize name))
        ;;
        ;; a function to set each cell content
        (set-cell-fn
         (lambda (table array ri ci table-item) ; RowIndice ColumnIndice
           (declare (ignore table))
           (CED-settext table-item ci (format nil "~a" (aref array ri ci)))))
        (on-cell-mouse-down
         ;; cell-indices: (RowIndice ColumnIndice)
         (lambda (event table array cell-indices table-item)
           (declare (ignore event table))
           (let ((ri (first cell-indices))
                 (ci (second cell-indices)))
             (ok-di (format nil
                            "row indice: ~d~%column indice: ~d~%array value: ~a~%table value: ~a"
                            ri ci (aref array ri ci) (CED-gettext table-item ci)))
             )))
        (on-empty-cell-mouse-down
         (lambda (event table array cell-indices table-item)
           (declare (ignore event table array cell-indices table-item))
           (beep)))
        ;; a keyword (:right, :center :left) or a swt constant (*swt.right*, ...)
        ;; Or a list of keyword and swt constants. One item per column (the last one
        ;; being the default for the remaining columns
        (alignement :right)
        (show-column-headers? t)
        ;; a list of strings. A header defaults to the row position (1 based).
        row-headers
        ;; a list of strings. A header defaults to the column position (1 based).
        column-headers
        ;; ~~~
        multi-view-combo-range
        (multi-view-combo-initial-value (first multi-view-combo-range))
        ;; 2 args: event control-data
        multi-view-combo-selection-fn
        ;; ~~
        ;; To add controls below the table in the grid layout: a list of grid-row
        ;; specifications (see Dialogue documentation)
        grid-rows
        ;; default layout data of the main table (grid layout - there is a onother table
        ;; just for the row headers)
        (table-layout-data `(:horizontalalignment ,*swt.fill* :grabexcesshorizontalspace t
                             :horizontalspan :all
                             ))
        ;; ~~~
        (grid-horizontalSpacing 4)
        (grid-verticalSpacing 8)
        )
  (ensure-boolean-value show-column-headers?)
  (let ((textextent-arg (if row-headers
                            row-headers
                          (format nil "~d" (first (array-dimensions array))))))
    (multiple-value-bind (row-headers-table-width ignore plist)
        (textextent textextent-arg :border-width t :scrollbar-width t)
      ;; table width (without the scrollbar area width wich is sometimes included in
      ;; the table ClientArea - on some Windows XP only, apparently)
      (incf row-headers-table-width
            (+ (* (getf plist :border-width) 2)  ; border-width: 2 pixels on my PC
               ;; Correction needed, using scrollbar-width (20 pixels on my PC) instead
               ;; of a constant to s'adapter à la résolution de l'écran, etc.
               ;; To compute the table width, this should be more accurate:
               ;;   (+ (- ClientArea-width scrollbar-width) (* 2 border-width))  
               (- (round (getf plist :scrollbar-width) 2))))
      (dialogue
       `(:grid
         :horizontalSpacing ,grid-horizontalSpacing
         :verticalSpacing ,grid-verticalSpacing
         ,(nconc
           (when multi-view-combo-range
             `((nil
                (:type :label :initial-value "View")
                (:accessor nil
                 :name :previous-search
                 :type :combo
                 :range ,multi-view-combo-range
                 :initial-value ,multi-view-combo-initial-value
                 :style (:drop_down :read_only)
                 :event (:selection
                         ,(lambda (event control-data)
                            (progn
                              (funcall multi-view-combo-selection-fn event control-data)
                              (let ((CED-data (getf *ced-info* name)))
                                (setf (CED-current-view CED-data)
                                      (get-control-value control-data))
                                (CED-fill-table name)
                                nil))))))))
           ;; ligne des 2 tables (row headers table and data table)
           `(((:type table.
               :name :row-headers
               :layout-data (:width ,row-headers-table-width))
              ;; la 1ère colonne étant cachée (zero-width), on ne peut sélectionner un ligne
              ;; si le style n'est pas *swt.full_selection*
              (:type table.
               :name :table
               :layout-data ,table-layout-data
               )))
           grid-rows))     
       :creation-init-function 
       (lambda (shell) (CED-creation-init-function shell
                                                   name
                                                   array
                                                   set-cell-fn
                                                   on-cell-mouse-down
                                                   on-empty-cell-mouse-down
                                                   show-column-headers?
                                                   row-headers
                                                   column-headers
                                                   multi-view-combo-range
                                                   multi-view-combo-initial-value
                                                   alignement))
       :exit-controls nil
       :name name
       :title title
       :close-function
       (lambda (shell event)
         (declare (ignore shell event))
         (remf *ced-info* name)
         T)
       ))))

(defun empty-cell? (array ri ci)
  (eq :empty-cell (aref array ri ci)))

(defclass CED-data ()
  ((CED-name :accessor CED-name)
   (CED-table :accessor CED-table)
   (CED-set-cell-fn :accessor CED-set-cell-fn)
   ;; the true columns - frefs (does not include the first zero-sized column)
   (CED-columns :accessor CED-columns)
   (CED-columns-number :accessor CED-columns-number)
   (CED-rows-number :accessor CED-rows-number)
   (CED-tableitems :accessor CED-tableitems)
   (CED-array :accessor CED-array)
   ;; the current cache (corresponding to the current-view)
   ;; To cache position (x and y) and size (w and h) of each cell (tableitem.getbounds)
   (CED-table-cache :accessor CED-table-cache)
   ;; one cache per view
   (CED-table-caches :accessor CED-table-caches)
   ;; names of each view
   (CED-multi-view-combo-range :accessor CED-multi-view-combo-range :initform nil)
   ;; name of current-view
   (CED-current-view :accessor CED-current-view :initform nil)
   ;; tableitems in the Row-Header auxiliary table
   (CED-RH-tableitems :accessor CED-RH-tableitems)
   ;; a list of cells coordinates (row-i col-i)
   (CED-selected-cells :accessor CED-selected-cells :initform nil)   
   ))

 
(defun CED-creation-init-function (shell
                                   shell-name
                                   array
                                   set-cell-fn
                                   on-cell-mouse-down
                                   on-empty-cell-mouse-down
                                   show-column-headers?
                                   row-headers column-headers
                                   multi-view-combo-range
                                   multi-view-combo-initial-value
                                   alignement)
  shell
  (let* ((table (find-control :table shell-name))
         (dims (array-dimensions array))
         (rows-number (first dims))
         (columns-number (second dims))
         (dum-column (tablecolumn.new table *swt.none*))
         (default-alignement (if (listp alignement) (first alignement) alignement))
         (columns
          (loop for ci from 0 below columns-number
                as current-alignement = (if (and alignement (listp alignement))
                                            (setf default-alignement (pop alignement))
                                          default-alignement)
                as c-header = (if-bind (ch (pop column-headers))
                                       ch
                                       (format nil "~d" (1+ ci)))
                as col = (tablecolumn.new table *swt.none*)
                collect col
                do
                (tablecolumn.setalignment col (case current-alignement
                                                (:right *swt.right*)
                                                (:left *swt.left*)
                                                (:center *swt.center*)
                                                (t current-alignement)))
                (tablecolumn.settext col c-header) ))
         (CED-data (make-instance 'CED-data)))
    dum-column
    (setf (getf *CED-info* shell-name) CED-data)
    (CED-set-row-headers shell-name CED-data table row-headers rows-number show-column-headers?)
    ;; les cells seront mise en blanc individuellement
    ;; au final ça permet de masquer (griser) l'espace reservé au scrollbars
    (table.setbackground table (system-color :background-gray))
    (table.setlinesvisible table t)
    (table.setheadervisible table show-column-headers?)
    (with-slots (CED-array CED-name
                 CED-table CED-table-caches
                 CED-multi-view-combo-range CED-current-view
                 CED-set-cell-fn CED-columns
                 CED-rows-number CED-columns-number CED-tableitems)
        CED-data
      (setf CED-name shell-name
            CED-table table
            CED-array array
            CED-set-cell-fn set-cell-fn
            CED-columns columns
            CED-rows-number rows-number
            CED-columns-number columns-number
            CED-multi-view-combo-range multi-view-combo-range
            CED-current-view multi-view-combo-initial-value
            ;; CED-table-cache is initialized in CED-fill-table
            CED-table-caches
            (loop for i from 0 below (if multi-view-combo-range
                                         (length multi-view-combo-range) 1)
                  collect (make-array (list rows-number columns-number) :initial-element nil))
            CED-tableitems
            (loop for ri from 0 below rows-number
                  as ti = (tableitem.new table *swt.none*) ;tableitem = row
                  collect ti                  
                  do
                  (tableitem.setbackground ti (system-color :white)))))
    (CED-fill-table shell-name)
    (CED-install-listeners shell-name table columns array
                           on-cell-mouse-down on-empty-cell-mouse-down)
    ))

;; (setf *ced-info* nil)
(defun CED-set-array (shell-name array)
  (setf (CED-array (getf *ced-info* shell-name))
        array))

;;; fill the table control using the current array and the defined set-cell-fn function.
;;; Set the current cache (the one correspoding to the current view)
(defun CED-fill-table (shell-name)
  (let* ((CED-data (getf *ced-info* shell-name))
         (table (CED-table CED-data))
         (array (CED-array CED-data))
         (rows-number (CED-rows-number CED-data))
         (columns-number (CED-columns-number CED-data))
         (columns (CED-columns CED-data))
         (set-cell-fn (CED-set-cell-fn CED-data)))
    ;; .3 sec (2400 cells)
    (loop for ri from 0 below rows-number
          as tableitem in (CED-tableitems CED-data)  ; a row
          do
          (dotimes (ci columns-number)
            (if (empty-cell? array ri ci)
                (progn
                  ;;(tableitem.settext tableitem ci "")
                  (tableitem.setbackground tableitem (1+ ci) (system-color
                                                              :dark_gray
                                                              )))
              (funcall set-cell-fn table array ri ci tableitem))
            ;; (when (= ri ci) (tableitem.setbackground tableitem ci (system-color :yellow)))
            ))
    ;; .07 sec (2400 cells)
    (without-redraw table
      (dolist (column columns)
        ;; compute columm width
        (tablecolumn.pack column)))    
    ;; set the current cache once the columns have been packed
    (setf (CED-table-cache CED-data)
          (let ((range (CED-multi-view-combo-range CED-data)))
            (if range
                (nth (position (CED-current-view CED-data)
                               (CED-multi-view-combo-range CED-data)
                               :test #'string=)
                     (CED-table-caches CED-data))
              (first (CED-table-caches CED-data)))))
    ;; useless because the layout  
    ;; (table.setsize table 1200 200)(table.pack table)
    ))

;;; pack the columns and reset the cache starting at columm CI
(defun CED-pack (shell-name ci)
  (let* ((CED-data (getf *ced-info* shell-name))
         (columns (CED-columns CED-data))
         (columns-number (CED-columns-number CED-data))
         (rows-number (CED-rows-number CED-data))
         (cache (CED-table-cache CED-data)))
    (tablecolumn.pack (nth ci columns))
    (loop for i from 0 below rows-number do
          (loop for j from ci below columns-number do
                (setf (aref cache i j) nil)))))

(defun CED-getcolumn (shell-name ci)
  (let* ((CED-data (getf *ced-info* shell-name))
         (columns (CED-columns CED-data)))
    (nth ci columns)))

(defun CED-set-row-headers (shell-name CED-data main-table row-headers rows-number
                                       show-column-headers?)
  main-table
  (let* ((table (find-control :row-headers shell-name))
         ;; 1ère colonne vide (truc car tablecolumn.setalignment ne marche pas pour la
         ;; 1ère colonne...)
         (dum-column (tablecolumn.new table *swt.none*))
         ;; on peut se passer de créer une colonne comme il n'y en a qu'une !
         ;; mais ça permet de fait tablecolumn.setalignment
         (column (tablecolumn.new table *swt.none*))
         ;; ? pas moyen de modifier la hauteur d'une ligne (tableitem) !!
         ;;(CED-data (getf *ced-info* shell-name))
         ;;(first-main-table-column (first (CED-columns CED-data)))
         )
    (declare (ignore dum-column))
    (tablecolumn.setalignment column *swt.right*)
    (table.setbackground table (system-color :background-gray))
    (table.setlinesvisible table t)
    ;; les 2 column headers de cette table sont inivisible (défault)
    ;; On crée une 1ère ligne vide (cell) au niveau des headers de la table principale
    ;; (c'est plus propre que de faire (table.setheadervisible table t)
    ;; uniquement si les headers de la table principale sont visibles
    (when show-column-headers?
      (let ((empty-ti (tableitem.new table *swt.none*)))
        empty-ti
        ;;(tableitem.setbackground empty-ti (system-color :cyan))
        ))
    (setf (CED-RH-tableitems CED-data)
          (loop for ri from 0 below rows-number
                as ti = (tableitem.new table *swt.none*)
                as r-header = (if-bind (rh (pop row-headers))
                                       rh
                                       (format nil "~d" (1+ ri)))
                collect ti
                do
                ;;(tableitem.setbackground ti 0 (system-color :yellow))
                ;; (tableitem.setbackground ti 1 (system-color :red))
                ;; remplissage de la vraie column: 1
                (tableitem.settext ti 1 r-header)
                ))
    ;; don't pack dum-column (or it would not be zero-sized)
    (tablecolumn.pack column)
    ))

(defun CED-install-row-headers-listener (shell-name table)
  (let*  ((CED-data (getf *ced-info* shell-name))
          (rows-number (CED-rows-number CED-data))
          (row-headers-table (find-control :row-headers shell-name))
          )
    rows-number table
    (table.addlistener
     row-headers-table
     *swt.mousedown*
     (new-proxy
      p +MARSHALL-ID+ 0
      (listener.
       (handleevent
        (event)event
        (CED-on-row-mouse-down-fn event CED-data)
        ))))
    ))

(defun CED-on-row-mouse-down-fn (event CED-data)
  (with-error- (:CED-on-row-mouse-down-fn)
    (block main
      (let ((x (event.x event))
            (y (event.y event))
            (rows-number (CED-ROWS-NUMBER CED-data))
            ;; (table-cache   )
            )     
        (loop ;; dans le widget, une seule colonne à considérer (la 2eme - la 1ère est vide)
              with table-ci = 1 
              for ri from 0 below rows-number
              as item in (CED-RH-tableitems CED-data)
              do
              (ignore-errors+
                  (format t "~% error in xxx: ~s ~s ~s ~s" ri item x y)
                (let (rect rx ry rw rh)
                  ;;as cached-item = (svref table-cache ri)
                  (cond #+never
                        (cached-item
                         (setf rx (first cached-item)
                               ry (second cached-item)
                               rw (third cached-item)
                               rh (fourth cached-item)))
                        (t
                         (setf rect (tableitem.getbounds item table-ci))
                         (setf
                          rx (rectangle.x rect)
                          ry (rectangle.y rect)
                          rw (rectangle.width rect)
                          rh (rectangle.height rect))
                         #+never
                         (setf (svref table-cache ri)
                               (list rx ry rw rh))
                         ))
                  (cond
                   ((< x rx) (return nil))
                   ;; with no caching, would have to do (rectangle.contains rect x y)
                   ((inside-rectangle? rx ry rw rh x y)
                    (update-selected-cells (CED-name CED-data)
                                           :event event
                                           :row-i ri)
                    (return-from main (values ri item)))
                   (t nil)))))
        ))))


(defun CED-install-listeners (shell-name table columns
                                         array on-cell-mouse-down on-empty-cell-mouse-down)
  (let* ((dims (array-dimensions array))
         (rows-number (first dims))
         (columns-number (second dims)))
    (CED-install-row-headers-listener shell-name table)
    ;; truc pour déselectionner (ne veut pas voir le highlight bleu...)
    ;; devenu inutile dès lors où la 1ère colonne est cachée (la sélection se faisant via
    ;; la 1ère colonne)
    #+never
    (table.addlistener
     table
     *swt.selection*
     (new-proxy
      p +MARSHALL-ID+ 0
      (listener.
       (handleevent
        (event)event
        ;; unselect , les 2 sont équivalents
        ;; (table.setselection table (make-new-vector :int 0))
        (table.deselectall table)))))
    
    (let ((column-i -1))
      (dolist (column columns)
        ;; needed for the closure
        (install-columm-listener shell-name column (incf column-i))))
  
    (table.addlistener
     table
     *swt.mousedown*
     (new-proxy
      p +MARSHALL-ID+ 0
      (listener.
       (handleevent
        (event)
        (multiple-value-bind (cell-indices table-item)
            (get-current-cell shell-name
                              table
                              (event.x event)
                              (event.y event)
                              :columns-number columns-number
                              :rows-number rows-number)
          (cond (cell-indices
                 (if (empty-cell? array (first cell-indices) (second cell-indices))
                     (funcall on-empty-cell-mouse-down event table array cell-indices table-item)
                   (progn
                     (update-selected-cells shell-name :cell-indices cell-indices :event event)
                     (funcall on-cell-mouse-down event table array cell-indices table-item))))
                (t (beep) ; in the scrollbar area...
                   )))))))

    (let ((current-cell-indices nil)
          (current-table-item nil)
          (empty-cell? nil))
      (table.addlistener
       table
       *swt.mousemove*
       (new-proxy
        p +MARSHALL-ID+ 0
        (listener.
         (handleevent
          (event)
          (let ((previous-cell-indices current-cell-indices)
                (previous-table-item current-table-item)
                (previous-cell-was-empty? empty-cell?))
            (multiple-value-setq (current-cell-indices current-table-item)
                (get-current-cell shell-name
                                  table
                                  (event.x event)
                                  (event.y event)
                                  :columns-number columns-number
                                  :rows-number rows-number))
            (let* ((ri (first current-cell-indices))
                   (ci (second current-cell-indices)))
              (when ri
                (setf empty-cell? (empty-cell? array ri ci)))
              (cond ((and previous-cell-indices
                          (equal previous-cell-indices current-cell-indices))
                     nil)
                    (t (when (and previous-cell-indices (not previous-cell-was-empty?))
                         (set-cell-background-color shell-name
                                                    :cell-indices previous-cell-indices
                                                    :table-item previous-table-item))
                 
                       (when (and current-cell-indices (not empty-cell?))
                         (set-cell-background-color shell-name
                                                    :current-cell? t
                                                    :cell-indices current-cell-indices
                                                    :table-item current-table-item)))))))))))

           
    ))

(defun install-columm-listener (shell-name column column-i)
  (tablecolumn.addlistener
   column
   *swt.selection*
   (new-proxy
    p +MARSHALL-ID+ 0
    (listener.
     (handleevent
      (event)
      (update-selected-cells shell-name
                             :event event
                             :column column
                             :column-i column-i))))))

(defun cells= (cell-indices1 cell-indices2)
  (and (= (first cell-indices1) (first cell-indices2))
       (= (second cell-indices1) (second cell-indices2))))

(defun set-cell-background-color (shell-name &key
                                             row-i
                                             col-i
                                             cell-indices
                                             table-item
                                             current-cell?
                                             )
  (let (CED-data)
    (unless row-i
      (setf row-i (first cell-indices) col-i (second cell-indices)))
    (unless table-item
      (setf CED-data (getf *ced-info* shell-name))
      (setf table-item (nth row-i (CED-tableitems CED-data))))          
    (tableitem.setbackground table-item
                             (1+ col-i)
                             (cond (current-cell?
                                    (system-color :yellow))
                                   ((selected-cell? shell-name row-i col-i)
                                    (system-color :cyan))
                                   (t (system-color :white))))))

(defun selected-cell? (shell-name row-i col-i)
  (let*  ((CED-data (getf *ced-info* shell-name))
          (selected-cells (CED-selected-cells CED-data))
          (cell-indices (load-time-value (list 0 0))))
    (setf (first cell-indices) row-i (second cell-indices) col-i)
    (member cell-indices selected-cells :test #'cells=)))

(defun update-selected-cells (shell-name &key
                                         cell-indices
                                         event
                                         column-i
                                         row-i
                                         column
                                         ;;(shift-down? nil shift-down?-p)
                                         ;;(control-down? nil control-down?-p)
                                         )
  column
  (let* ((CED-data (getf *ced-info* shell-name))
         (array (CED-array CED-data))
         (selected-cells (CED-selected-cells CED-data))
         (down? ;;(or (and shift-down?-p shift-down?) (and control-down?-p control-down?)
                (and event (let ((statemask (event.statemask event)))
                             (modifier-key-down? statemask)
                             ;;(or (shift-down? statemask) (control-down? statemask))
                             )))
         (whole-line? (or row-i column-i)))
    (flet (;; update a cell selection state - si pas dans cas d'une sélection de ligne complète
           (update (cell-indices)
             (cond ((member cell-indices selected-cells :test #'cells=)
                    ;; if whole-line? and whole line is not already selected just keep the cell
                    ;; selected
                    (unless whole-line?
                      (setf selected-cells (delete cell-indices selected-cells
                                                   :test #'cells=))
                      (setf (CED-selected-cells CED-data) selected-cells)
                      (set-cell-background-color shell-name :cell-indices cell-indices
                                                 :current-cell? (not whole-line?))))
                   (t (push cell-indices selected-cells)
                      (setf (CED-selected-cells CED-data) selected-cells)
                      (set-cell-background-color shell-name :cell-indices cell-indices)))))
      (cond ((or down?
                 ;; SWT missing feature, the mask is not updated (event du mouse down
                 ;; dans col header de table)
                 ;; see bug   https://bugs.eclipse.org/bugs/show_bug.cgi?id=65679
                 whole-line?)
             (cond (whole-line?
                    (let ((cells nil))
                      (if row-i (dotimes (ci (CED-columns-number CED-data))
                                  (unless (empty-cell? array row-i ci)
                                    (push (list row-i ci) cells)))
                        (dotimes (ri (CED-rows-number CED-data))
                          (unless (empty-cell? array ri column-i)
                            (push (list ri column-i) cells))))
                      ;; vérif. si toute la ligne est déjà sélectionnée
                      (if (set-difference cells selected-cells :test #'cells=)
                          (dolist (cell cells) (update cell))
                        (progn
                          (setf selected-cells
                                (set-difference selected-cells cells :test #'cells=)
                                (CED-selected-cells CED-data) selected-cells)
                          (dolist (cell cells)
                            (set-cell-background-color shell-name :cell-indices cell
                                                       :current-cell? nil))))))
                          
                   (t (update cell-indices))))
            (t (setf (CED-selected-cells CED-data) nil)
               (dolist (cell-indices selected-cells nil)
                 (set-cell-background-color shell-name :cell-indices cell-indices)))))))

#|
OPTIMISATION
- stocker les tableitems (évite de faire table.getitem)
- marshalling : pour accès simultané cell-bounds-x y w h
  et fonction lisp inside-rectangle?
- cache des rectangles

TEST ACCES A LA DERNIERE CELL :
******************************
-- fonction de base (anciennement : sans cache, seulement appel à tableitem.getbounds
                                  et rectangle.x)
user time    =      1.109

NOTE
----
 o ajouter un appel à  rectangle.y :  1.625 !
   plus appel rectangle.width    :    2.156
 o supprimer appel à rectangle.x :    0.9

OPTIMISATION cache
- cache de rx ry rw et rh :
Cela prend 2.390 seconde pour remplir tout le cache, i.e. en faisant en sorte
d'accèder la dernière cell la première fois.
- Ensuite, le cache étant rempli, time = 0
|#

(defun inside-rectangle? (rx ry rw rh x y)
  (declare (type fixnum rx ry rw rh x y))
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (and (i<= rx x)
       (i<= x (i+ rx rw))
       (i<= ry y)
       (i<= y (i+ ry rh))))


;;; returns the row and columm indices or nil if there is no current cell
;;; NOTE: columm indice is the indice in the true columms (the first zero-sized column
;;;       is ignored)
(defun get-current-cell (shell-name
                         table
                         x y
                         &key
                         (rows-number (table.getitemcount table))
                         (columns-number (table.getcolumncount table))
                         )
  (let* ((CED-data (getf *ced-info* shell-name))
         (table-cache (CED-table-cache CED-data)))
    (progn ; time
     (loop for ri from 0 below rows-number
           ;; (table.getitem table ri)
           as item in (CED-tableitems CED-data)
           do
           ;; sometimes get this error : java.lang.Exception: Invalid reference id
           (ignore-errors+
               (format t "~% error in get-current-cell: ~s ~s ~s ~s" ri item x y)
             (loop with rect and rx and ry and rw and rh
                   for ci from 0 below columns-number
                   as cached-item = (aref table-cache ri ci)
                   do
                   (cond (cached-item
                          (setf rx (first cached-item)
                                ry (second cached-item)
                                rw (third cached-item)
                                rh (fourth cached-item)))
                         (t
                          ;;(with-marshalling (1 +MARSHALL-NO-IDS+)
                          ;; with with-marshalling, tableitem.getbounds returns ((:EMPTY)) !
                          (setf rect
                                ;;  1+ because the zero-sized first column!
                                (tableitem.getbounds item (1+ ci)))
                          ;; (show-fref rect)
                          (setf
                           rx (rectangle.x rect)
                           ry (rectangle.y rect)
                           rw (rectangle.width rect)
                           rh (rectangle.height rect))
                          (setf (aref table-cache ri ci)
                                (list rx ry rw rh))))
                   (cond
                    ((< x rx) (return nil))
                    ;; with no caching, would have to do (rectangle.contains rect x y)
                    ((inside-rectangle? rx ry rw rh x y)
                     (return-from get-current-cell (values (list ri ci) item)))
                    (t nil)))))
     ))
  ;; no current cell (clicking oustside any columns, still in the table.getclientarea)
  nil)

;;; ****************************************************************************
;;; 

#|
TEST - TODO
-----------

- ajuster la hauteur des lignes de la table des row-headers selon la hauteur des lignes
  de la table principale (après chaque pack de column).
  N'est pas possible (SWT)
  o il y a tableitem.getbounds mais pas tableitem.setbounds - par altération ?
  o sinon faire les row-headers via une colonne de labels
ou
 Listener paintListener = new Listener() {
   public void handleEvent(Event event) {
    switch(event.type) {
     case SWT.MeasureItem: {
      event.height = height;
      break;
     }
    }
   }
  };
  table.addListener(SWT.MeasureItem, paintListener);

Et générer l'event comme ceci :
Invoking table1.redraw() will force MeasureItem to be called on your hooked
MeasureItem listener.  Just be aware of the following row height change
restrictions:
- changing the height of one row will change the heights of all rows in the
table to this value; this should not be a problem in your context since this
is how all swt Tables behave, including your table2
- a row height will never become smaller, only taller

- Eventuellement, pour plus d'efficacité, avoir autant de table (controls) que de views



1.

(setf *alpha-array
      (let* ((rows-number 5)
             (columns-number 4)
             (array (make-array (list rows-number columns-number))))
        (loop for i from 0 below rows-number do
              (loop for j from 0 below columns-number do
                    (setf (aref array i j)
                          (if (= i j)
                              :empty-cell
                            (+ (* i columns-number) j)))))
        array))

(def-swt-test
  (cells-editor
   *alpha-array
   :row-headers '("A" "B" "CCC")
   :set-cell-fn
   (lambda (table array ri ci table-item) ; RowIndice ColumnIndice
     (declare (ignore table))
     (CED-settext table-item ci (format nil "~a" (aref array ri ci))))
   :on-cell-mouse-down
   ;; cell-indices: (RowIndice ColumnIndice)
   (lambda (event table array cell-indices table-item)
     (declare (ignore event table))
     (let ((ri (first cell-indices))
           (ci (second cell-indices)))
       (ok-di
        (format nil
                "row indice: ~d~%column indice: ~d~%array value: ~a~%SWT table value: ~a"
                ri ci
                (aref array ri ci)
                (CED-gettext table-item ci)))))))


;;; viewing 1 array of numbers
(progn
  (defvar *alpha-array nil)
  (let ((rows-number 3)  
        (columns-number 4))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (if (= i j)
                                  :empty-cell
                                (+ (* i columns-number) j)))))
            array)))
  (def-swt-test
    (cells-editor
     *alpha-array
     :column-headers '("-0-" "---1---")
     :row-headers '("A" "B" "CCC"))))


;; text and images
(progn
  (defvar *alpha-array nil)
  (let ((rows-number 3)  
        (columns-number 4))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (if (= i j)
                                  :empty-cell
                                (+ (* i columns-number) j)))))
            array)))
  (def-swt-test
    (let ((image1 (display.getsystemimage *display* *SWT.ICON_INFORMATION*))
          (image2 (display.getsystemimage *display* *SWT.ICON_QUESTION*)))
      (cells-editor
       *alpha-array
       :set-cell-fn
       (lambda (table array ri ci table-item) ; RowIndice ColumnIndice
         (declare (ignore table))
         (if (evenp ci)
             (CED-setimage table-item ci image2)
           (CED-setimage table-item ci image1))
         (CED-settext table-item ci 
                      (format nil "~a" (aref array ri ci))))))))

;;; - view 1 array of numbers
;;; - change the value of cells
(progn
  (defvar *alpha-array nil)
  (let ((rows-number 10)  
        (columns-number 10))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (if (= i j)
                                  :empty-cell
                                (+ (* i columns-number) j)))))
            array)))
  (def-swt-test
    (let ((cells-ed-name :cells-editor-test))
      (cells-editor
       *alpha-array
       :name cells-ed-name
       :on-cell-mouse-down
       (lambda (event table array cell-indices table-item)
         (let ((ci (second cell-indices))
               (new-content "Visited"))
           ;; CED-settext-and-pack: set text of cell CI in table-item if New-Content
           ;; is different from the current content
           (unless (CED-settext-and-pack new-content cells-ed-name ci table-item)
             (beep)))
         ))))))


;;; 2 views, 2 arrays of scalars
(progn
  (defvar *alpha-array nil)
  (defvar *beta-array nil)
  (let ((rows-number 3)  
        (columns-number 4))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (if (= i j)
                                  :empty-cell
                                (let ((v (+ (* i columns-number) j)))
                                  v)))))
            array))
    (setf *beta-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (if (= i j)
                                  :empty-cell
                                (let ((v (+ 10000 (* i columns-number) j)))
                                  v)))))
            array)))
  (def-swt-test
    (cells-editor
     *alpha-array
     :column-headers '("--0--" "---1---" "---2---")
     :row-headers '("A" "iiiiiiiiiiiiiiiiiiiiiii" "MMMMMtttttttM")   ))

  ;; (getf *CED-info* shell-name)
  ;; 2 views , 2 arrays of scalar values
  (def-swt-test
    (let ((cells-editor-name :cells-editor-test))
      (cells-editor
       *alpha-array
       :name cells-editor-name
       :column-headers '("--0--" "---1---")
       :row-headers '("iiiiii" "BBB" "CCCC")
       :multi-view-combo-range `("Alpha values" "Beta values")
       :multi-view-combo-selection-fn
       (lambda (event control-data) event
         (CED-set-array cells-editor-name
                        (if (string-equal "Alpha values"
                                          (get-control-value control-data))
                            *alpha-array
                          *beta-array))))                 
      )))

;;; 3 views, 1 array of CLOS instances (3 slots to view)
(progn
  (defvar *array nil)
  (defclass test-data ()
    ((alpha-value :accessor alpha-value :initarg :alpha-value)
     (beta-value :accessor beta-value :initarg :beta-value)
     (omega-value :accessor omega-value :initarg :omega-value)
     (what-to-view :accessor what-to-view :initform "Alpha values"
                   :allocation :class)))
  (defmethod print-object ((i test-data) stream)
    (format stream "<test-data: ~a | ~a | ~a | ~a>" (alpha-value i) (beta-value i)
            (omega-value i)(what-to-view i))
    i)
  (let ((rows-number 27)
        (columns-number 25))
    (setf *array
          (let ((array (make-array (list rows-number columns-number))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (if (= i j)
                                  :empty-cell
                                (make-instance 'test-data
                                               :alpha-value (+ (* i columns-number) j)
                                               :beta-value (+ 1000 (* i columns-number ) j)
                                               :omega-value (gensym))))))
            array)))
  (def-swt-test
    (let ((cells-editor-name :cells-editor-test))
      (setf (class-slot-value 'test-data 'what-to-view) "Alpha values")
      (cells-editor
       *array
       :name cells-editor-name
       :alignement '(:right :left :center)
       ;; initial value of the combo defaults to the first range value
       :multi-view-combo-range `("Alpha values" "Beta values" "Omega values")
       :multi-view-combo-selection-fn
       (lambda (event control-data) event
         (setf (class-slot-value 'test-data 'what-to-view)
               (get-control-value control-data)))
       :set-cell-fn
       (lambda (table array ri ci table-item) ; RowIndice ColumnIndice
         (declare (ignore table))
         (let* ((instance (aref array ri ci))
                (what-to-view (class-slot-value 'test-data 'what-to-view))
                (reader (cond ((string= what-to-view "Alpha values") #'alpha-value)
                              ((string= what-to-view "Beta values") #'beta-value)
                              (t #'omega-value))))
           (CED-settext table-item ci 
                        (format nil "~a" (funcall reader instance)))))))))


;; on-cell-mouse-down and modifier-key-down?
(progn
  (defvar *alpha-array nil)
  (let ((rows-number 3)  
        (columns-number 4))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (+ (* i columns-number) j))))
            array)))
  (def-swt-test
    (cells-editor
     *alpha-array
     :on-cell-mouse-down
     (lambda (event table array cell-indices table-item)
       (declare (ignore table))
       (let ((ri (first cell-indices))
             (ci (second cell-indices)))
         (ok-di (format nil
                        "row indice: ~d~%column indice: ~d~%array value: ~a~%table value: ~a"
                        ri ci (aref array ri ci) (CED-gettext table-item ci))
                :icon (if (modifier-key-down? (event.statemask event))
                          :error
                        :info))))))

  ))

;;; adding controls in the cells editor
(progn
  (defvar *alpha-array nil)
  (let ((rows-number 3)  
        (columns-number 4))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (+ (* i columns-number) j))))
            array)))
  (def-swt-test
    (cells-editor
     *alpha-array
     :grid-rows
     `(;; a grid-row
       (;; empty control in the first columm (column of the row headers table)
        nil
        (:type :label :initial-value "Label1")
        (:type :label :initial-value "Label2")
        )
       ))))

;;; adding controls in the cells editor without the horizontal span of the main cells table
(progn
  (defvar *alpha-array nil)
  (let ((rows-number 3)  
        (columns-number 4))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j)
                              (+ (* i columns-number) j))))
            array)))
  (def-swt-test
    (cells-editor
     *alpha-array
     :table-layout-data nil
     :grid-rows
     `(;; a grid-row
       (;; empty control in the first columm (column of the row headers)
        nil
        (:type :label :initial-value "Label1")
        (:type :label :initial-value "Label2")
        )
       ))))

;;; POUR PERFORMANCE OPTIMISATION - voir get-current-cell
;;; Test sur array de 2400 cells
(progn
  (defvar *alpha-array nil)
  (let ((rows-number 40) 
        (columns-number 60))
    (setf *alpha-array
          (let ((array (make-array (list rows-number columns-number ))))
            (loop for i from 0 below rows-number do
                  (loop for j from 0 below columns-number do
                        (setf (aref array i j) "-")))
            array))
    (def-swt-test
      (cells-editor
       *alpha-array
       :column-headers (loop for i below columns-number collect ".")))))

|#
