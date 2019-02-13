(in-package :dialogue)

;;;; - list control
;;;;   style
;;;;  :single :multi :border :v_scroll :h_scroll   
#|
   ;; :type :combo
             ;; truc pour contrer erreur quand :style comprend :read_only (+ :simple)
             ;; de toute façon :read_only semble buggé, (selection marche plus 
             ;; :initial-value "" ;; :range ,(list "")
             ;;:style (:simple :read_only)
             ;; :style :simple
             ;; :value-accepter ,(lambda (new control-data) (ok-di "Not editable"))
        
|#

;;; :multi
(def-SWT-test
  (let* ((value (list 1 2))
         (range (list 0 1 2 3 4))
         ;; property list
         (data (list :list-wi value))) 
    (dialogue 
     `((:edit :plist
        :name :list-wi
        :range ,range
        :type :list
        :style (:multi :v_scroll :h_scroll :border)
        ))
     :data data
     :title "List")
    (print (getf data :list-wi))))

;;; :single
(def-SWT-test
  (let* ((value 1)
         (range (list 0 1 2 3 4))
         ;; property list
         (data (list :list-wi value))) 
    (dialogue 
     `(
       (:edit :plist
        :name :list-wi
        :range ,range
        :type :list
        :style (:single :v_scroll :h_scroll :border)
        :value-accepter ,(lambda (new control-data) (oddp new))
        ))     
     :data data
     :title "List")
    (print (getf data :list-wi))))

(defun list-unselect (list)
  (list.setselection list (make-j-vector)))

(def-SWT-test
  (let* ((value (list 1 2))
         (range (list 0 1 2 3 4))
         ;; property list
         (data (list :list-wi value))) 
    (dialogue 
     `((:edit :plist
        :name :list-wi
        :range ,range
        :type :list
        :style (:multi :v_scroll :h_scroll :border)
        :event (:selection
                ,(lambda (event control-data)
                   (with-error- (:list)  event
                     (let ((value (get-control-value control-data)))
                       ;; ! le event.statemask est tjrs 0 ! bug SWT
                       ;; (when (modifier-key-down? (event.statemask event))
                       (ok-di (format nil "Value: ~s" value))
                       (list-unselect (CD-control control-data))
                       ))))
        )) 
     :data data
     :title "List")
    (print (getf data :list-wi))))



              
        