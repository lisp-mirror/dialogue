(in-package :dialogue)


(def-SWT-test  
  (let* ((name "Noé")
         (email "n@algo.be")
         (salary  0.0)
         (year 2000)
         ;; property list
         (data (list 'name name 'email email 'salary salary 'year year))
         (compound
          (loop for pname in data by #'cddr
                collect
                `(;; must be a property name in plist, see :accessor documentation however
                  :name ,pname
                  ;; just to allow Dialogue to define input validation functions
                  :value-type ,(case pname
                                 (salary 'float)
                                 (year 'fixnum)
                                 (t 'string))
                  :prompt ,(string-capitalize pname)))))
    (dialogue compound
              :main t
              :data data
              :edit :plist     ; !
              :title "Plist")
    (setf name (getf data 'name)
          email (getf data 'email)
          salary (getf data 'salary)
          year (getf data 'year))
    (values name email salary year)))
