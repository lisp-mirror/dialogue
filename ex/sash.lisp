(in-package :dialogue)

;;;; sashform


(def-SWT-test
  (dialogue
   `(:column :fill t ;; :spacing 6 :margintop 10 :marginheight 5  :marginwidth 15
     ((:sashform
       :name :sash
       :style :vertical
       :layout-data (:height 300 :width 200)
       ((:type :text :style (:multi :border :v_scroll))
        (:type :button :initial-value "111" :style (:push))
        (:type :button :initial-value "222")              
        ))))
   :name :test-di
   :title "Sashform"))


(def-SWT-test
  (dialogue
   `(:grid
     (((:sashform
        :name :sash1
        :style :vertical
        :layout-data (:height 200 :width 400
                      :horizontalalignment ,*swt.fill*
                      :grabexcesshorizontalspace t
                      :verticalalignment ,*swt.fill*
                      :grabexcessverticalspace t)
        ((:type :text :style (:multi :border)
          :initial-value "1st text control in Sash#1")
         (:type :text :style (:multi :border)
          :initial-value "2nd text control in Sash#1")
         (:sashform
          :name :sash2
          :style :horizontal
          ((:type :text :style (:multi :border :wrap)
            :initial-value "1st text control in Sash#2")
           (:type :text :style (:multi :border) :initial-value "2nd in Sash#2")
           (:type :text :style (:multi :border) :initial-value "3rd in Sash#2")))
         )))))
   :creation-init-function 
   (lambda (shell) shell
     ;; Setting the weights of :sash1
     (set-sash-weights (find-control :sash1 :test-di) (list 1 1 2)))
   :exit-controls nil
   :name :test-di
   :title "2 nested sashforms"))

(def-SWT-test
  (let ()
    (dialogue
     `(:grid
       (((:sashform
          :name :sash
          :style :vertical
          :layout-data (:height 300 :width 200
                        :horizontalalignment ,*swt.fill*
                        :grabexcesshorizontalspace t
                        :verticalalignment ,*swt.fill*
                        :grabexcessverticalspace t
                        )
          ((:type :text :style (:multi :border) :initial-value "111")
           (:type :text :style (:multi :border) :initial-value "222")
           (:type :text :style (:multi :border :v_scroll) :initial-value "333")
           )))))
     :creation-init-function 
     (lambda (shell) shell
       (swttools:set-sash-weights (find-control :sash :test-di)
                                  (list 3 2 1))
       )
     :main t
     :exit-controls nil
     :name :test-di
     :title "Sashform")))

(trace process-layout process-control-or-composite process-grid-layout
  process-compound process-composite apply-setX-options)

(def-SWT-test
  (let ()
    (dialogue
     `(:grid
       (((:sashform
          :name :sash
          :style :vertical
          :layout-data (:height 300 :width 200
                        :horizontalalignment ,*swt.fill*
                        :grabexcesshorizontalspace t
                        :verticalalignment ,*swt.fill*
                        :grabexcessverticalspace t
                        )
          :setbackground ,(system-color :red)
          ;; devrait fonctionner ! mais non...
          ;;:setweights ,(box-vector :int)
          ((:type :text :style (:multi :border) :initial-value "111")
           (:type :text :style (:multi :border) :initial-value "222")
           (:type :text :style (:multi :border :v_scroll) :initial-value "333")
           )))))
     :creation-init-function 
     (lambda (shell) shell
       (swttools:set-sash-weights (find-control :sash :test-di) (list 3 2 1))
       )
     :main t
     :exit-controls nil
     :name :test-di
     :title "Sashform")))



(def-SWT-test
  (dialogue `((:sashform
               :name :sash
               :style :vertical
               :layout-data (:height 300 :width 200)
               ((:type :text :style (:multi :border :v_scroll))
                (:type :button :initial-value "111" :style (:push))
                (:type :button :initial-value "222")              
                )))
            :creation-init-function 
            (lambda (shell) shell)
            :main t
            :name :test-di
            :title "Sashform"))

  

