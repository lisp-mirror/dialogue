(in-package :dialogue)

(export
 '(
   ;; cl tools
   split-list-items-and-kv-pairs
   ensure-fdefinition
   symbol-macro?
   xor
   
   ;; foil
   typep_foil
   type-of_foil
   valid-fref?
   
   ;; Java
   ensure-boolean-value
   apply-j-method

   ;; display shell
   def-SWT-test
   run-SWT-ui
   *stack-size*
   make-swt-display
   dispose-swt-display
   ensure-swt-display
   shell-show
   with-shell&dispatch
   with-shell
   *default-cursor*
   *target-cursor*
   *waiting-cursor*
   *hand-cursor*

   ;; SWT utilities 1
   left-button? middle-button? right-button?
   button-down? button1-down? button1-down? button2-down? button3-down?
   control-down? alt-down? shift-down? modifier-key-down?
   without-redraw
   textextent
   beep
   flush-events
   system-color
   set-sash-weights
   
   ;; SWT utilities 2
   def-style-constant
   get-event-type
   get-any-event-type
   combine-styles
   get-style
   
   remove-disposed-instances

   ;; simple dialogs
   *dialog-default-title*
   ok-di ok-cancel-di yes-no-di yes-no-cancel-di
   done-di
   error-di
   simple-dialog

   ;; cells-editor
   cells-editor
   CED-fill-table
   CED-set-array
   CED-setimage
   CED-settext
   CED-settext-and-pack
   CED-gettext
   CED-pack
   CED-getcolumn

   ;; Dialogue
   make-exit-controls-composite-spec
   
   close-dialogs
  
   *trim-text-control-value?*
     
   *dialogue-default-font*
   *dialogue-default-trimmings*

   dialogue
   get-dialog
   dialog-exists?
   get-dialog-name
   find-dialog-data
   find-control-data
   find-control
   add-other-control
   get-control-value
   get-controls-values
   set-control-value
   set-controls-values
   set-control-enabled
   do-controls
   dd-control-data-list
   dd-dialog
   dd-result
   dd-plist

   do-written-back-controls
   data-written-back?
   cancel-control
   ok-control

   update-data

   cd-dialog-data
   cd-name
   cd-control

   get-range
   set-range
   add-to-range
   remove-from-range
   settext.combo-or-list
   reset-combo-or-list

   *dialog-data
   
   ))
   
