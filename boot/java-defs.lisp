(in-package :cl-user)

;;; global constant pool
;;; *SWT.LEFT*, *swt.drop_down* , *SWT.COLOR_BLACK* etc...
(without-redefinition-warnings
  (def-foil-class "java.lang.Object")
  (def-foil-class "java.lang.String")
  (def-foil-class "java.util.Properties")

  (def-foil-class "org.eclipse.swt.SWT")
  )
