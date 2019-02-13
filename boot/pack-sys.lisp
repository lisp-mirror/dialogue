(in-package :user)

;;; ****************************************************************************
;;; packages

(defpackage :foil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :dlg
    (:use :common-lisp :lispworks :foil)
    (:import-from :user
     without-redefinition-warnings
     without-protection-errors)
    (:nicknames :dialogue)))

(loadb (make-boot-path "pack-dialogue"))

(use-package '(:dialogue
               :foil
               )
             :cl-user)

(defparameter *java-packages*
  '(
    "java.lang"
    "java.io"
    "java.util"
    ;; SWT
    "org.eclipse.swt"
    "org.eclipse.swt.widgets"
    "org.eclipse.swt.events"
    "org.eclipse.swt.graphics"
    "org.eclipse.swt.browser"
    "org.eclipse.swt.layout"
    "org.eclipse.swt.custom"
    "org.eclipse.swt.awt"
    "org.eclipse.swt.dnd"
    "org.eclipse.swt.accessibility"
    ))

(defun make-java-packages ()
  (loop for jpn in *java-packages*  
        do
        (let* ((jpn (string-downcase jpn))
               (jp (find-package jpn)))
          (cond (jp nil)
                (t
                 (msg "making java package " jpn)
                 (setf jp (make-package jpn)))))))

(defun finalize-java-packages (&optional make?)
  (loop for jpn in *java-packages*  
        do
        (finalize-java-package jpn make?)))

(defun finalize-java-package (jpn &optional make?)
  (let* ((jpn (string-downcase jpn))
         (jp (find-package jpn)))
    (when (and (not jp) make?)
      (setf jp (make-package jpn)))
    (when jp 
      (dolist (p '(:cl-user :dialogue))
        (use-package jp p)))))

 
;;; ****************************************************************************
;;; systems

(defsystem :foil
  (:default-pathname
   (merge-pathnames
    (make-pathname :directory '(:relative "foil"))
    *application-startup-directory*))
  :members ("foil" "patch")
  :rules ((:in-order-to :compile (:all)
           (:caused-by (:compile :previous))
           (:requires (:load :previous)))))


(defsystem :dialogue
  (:default-pathname
   (merge-pathnames
    (make-pathname :directory '(:relative "dialogue"))
    *application-startup-directory*))
  :members
  ("cl-util"
   "display-shell"
   "java"
   "swt-util"
   "swt-util2"
   "simple-di"
   "util"
   "defs"
   "defaults"
   "dialogue"
   "control"
   "write"
   "cells-editor")
  :rules
  ((:in-order-to
    :compile
    (:all)
    (:requires
     (:load
      "cl-util"
      "display-shell"
      "java"
      "swt-util"
      "swt-util2"
      "simple-di"
      "util"
      "defs"
      "defaults")))))

;;; ****************************************************************************
;;;


