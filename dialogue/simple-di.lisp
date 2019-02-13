;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)


(defvar *dialog-default-title* "")


;;; ****************************************************************************
;;; Simple common dialogs : ok-di, ok-cancel, ...

#|

(def-SWT-test (with-shell (ok-di "Hello")))
(def-SWT-test (with-shell (ok-cancel-di "Hello"))))
(def-SWT-test (with-shell (m (yes-no-di "Hello"))))
(def-SWT-test (with-shell (m (yes-no-cancel-di "Hello"))))

(def-SWT-test (with-shell (m (ok-di "Hello" :icon :warning))))
(def-SWT-test (with-shell (m (ok-di "Hello" :icon :error))))
(def-SWT-test (with-shell (m (ok-di "Hello" :icon :working))))
(def-SWT-test
  (with-shell (m (simple-dialog "error message"
                                *default-swt-parent*
                                :error
                                "Titre"
                                '(:yes :no)))))
|#

(defconstant +done-message+ "Done")

(defun done-di (&optional (message +Done-message+)
                          &key (parent *default-swt-parent*)
                          (title *dialog-default-title*))
  (unless message
    (setf message +Done-message+))
  (ok-di message :parent parent :title title)
  nil)



(defun error-di (message &key
                         condition
                         (parent *default-swt-parent*)
                         (title *dialog-default-title*))
  (when condition
    (setf message
          (format nil "An error has occured:~&~a~2%~a"
                  condition
                  message)))
  (simple-dialog message parent :error title '(:ok)))



(defun ok-di (message &key
                      (parent *default-swt-parent*)
                      (icon :information)
                      (title *dialog-default-title*))
  (simple-dialog message parent icon title '(:ok))
  nil)

(defun ok-cancel-di (message &key
                             (parent *default-swt-parent*)
                             (icon :question)
                             (title *dialog-default-title*))
  (eq :yes
      (simple-dialog message parent icon title '(:ok :cancel))))


(defun yes-no-di (message &key
                          (parent *default-swt-parent*)
                          (icon :question)
                          (title *dialog-default-title*))
  (eq :yes
      (simple-dialog message parent icon title '(:no :yes))))

;;; returns :yes , :no or :cancel
(defun yes-no-cancel-di (message &key
                                 (parent *default-swt-parent*)
                                 (icon :question)
                                 (title *dialog-default-title*))
  (simple-dialog message parent icon title '(:yes :no :cancel)))


;;; icon is one of: :information (:info) :warning (:warn) :question :error :working (:work)
;;; buttons : an unordered list of one or more of these keywords:
;;;   :ok :cancel :yes :no :abort :retry :ignore
;;; Returns one of the keywords
;;; If parent is nil, do nothing
(defun simple-dialog (message parent icon title buttons)
  (when parent
    (when (or (not title) (string= title ""))
      (setf title *dialog-default-title*))
    (let ((di (make-new
               messagebox.
               parent
               (apply #'logior
                      (case icon
                        (:error *swt.icon_error*)
                        (:question *swt.icon_question*)
                        ((:warn :warning) *swt.icon_warning*)
                        ((:work :working) *swt.icon_working*)
                        ((:information :info) *swt.icon_information*)
                        (t (error "unknown icon style ~s
Should be a keyword (:question, :error, :warning, :working, :information" icon)))
                      (loop for b in buttons
                            collect (case b
                                      (:ok *swt.ok*)
                                      (:cancel *swt.cancel*)
                                      (:yes *swt.yes*)
                                      (:no *swt.no*)
                                      (:abort *swt.abort*)
                                      (:retry *swt.retry*)
                                      (:ignore *swt.ignore*)
                                      (t (error "unknown button style ~s
Buttons should be a list of keywords (:ok, :cancel, :yes, :no, :abort, :retry, :ignore)" b))))))))
      (messagebox.setmessage di message)
      (messagebox.settext di title)
      (let ((r (messagebox.open di)))
        (case r 
          (#.*swt.ok* :ok)
          (#.*swt.cancel* :cancel)
          (#.*swt.yes* :yes)
          (#.*swt.no* :no)
          (#.*swt.abort* :abort)
          (#.*swt.retry* :retry)
          (#.*swt.ignore* :ignore)
          (t (error "unknown answer ~s" r)))))))
