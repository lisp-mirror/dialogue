;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)


; (def-java-generic "ISDISPOSED" disposed? :return-t)
(def-java-generic "SETTEXT")
(def-java-generic "GETTEXT")
(def-java-generic "SETSIZE")
(def-java-generic "SETLOCATION")
(def-java-generic "SETLAYOUT")
(def-java-generic "SETLAYOUTDATA")
(def-java-generic "PACK")
(def-java-generic "SETSELECTION")
(def-java-generic "SETDATA")
(def-java-generic "GETDATA")
(def-java-generic "GETDISPLAY")
(def-java-generic "SETFOCUS")
(def-java-generic "SETITEMS")
(def-java-generic "SETFONT")
(def-java-generic "UPDATE")
(def-java-generic "GETPARENT")
;(DEF-java-generic "TOSTRING")


(defun settext.combo-or-list (control text)
  (let ((text (if text text "")))
    (if (typep_foil control 'combo.)
        (combo.settext control text)
      (list.setitem control 0 text))))

;;; ****************************************************************************
;;; style constants

;; +label-styles+
(def-style-constant
 :label 
 '(:left :wrap :center :right :separator :horizontal :vertical
   :shadow_in :shadow_out :shadow_none)
 '((:shadow_in :shadow_out :shadow_none)))

(def-style-constant
 :button
 '(:push :arrow :check :radio :toggle :flat
   :up :down :left :right :center)
 '((:arrow :check :push :radio :toggle)
   (:up :down :left :right)))

(def-style-constant
 :text
 '(:left :center :multi :password :single :right :read_only :wrap
   :border
   ;;? NO ?
   :v_scroll :h_scroll)
 '((:multi :single)))

(def-style-constant
 :combo
 '(:drop_down :read_only :simple)
 '((:drop_down :simple)))


(def-style-constant
 :list
 '(:single :multi :border :v_scroll :h_scroll)
 '((:single :multi)))


(def-style-constant :composite
                    '(:no_background :no_focus :no_merge_paints
                      :no_redraw_resize :no_radio_group :embedded))

(def-style-constant :sashform
                    '(:vertical :horizontal)
                    '(:vertical :horizontal))

;; only one
(def-style-constant
 :group
 '(:shadow_etched_in :shadow_etched_out :shadow_in
   :shadow_out :shadow_none))

;;only one
(def-style-constant
 :tabfolder
 '(:top :bottom))


