(in-package :cl-user)

(require "comm")
(require "hqn-web")


;;; ****************************************************************************
(
defvar *application-startup-directory*
  #+:lispworks
  (let ((init-file (second (member "-init" sys:*line-arguments-list* :test #'string-equal))))
    (make-pathname
     :host (pathname-host init-file)
     :directory (pathname-directory init-file))))

(defparameter *java-dump-dir*
  (merge-pathnames
   (make-pathname
    :directory '(:relative "foildumps"))
   *application-startup-directory*))

(defparameter *swt-jar-path*
  (namestring 
   (make-pathname
    :defaults *application-startup-directory*
    :name "swt"
    :type  "jar")))

;;; ****************************************************************************

(defconstant +compiled-file-extension+
  #+:lispworks compiler:*fasl-extension-string*
  #-:lispworks
  (pathname-type (compile-file-pathname "foo.lisp")))

(defun loadb (lisp-file &key (recompile nil) (lisp nil))
  (let ((fasl-file (make-pathname :defaults lisp-file :type +compiled-file-extension+)))
    (when recompile
      (when (probe-file fasl-file)
        (delete-file fasl-file)))
    (when (and (not lisp)
               (or (not (probe-file fasl-file))
                   (>  (file-write-date lisp-file)(file-write-date fasl-file))))
      (compile-file lisp-file :output-file fasl-file))
    (load (if (or lisp
                  (not (probe-file fasl-file)))
              lisp-file
            fasl-file))))
            

;;; ****************************************************************************
;;;

(defun make-boot-path (file-name)
  (merge-pathnames
   (make-pathname :directory '(:relative "boot") :name file-name :type  "lisp")
   *application-startup-directory*))

(defun compile-dia ()
  (loadb (make-boot-path "util") :recompile t)
  (loadb (make-boot-path "pack-sys") :recompile t)
  (compile-system :foil :force t :load T)
  (loadb (make-boot-path "foil-aux") :recompile t)
  (start-JavaVM-Foil)
  (start-fvm)
  (dump&compile-java-defs)
  (loadb (make-boot-path "java-defs") :recompile t)
  (finalize-java-packages)
  (loadb (make-boot-path "employee") :recompile t)
  (compile-system :dialogue :force t :load t)
  (loadb (make-boot-path "pack-dialogue") :recompile t)
  )

(defun load-dia ()
  (loadb (make-boot-path "util"))
  (loadb (make-boot-path "pack-sys"))
  (load-system :foil)
  (loadb (make-boot-path "foil-aux"))
  (start-JavaVM-Foil)
  (start-fvm)
  ;;; car (def-foil-class "java.lang.Object") ... rate la création du package
  (make-java-packages)
  (loadb (make-boot-path "java-defs"))  
  (load-java-dumps '("swtwidgets"
                     "swtevents"
                     "swtgraphics"
                     "swtlayout"
                     "custom"
                     "browser"))
  (finalize-java-packages)
  (loadb (make-boot-path "employee"))
  (load-system :dialogue)
  (loadb (make-boot-path "pack-dialogue"))
  )


#|
(load-dia)
(compile-dia)
(compile-system :dialogue :force t :load t)
|#
