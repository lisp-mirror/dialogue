(in-package :foil)
(export '(new-proxy-2))

#|
In new-proxy, change:


(find-symbol (string-upcase (string-append (first interface-def)
                                           method-name))
             (symbol-package (first interface-def)))


to:


(if (find #\. (symbol-name method-name) :test #'char-equal)
    method-name
  (find-symbol (string-upcase (string-append (first interface-def)
                                             method-name))
               (symbol-package (first interface-def))))

|#

(defmacro new-proxy-2 (proxy arg-marshall-flags arg-marshall-depth &rest interface-defs)
  (let ((method-sym (gensym))
        (args-sym (gensym)))
    `(let ((,proxy (make-new-proxy ,arg-marshall-flags ,arg-marshall-depth
                                   ,@(mapcar #'car interface-defs))))
       ,@(mapcan
          (lambda (interface-def)
            (mapcar
             (lambda (method-def)
               (destructuring-bind (method-name args &body body) method-def
                 `(defmethod handle-proxy-call
                             ((,method-sym
                               (eql ',(if (find #\. (symbol-name method-name)
                                                :test #'char-equal)
                                          method-name
                                        (find-symbol (string-upcase
                                                      (string-append
                                                       (first interface-def)
                                                       method-name))
                                                     (symbol-package
                                                      (first interface-def))))))
                              (,proxy (eql ,proxy))
                              &rest ,args-sym)
                    (destructuring-bind ,args ,args-sym
                      ,@ body))))
             (rest interface-def)))
          interface-defs)
       ,proxy)))


#+copy-original
(defmacro new-proxy (proxy arg-marshall-flags arg-marshall-depth &rest interface-defs)
  (let ((method-sym (gensym))
        (args-sym (gensym)))
    `(let ((,proxy (make-new-proxy ,arg-marshall-flags ,arg-marshall-depth
                                   ,@(mapcar #'car interface-defs))))
       ,@(mapcan
          (lambda (interface-def)
            (mapcar
             (lambda (method-def)
               (destructuring-bind (method-name args &body body) method-def
                 `(defmethod handle-proxy-call
                             ((,method-sym
                               (eql ',(find-symbol
                                       (string-upcase
                                        (string-append (first interface-def)
                                                       method-name))
                                       (symbol-package (first interface-def)))))
                              (,proxy (eql ,proxy))
                              &rest ,args-sym)
                    (destructuring-bind ,args ,args-sym
                      ,@ body))))
             (rest interface-def)))
          interface-defs)
       ,proxy)))
