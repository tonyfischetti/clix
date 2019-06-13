; --------------------------------------------------------------- ;
; cli argument macro

(defmacro def-cli-args (script-name after-name description &body body)
  (let* ((switches       (mapcar #'second    body))
         (longswitches   (mapcar #'third     body))
         (descriptions   (mapcar #'fourth    body))
         (expressions    (mapcar #'fifth     body))
         (flag-lines     (mapcar (lambda (x y z)
                                   (format nil "  ~A, ~17A~A~%" x y z))
                                 switches longswitches descriptions))
         (tmp (mapcar (lambda (x)
                        `((or
                            (string= current ,(second x)) (string= current ,(third x)))
                          (progn
                            (or-die ((format nil "Fatal error processing ~A flag (~A)~%~%~A" ,(second x) error! +USAGE-TEXT!+))
                              ,@(nthcdr 4 x)))))
                      body)))
  `(progn
     (macrolet ((assign-next-arg! (avar)
       `(progn (setq ,avar (cadr args!)) (process-args! (cddr args!)))))
       (or-die ("invalid arguments")
         (defparameter args!        nil)
         (defparameter bare-args!   nil)
         (defparameter +USAGE-TEXT!+
           (format nil "Usage: ~A ~A~%~A~%~%~A"
                   ,script-name ,after-name ,description
                   (format nil "~{~A~}" (list ,@flag-lines))))
         (defun print-usage! ()
           (format t "~A" +USAGE-TEXT!+)
           (die "" :status 0))
         (defun process-args! (args)
           (setq args! args)
           (unless (null args!)
             (let ((current (car args!)))
               (cond
                 ((cl-ppcre:scan "^-\\w\\w+$" current)
                            (process-args! (append
                                             (mapcar
                                               (lambda (x) (format nil "-~A" x))
                                               (cdr (cl-ppcre:split "" current)))
                                             (cdr args))))
                 ,@tmp
                 (t (progn (setq bare-args! (reverse (cons current bare-args!))) (process-args! (cdr args!))))
                 )
               ))))))))

;---------------------------------------------------------;

