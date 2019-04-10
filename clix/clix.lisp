;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;;      Personal common lisp utilities              ;;
;;                                                  ;;
;;              Tony Fischetti                      ;;
;;              tony.fischetti@gmail.com            ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :clix
  (:use :common-lisp :sb-ext)
  (:export :slurp
           :barf
           :explain
           :die
           :advise
           :or-die
           :err!
           :die-if-null
           :progress
           :get-size
           :for-each
           :for-each-list
           :for-each-vector
           :for-each-hash
           :for-each-line
           :for-each-in-stream
           :cmdargs
           :clear
           :-<>
           :<>
           :zsh
           :eval-always
           :abbr
           :str-join
           :substr
           :aif
           :interpose
           :set-hash
           :get-hash
           :print-hash-table
           :index!
           :line!
           :value!
           :key!
           :this-loop!
           :clix-log
           :*clix-output-stream*
           :*clix-log-level*
           :*clix-zsh*
           :+yellow-bold+
           :+green-bold+
           :+cyan-bold+
           :+reset-terminal-col+
           ))
(in-package :clix)

;---------------------------------------------------------;




;---------------------------------------------------------;
; parameters

; (defparameter *clix-output-stream* *standard-output*)
(defparameter *clix-output-stream* *terminal-io*)
(defparameter *clix-log-level* 1)

(defvar +yellow-bold+         (format nil "~c[33;1m" #\ESC))
(defvar +green-bold+          (format nil "~c[32;1m" #\ESC))
(defvar +cyan-bold+           (format nil "~c[36;1m" #\ESC))
(defvar +reset-terminal-col+  (format nil "~c[0m" #\ESC))

(defparameter *clix-zsh* "/usr/local/bin/zsh")


;---------------------------------------------------------;




;---------------------------------------------------------;
; convenience

(defun slurp (path)
  "Reads file at PATH into a single string"
  (with-open-file (stream path :if-does-not-exist :error)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


(defun barf (path contents &key (printfn #'write-string) (overwrite nil))
  "Outputs CONTENTS into filename PATH with function PRINTFN
   (default WRITE-STRING) and appends by default (controllable by
   by boolean OVERWRITE)"
  (with-open-file (stream path :direction :output
                          :if-exists (if overwrite :supersede :append)
                          :if-does-not-exist :create)
    (funcall printfn contents stream)))


(defmacro explain (message &rest body)
  "prints MESSAGE to standard output before eval-ing BODY"
  `(progn (format t "~A~%" ,message) ,@body))



(defmacro or-die ((message &key (errfun #'die)) &body body)
  "anaphoric macro that binds ERR! to the error
   It takes a MESSAGE with can include ERR! (via
   (format nil...) for example) It also takes ERRFUN
   which it will FUNCALL with the MESSAGE. The default
   is to DIE, but you can, for example, PRINC instead"
  `(handler-case
     (progn
       ,@body)
     (error (err!)
       (funcall ,errfun (format nil "~A" ,message)))))


(defmacro die-if-null (avar &rest therest)
  "Calls CLIX:DIE if any of the arguments are NIL"
  `(when (not (and ,avar ,@therest))
     (die "Error: at least one of the arguments is NIL")))


(defun die (message &key (status 1))
  "Prints MESSAGE to *ERROR-OUTPUT* and quits with a STATUS (default 1)"
  (format *error-output* "~A~%" message)
  #+clisp (ext:exit status)
  #+sbcl  (sb-ext:quit :unix-status status))

(defun advise (message)
  "Prints MESSAGE to *ERROR-OUTPUT* but resumes
   (for use with OR-DIE's ERRFUN)"
   (format *error-output* "~A~%" message))


(declaim (inline set-hash))
(defun set-hash (ht key val)
  "Set VAL at KEY in hash-table HT."
  (setf (gethash key ht) val))


(declaim (inline get-hash))
(defun get-hash (ht key)
  "Get value at KEY in hash-table HT"
  (gethash key ht))


(declaim (inline rem-hash))
(defun rem-hash (ht key)
  "Remove KEY in hash-table HT"
  (remhash key ht))


(defun cmdargs ()
  "A multi-implementation function to return argv (program name is CAR)"
  (or
   #+CLISP (cons "program_name" *args*)
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))


(defun clear ()
  "A multi-implementation function to clear the terminal screen"
   #+clisp    (shell "clear")
   #+ecl      (si:system "clear")
   #+sbcl     (sb-ext:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*)
   #+clozure  (ccl:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*))


(defmacro -<> (expr &rest forms)
  "Threading macro (put <> where the argument should be
   Stolen from https://github.com/sjl/cl-losh/blob/master/src/control-flow.lisp"
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))


(defun zsh (acommand &key (err-fun #'(lambda (code stderr)
                                       (error
                                         (format nil "~A (~A)"
                                                 stderr code))))
                          (echo nil))
  (flet ((strip (astring)
    (if (string= "" astring)
      astring
      (subseq astring 0 (- (length astring) 1)))))
    (when echo
      (format t "$ ~A~%" acommand))
    (let* ((outs        (make-string-output-stream))
           (errs        (make-string-output-stream))
           (theprocess  (run-program *clix-zsh* `("-c" ,acommand)
                                     :output outs
                                     :error  errs))
           (retcode     (process-exit-code theprocess)))
      (when (> retcode 0)
        (funcall err-fun retcode (strip (get-output-stream-string errs))))
      (values (strip (get-output-stream-string outs))
              (strip (get-output-stream-string errs))
              retcode))))

;---------------------------------------------------------;





;---------------------------------------------------------;
; for-each and friends

(declaim (inline progress))
(defun progress (index limit &key (interval 1) (where *standard-output*))
  (when (= 0 (mod index interval))
    (format where "~A of ~A..... [~$%]~%" index limit (* 100 (/ index limit)))))

(defgeneric get-size (obj))

(defmethod get-size ((alist list))
  (list-length alist))

(defmethod get-size ((ahash hash-table))
  (hash-table-count ahash))

(defmethod get-size ((avector vector))
  (length avector))


(defmacro for-each ((a-thing &key (size? nil)) &body body)
  ;doesn't variable capture now
  (let ((tmp (gensym)))
    `(let ((index!    -1)
           (key!      nil)
           (value!    nil)
           (size!     nil)
           (,tmp      ,a-thing))
      (when ,size? (setf size! (get-size ,tmp)))
      (etypecase ,tmp
        (hash-table     (for-each-hash   (index! key! value! ,tmp) ,@body))
        (vector         (for-each-vector (index! value! ,tmp)      ,@body))
        (list           (for-each-list   (index! value! ,tmp)      ,@body))))))


(defmacro for-each-list ((index item a-list) &body body)
  `(let ((,index -1)) (dolist (,item ,a-list) (incf ,index) (block this-loop! ,@body))))


(defmacro for-each-vector ((index item a-vector) &body body)
  `(let ((,index -1)) (loop for ,item across ,a-vector
                            do (progn (incf ,index) (block this-loop! ,@body)))))


(defmacro for-each-hash ((index key value a-hash) &body body)
  `(let ((,index -1)) (loop for ,key being the hash-keys of ,a-hash
                            do (progn (incf ,index) (setf ,value (gethash ,key ,a-hash)) (block this-loop! ,@body)))))

(defmacro for-each-line ((afilename &key (external-format :default)) &body body)
  (let ((instream   (gensym))
        (resolvedfn (gensym)))
    `(let* ((index!        -1)
            (line!         nil)
            (,resolvedfn  ,afilename)
            (,instream    (open ,resolvedfn :if-does-not-exist :error :external-format ,external-format)))
       (loop for line! = (read-line ,instream nil)
             while line! do (progn (incf index!) (block this-loop! ,@body)))
       (close ,instream))))

(defmacro for-each-in-stream (astream &body body)
  (let ((instream   (gensym)))
    `(let* ((index!        -1)
            (line!         nil)
            (,instream    ,astream))
       (loop for line! = (read-line ,instream nil)
             while line! do (progn (incf index!) (block this-loop! ,@body)))
       (close ,instream))))


;---------------------------------------------------------;





;---------------------------------------------------------;
; Stolen or inspired by https://github.com/vseloved/rutils/

(define-condition rutils-style-warning (simple-condition style-warning) ())

(defmacro eval-always (&body body)
  "Wrap BODY in eval-when with all keys (compile, load and execute) mentioned."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro abbr (short long &optional lambda-list)
  "Abbreviate LONG macro or function name as SHORT.
   If LAMBDA-LIST is present, also copy appropriate SETF-expander."
  `(eval-always
     ;; Lispworks signals error while abbreviating to keywords
     ;; SBCL has package locks when accessing built-in functionality
     ;; other similar things are probably possible in other implementations
     (handler-bind ((error (lambda (e)
                             (let ((r (find-restart 'continue e)))
                               (when r
                                 (warn 'rutils-style-warning
                                       :format-control
                                       "Skipped error during abbreviation: ~A"
                                       :format-arguments (list e))
                                 (invoke-restart r))))))
       (cond
         ((macro-function ',long)
          (setf (macro-function ',short) (macro-function ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         ((special-operator-p ',long)
          (error "Can't abbreviate a special-operator ~a" ',long))
         ((fboundp ',long)
          (setf (fdefinition ',short) (fdefinition ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long))
          ,(when lambda-list
            `(define-setf-expander ,short ,lambda-list
               (values ,@(multiple-value-bind
                          (dummies vals store store-form access-form)
                          (get-setf-expansion
                           (cons long (remove-if (lambda (sym)
                                                   (member sym '(&optional &key)))
                                                 lambda-list)))
                          (let ((expansion-vals (mapcar (lambda (x) `(quote ,x))
                                                        (list dummies
                                                              vals
                                                              store
                                                              store-form
                                                              access-form))))
                            (setf (second expansion-vals)
                                  (cons 'list vals))
                            expansion-vals))))))
         (t
          (error "Can't abbreviate ~a" ',long)))
       (setf (documentation ',short 'function) (documentation ',long 'function))
',short)))

; (abbr ds-bind destructuring-bind)
; (abbr mv-bind multiple-value-bind)
; (abbr print# print-hash-table)
; (abbr get# gethash (key hashtable &optional default))
; (abbr set# sethash)
; (abbr getset# getsethash)
; (abbr rem# remhash)
; (abbr λ lambda)

(defun str-join (delim strings)
  "Join STRINGS with DELIM."
  (format nil (format nil "~~{~~A~~^~A~~}" delim) strings))

(defun substr (string start &optional end)
  "Efficient substring of STRING from START to END (optional),
  where both can be negative, which means counting from the end."
  (let ((len (length string)))
    (subseq string
            (if (minusp start) (+ len start) start)
            (if (and end (minusp end)) (+ len end) end))))

(defmacro aif (test then &optional else)
  "Like IF. IT is bound to TEST."
  `(let ((it ,test))
     (if it ,then ,else)))

(defun interpose (separator list)
  "Returns a sequence of the elements of SEQUENCE separated by SEPARATOR."
  (labels ((rec (s acc)
                (if s
                  (rec (cdr s) (nconc acc
                                      (list separator (car s))))
                  acc)))
    (cdr (rec list nil))))


(defun print-hash-table (ht &optional (stream *standard-output*))
  "Pretty print hash-table HT to STREAM."
  (let ((*print-pretty* t) (i 0))
    (pprint-logical-block (stream nil)
                          (pprint-newline :fill stream)
                          (princ "#{" stream)
                          (unless (eq (hash-table-test ht) 'eql)
                            (princ (hash-table-test ht) stream))
                          (pprint-indent :block 2 stream)
                          (block nil
                                 (maphash (lambda (k v)
                                            (pprint-newline :mandatory stream)
                                            (when (and *print-length* (> (incf i) *print-length*))
                                              (princ "..." stream)
                                              (return))
                                            (when (and k (listp k)) (princ #\' stream))
                                            (if (typep k 'hash-table)
                                              (print-hash-table k stream)
                                              (prin1 k stream))
                                            (princ " " stream)
                                            (when (and v (listp v)) (princ #\' stream))
                                            (if (typep v 'hash-table)
                                              (print-hash-table v stream)
                                              (prin1 v stream)))
                                          ht))
                          (pprint-indent :block 1 stream)
                          (pprint-newline :mandatory stream)
                          (princ "} " stream)))
  ht)

;---------------------------------------------------------;




;---------------------------------------------------------;
; Experimental logging and reader macros

(defun prettify-time-output (thetimeoutput)
  ; (format t "~A~%" (length thetimeoutput))
  ;(remove-if (lambda (x) (char= #\Return x)) thetimeoutput)
  (subseq thetimeoutput 0 (- (length thetimeoutput) 4)))
  ; (remove-if (lambda (x) (or (char= #\Linefeed x) (char= #\Return x))) thetimeoutput))



;;;;;; A HAVE TO REFACTOR THIS REALLY BAD
;;;;;; I JUST WANNA TRY SOMETHING OUT RIGHT QUICK


(defun clix-log-verbose (stream char arg)
  ;;;;;; HOW UNHYGENIC IS THIS???!!
  (declare (ignore char))
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
    (let ((sexp               (read stream t))
          (thetime            (get-universal-time))
          (thereturnvalue     nil)
          (thetimingresults   nil)
          (daoutputstream     (make-string-output-stream)))
      `(progn
         (format *clix-output-stream*
                 "--------------------~%[~A-~A-~A ~2,'0d:~2,'0d:~2,'0d]~%~%FORM:~%~A~%"
                 ,year ,month ,date ,hour ,minute ,second
                 ; (write-to-string ',sexp))
                 (format nil "λ ~S~%" ',sexp))
         (let ((daoutputstream (make-string-output-stream)))
           (let ((*trace-output* daoutputstream))
             (setq thereturnvalue (progn (time ,sexp))))
               (setq thetimingresults (prettify-time-output (get-output-stream-string daoutputstream))))
         (format *clix-output-stream* "RETURNED:~%~A~%" thereturnvalue)
         (format *clix-output-stream* "~%~A~%--------------------~%~%~%" thetimingresults)
         (finish-output *clix-output-stream*)
         thereturnvalue))))


(defun clix-log-just-echo (stream char arg)
  ;;;;;; HOW UNHYGENIC IS THIS???!!
  (declare (ignore char))
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
    (let ((sexp               (read stream t))
          (thetime            (get-universal-time))
          (thereturnvalue     nil)
          (thetimingresults   nil)
          (daoutputstream     (make-string-output-stream)))
      `(progn
         (format *clix-output-stream* "~%λ ~S~%" ',sexp)
         (let ((daoutputstream (make-string-output-stream)))
           (let ((*trace-output* daoutputstream))
             (setq thereturnvalue (progn (time ,sexp))))
               (setq thetimingresults (prettify-time-output (get-output-stream-string daoutputstream))))
         ; (format *clix-output-stream* "RETURNED:~%~A~%" thereturnvalue)
         ; (format *clix-output-stream* "~%~A~%--------------------~%~%~%" thetimingresults)
         (finish-output *clix-output-stream*)
         thereturnvalue))))


(defun clix-log (stream char arg)
  (cond ((= *clix-log-level* 2)    (clix-log-verbose   stream char arg))
        ((= *clix-log-level* 1)    (clix-log-just-echo stream char arg))
        (                          nil)))


(set-dispatch-macro-character #\# #\! #'clix-log)



; (set-macro-character #\💯 #'clix-log3)

