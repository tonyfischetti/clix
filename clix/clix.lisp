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
           :error!
           :die-if-null
           :progress
           :get-size
           :for-each
           :for-each-node-set
           :index!
           :value!
           :key!
           :this-pass!
           :this-loop!
           :continue!
           :break!
           :for-each->list
           :for-each->vector
           :collect!
           :cmdargs
           :clear-screen
           :-<>
           :<>
           :zsh
           :universal->unix-time
           :unix->universal-time
           :get-unix-time
           :get-current-time
           :make-pretty-time
           :request
           :xml-parse
           :xml-parse-file
           :xpath
           :xpath-1
           :xml-name
           :xml-text
           :eval-always
           :abbr
           :str-join
           :substr
           :aif
           :interpose
           :set-hash
           :get-hash
           :rem-hash
           :set-alist
           :get-alist
           :rem-alist
           :print-hash-table
           :with-gensyms
           :it!
           :clix-log
           :*clix-output-stream*
           :*clix-external-format*
           :*clix-log-level*
           :*clix-zsh*
           :ignore-the-errors-wrapper
           :+red-bold+
           :+green-bold+
           :+yellow-bold+
           :+blue-bold+
           :+magenta-bold+
           :+cyan-bold+
           :+reset-terminal-color+
           :re-compile
           :str-split
           :str-replace
           :str-replace-all
           :str-detect
           :str-subset
           :str-scan-to-strings
           :~m
           :~r
           :~ra
           :~s
           :~f
           :with-r
           :r-get
           :alist->hash-table
           :hash-table->alist
           :debug-these
           :with-a-file
           :stream!
           :rnorm
           :delim
           :defparams
           :if->then
           :if-this->then
           ))
(in-package :clix)

;---------------------------------------------------------;

(pushnew :clix *features*)

;---------------------------------------------------------;
; parameters

; (defparameter *clix-output-stream* *standard-output*)
(defparameter *clix-output-stream* *terminal-io*)
(defparameter *clix-log-level* 1)

(defparameter *clix-external-format* :UTF-8)

(defvar +red-bold+              (format nil "~c[31;1m" #\ESC))
(defvar +green-bold+            (format nil "~c[32;1m" #\ESC))
(defvar +yellow-bold+           (format nil "~c[33;1m" #\ESC))
(defvar +blue-bold+             (format nil "~c[34;1m" #\ESC))
(defvar +magenta-bold+          (format nil "~c[35;1m" #\ESC))
(defvar +cyan-bold+             (format nil "~c[36;1m" #\ESC))
(defvar +reset-terminal-color+  (format nil "~c[0m"    #\ESC))

(defparameter *clix-zsh* "/usr/local/bin/zsh")

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

;---------------------------------------------------------;

;---------------------------------------------------------;

; Stolen from Practical common lisp
(defmacro with-gensyms ((&rest names) &body body)
  "Why mess with the classics"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

; --------------------------------------------------------------- ;

;---------------------------------------------------------;
; Experimental logging and reader macros

(defun prettify-time-output (thetimeoutput)
  ; (format t "~A~%" (length thetimeoutput))
  ;(remove-if (lambda (x) (char= #\Return x)) thetimeoutput)
  (subseq thetimeoutput 0 (- (length thetimeoutput) 4)))
  ; (remove-if (lambda (x) (or (char= #\Linefeed x) (char= #\Return x))) thetimeoutput))

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
  (let ((sexp               (read stream t))
    ;       (thetime            (get-universal-time))
    ;       (thereturnvalue     nil)
    ;       (thetimingresults   nil))
      `(progn
         (format t "~S~%" ',sexp)
         (format *clix-output-stream* "~%λ ~S~%" ',sexp)
         (let* ((daoutputstream   (make-string-output-stream))
                (*trace-output*   daoutputstream)
                (thereturnvalue   (progn (time ,sexp))))
           (finish-output *clix-output-stream*)
           ,thereturnvalue)))))


(defun clix-log (stream char arg)
  (cond ((= *clix-log-level* 2)    (clix-log-verbose   stream char arg))
        ((= *clix-log-level* 1)    (clix-log-just-echo stream char arg))
        (                          nil)))


(set-dispatch-macro-character #\# #\! #'clix-log)

(defun ignore-the-errors-wrapper (stream char arg)
  (declare (ignore char))
  (let ((sexp (read stream t)))
    `(ignore-errors ,sexp)))

(set-dispatch-macro-character #\# #\? #'ignore-the-errors-wrapper)




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

; reader macro for this?


(defun die (message &key (status 1))
  "Prints MESSAGE to *ERROR-OUTPUT* and quits with a STATUS (default 1)"
  (format *error-output* "~A~%" message)
  #+clisp (ext:exit status)
  #+sbcl  (sb-ext:quit :unix-status status))

(defmacro or-die ((message &key (errfun #'die)) &body body)
  "anaphoric macro that binds ERR! to the error
   It takes a MESSAGE with can include ERR! (via
   (format nil...) for example) It also takes ERRFUN
   which it will FUNCALL with the MESSAGE. The default
   is to DIE, but you can, for example, PRINC instead"
  `(handler-case
     (progn
       ,@body)
     (error (error!)
       (funcall ,errfun (format nil "~A" ,message)))))


(defmacro die-if-null (avar &rest therest)
  "Macro to check if any of the supplied arguments are null"
  (let ((whole (cons avar therest)))
    `(loop for i in ',whole
           do (unless (eval i) (die (format nil "Fatal error: ~A is null" i))))))

(defun advise (message)
  "Prints MESSAGE to *ERROR-OUTPUT* but resumes
   (for use with OR-DIE's ERRFUN)"
   (format *error-output* "~A~%" message))


(defmacro set-hash (aht akey aval)
  (with-gensyms (theht thekey theval)
    `(let ((,theht  ,aht)
           (,thekey ,akey)
           (,theval ,aval))
       (setf (gethash ,thekey ,theht) ,theval))))


(declaim (inline get-hash))
(defun get-hash (ht key)
  "Get value at KEY in hash-table HT"
  (gethash key ht))


(declaim (inline rem-hash))
(defun rem-hash (ht key)
  "Remove KEY in hash-table HT"
  (remhash key ht))

(defmacro set-alist (aalist key value &key (test #'eq))
  "Adds `key` and `value` to an alist `aalist`
  (must be a macro in order to modify the alist in place)"
  `(if (null (assoc ,key ,aalist :test ,test))
    (push (cons ,key ,value) ,aalist)
    (setf (cdr (assoc ,key ,aalist :test ,test)) ,value)))

(declaim (inline get-alist))
(defun get-alist (aalist key &key (test #'eq))
  "Returns value of `key` of `aalist`"
  (cdr (assoc key aalist :test test)))

(defmacro rem-alist (aalist key)
  "Destructively deletes `key`/value pair from `aalist`
   (must be a macro in order to modify the alist in place)"
  `(setq ,aalist (remove-if (lambda (x) (eq (car x) ,key)) ,aalist)))


(defun cmdargs ()
  "A multi-implementation function to return argv (program name is CAR)"
  (or
   #+CLISP (cons "program_name" *args*)
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))


(defun clear-screen ()
  "A multi-implementation function to clear the terminal screen"
   #+clisp    (shell "clear")
   #+ecl      (si:system "clear")
   #+sbcl     (sb-ext:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*)
   #+clozure  (ccl:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*))


(defmacro -<> (expr &rest forms)
  "Threading macro (put <> where the argument should be)
   Stolen from https://github.com/sjl/cl-losh/blob/master/src/control-flow.lisp"
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))


(defun zsh (acommand &key (dry-run nil)
                          (err-fun #'(lambda (code stderr) (error (format nil "~A (~A)" stderr code))))
                          (echo nil)
                          (enc *clix-external-format*))
  "Runs command `acommand` through the ZSH shell specified by the global *clix-zsh*
   `dry-run` just prints the command (default nil)
   `err-fun` takes a function that takes an error code and the STDERR output
   `echo` will print the command before running it
   `enc` takes a format (default is *clix-external-format* [which is :UTF-8 by default])"
  (flet ((strip (astring)
    (if (string= "" astring)
      astring
      (subseq astring 0 (- (length astring) 1)))))
    (when (or echo dry-run)
      (format t "$ ~A~%" acommand))
    (unless dry-run
      (let* ((outs        (make-string-output-stream))
             (errs        (make-string-output-stream))
             (theprocess  (run-program *clix-zsh* `("-c" ,acommand)
                                       :output outs
                                       :error  errs
                                       :external-format enc))
             (retcode     (process-exit-code theprocess)))
        (when (> retcode 0)
          (funcall err-fun retcode (strip (get-output-stream-string errs))))
        (values (strip (get-output-stream-string outs))
                (strip (get-output-stream-string errs))
                retcode)))))
; --------------------------------------------------------------- ;


; --------------------------------------------------------------- ;
; time

(defun universal->unix-time (universal-time)
  "Converts universal (common lisp time from `(get-universal-time)` to UNIX time"
  (- universal-time *unix-epoch-difference*))

(defun unix->universal-time (unix-time)
  "Converts UNIX time to  universal (common lisp time from `(get-universal-time)`"
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  "Get current UNIX time"
  (universal->unix-time (get-universal-time)))

(defun make-pretty-time (a-unix-time &key (just-date nil) (just-time nil))
  "Makes a nicely formatted (YYYY-MM-DD HH?:MM:SS) from a UNIX time
   `just-date` will return just the pretty date
   `just-time` will return just the pretty time"
  (let ((thisuniversaltime (unix->universal-time a-unix-time)))
    (multiple-value-bind (second minute hour date month year)
      (decode-universal-time thisuniversaltime)
      (if (and (not just-date) (not just-time))
        (format nil "~d-~2,'0d-~2,'0d ~d:~2,'0d:~2,'0d" year month date hour minute second)
        (if just-date
          (format nil "~d-~2,'0d-~2,'0d" year month date)
          (format nil "~d:~2,'0d:~2,'0d" hour minute second))))))


(defun get-current-time (&key (just-date nil) (just-time nil))
  "Uses `make-pretty-time` to get the current datetime"
  (make-pretty-time (-<> (get-universal-time) universal->unix-time)
                    :just-date just-date :just-time just-time))

;---------------------------------------------------------;


; --------------------------------------------------------------- ;
; XML stuff

(defun request (aurl)
  (drakma:http-request aurl))

(defun xml-parse (astring)
  (cxml:parse astring (cxml-dom:make-dom-builder)))

(defun xml-parse-file (afile)
  (cxml:parse-file afile (cxml-dom:make-dom-builder)))

(defun xpath (adom anxpath)
  (xpath:evaluate anxpath adom))

(defun xpath-1 (adom anxpath)
  (xpath:first-node (xpath adom anxpath)))

(defun xml-name (andom)
  (dom:tag-name andom))

(defun xml-text (andom)
  (xpath:string-value andom))


; --------------------------------------------------------------- ;


;---------------------------------------------------------;
; for-each and friends

(declaim (inline progress))
(defun progress (index limit &key (interval 1) (where *standard-output*))
  (when (= 0 (mod index interval))
    (format where "~A of ~A..... [~$%]~%" index limit (* 100 (/ index limit)))))

(defmacro break! ()
  "For use with `for-each`
   It's short for `(return-from this-loop!"
  `(return-from this-loop!))

(defmacro continue! ()
  "For use with `for-each`
   It's short for `(return-from this-pass!"
  `(return-from this-pass!))

(defmacro for-each (a-thing &body body)
  "A super-duper imperative looping construct.
   It takes either
     a filename string    (to be treated as a file and goes line by line)
     a hash-table
     a vector
     a list
     a string             (that goes character by character)
     or a stream          (that goes line by line)
  It is anaphoric and introduces
     `index!`             (which is a zero indexed counter of which element we are on)
     `key!`               (the key of the current hash-table entry [only for hash-tables])
     `value!`             (the value of the current element)
     `this-pass!`         (a block that returning from immediately moves to the next iteration)
     `this-loop!`         (a block that returning from exits the loop)
  For convenience, `(continue!)` and `(break!)` will execute `(return-from this-pass!)`
  and `(return-from this-loop!)`, respectively
  If it's a filename, the external format is *clix-external-format* (:UTF-8 by default)
  Oh, it'll die if Control-C is used during the loops execution."
  (let ((tmp (gensym)))
    `(handler-case
       (let ((index!    -1)
           (key!      nil)
           (value!    nil)
           (,tmp      ,a-thing))
      (declare (ignorable index!))
      (declare (ignorable key!))
      (declare (ignorable value!))
      (cond
        ((and (listp ,tmp) (listp (car ,tmp)) (not (alexandria:proper-list-p (car ,tmp))))
                    (for-each-alist (index! key! value! ,tmp)               ,@body))
        ((and (stringp ,tmp) (cl-fad:file-exists-p ,tmp))
                    (for-each-line      (index! value! ,tmp)                ,@body))
        (t
          (progn
            (etypecase ,tmp
              ; (string         (for-each-line      (index! value! ,tmp)      ,@body))
              (hash-table     (for-each-hash      (index! key! value! ,tmp) ,@body))
              (vector         (for-each-vector    (index! value! ,tmp)      ,@body))
              (list           (for-each-list      (index! value! ,tmp)      ,@body))
              (xpath:node-set (for-each-node-set  (index! value! ,tmp)      ,@body))
              (stream         (for-each-stream    (index! value! ,tmp)      ,@body)))))))
       (sb-sys:interactive-interrupt ()
         (die (format nil "~%~ALoop aborted. Bailing out.~A~%" +red-bold+ +reset-terminal-color+))))))

(defmacro for-each-list ((index value a-list) &body body)
  `(let ((,index -1))
     (block this-loop!
            (dolist (,value ,a-list)
              (incf ,index)
              (block this-pass! ,@body)))))

(defmacro for-each-vector ((index value a-vector) &body body)
  `(let ((,index -1))
     (block this-loop! (loop for ,value across ,a-vector
                             do (progn (incf ,index)
                                       (block this-pass! ,@body))))))

(defmacro for-each-hash ((index key value a-hash) &body body)
  `(let ((,index -1))
     (block this-loop!
            (loop for ,key being the hash-keys of ,a-hash
                  do (progn (incf ,index)
                            (setf ,value (gethash ,key ,a-hash))
                            (block this-pass! ,@body))))))

(defmacro for-each-alist ((index key value aalist) &body body)
  (let ((tmp (gensym)))
    `(let ((,index -1))
       (block this-loop!
              (loop for ,tmp in ,aalist
                    do (progn
                         (incf ,index)
                         (setq ,key (car ,tmp))
                         (setq ,value (cdr ,tmp))
                         (block this-pass! ,@body)))))))

(defmacro for-each-stream ((index value astream) &body body)
  (let ((instream   (gensym)))
    `(let* ((,index        -1)
            (,instream    ,astream))
       (block this-loop!
         (loop for ,value = (read-line ,instream nil)
               while ,value do (progn (incf ,index) (block this-pass! ,@body)))
         (close ,instream)))))

(defmacro for-each-line ((index value afilename) &body body)
  (let ((instream   (gensym))
        (resolvedfn (gensym)))
    `(let* ((,index        -1)
            (,resolvedfn  ,afilename)
            (,instream    (open ,resolvedfn :if-does-not-exist :error
                                :external-format *clix-external-format*)))
       (block this-loop!
         (loop for ,value = (read-line ,instream nil)
               while ,value do (progn (incf ,index) (block this-pass! ,@body)))
         (close ,instream)))))

(defmacro for-each-node-set ((index value aset) &body body)
  `(let ((,index -1))
     (block this-loop!
            (xpath:do-node-set (,value ,aset)
              (incf ,index)
              (block this-pass! ,@body)))))


(defmacro for-each->list (athing &body body)
  (with-gensyms (result)
    `(let ((,result '()))
       (flet ((collect! (item) (push item ,result) item))
         (for-each ,athing ,@body))
       (nreverse ,result))))

(defmacro for-each->vector (athing options &body body)
  (destructuring-bind (&key (size 16) (element-type t)) options
    (with-gensyms (result)
      `(let ((,result (make-array ,size :adjustable t :fill-pointer 0
                                  :element-type ,element-type)))
         (flet ((collect! (item) (vector-push-extend item ,result) item))
           (for-each ,athing ,@body))
         ,result))))

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
  `(let ((it! ,test))
     (if it! ,then ,else)))

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

; --------------------------------------------------------------- ;

; cl-ppcre wrappers where the arguments are re-arranged to make sense to me

(defun re-compile (aregex)
  (cl-ppcre:create-scanner aregex))

(declaim (inline str-split))
(defun str-split (astr sep &key (limit))
  "Wrapper around cl-ppcre:split with string first"
  (cl-ppcre:split sep astr :limit limit))

(declaim (inline str-replace))
(defun str-replace (astr from to)
  "Wrapper around cl-ppcre:regex-replace with string first"
  (cl-ppcre:regex-replace from astr to))

(declaim (inline str-replace-all))
(defun str-replace-all (astr from to)
  "Wrapper around cl-ppcre:regex-replace-all with string first"
  (cl-ppcre:regex-replace-all from astr to))

(declaim (inline str-detect))
(defun str-detect (astr pattern)
  "Returns true if `pattern` matches `astr`
   Wrapper around cl-ppcre:scan"
  (if (cl-ppcre:scan pattern astr) t nil))

(declaim (inline str-subset))
(defun str-subset (anlist pattern)
  "Returns all elements that match pattern"
  (let ((ret nil))
    (for-each anlist
      (when (str-detect value! pattern)
        (setq ret (append ret (list value!)))))
    ret))

(declaim (inline str-scan-to-strings))
(defun str-scan-to-strings (astr pattern)
  "Wrapper around cl-ppcre:scan-to-strings with string first
   and only returns the important part (the vector of matches)"
  (multiple-value-bind (dontneed need)
    ; (declaim (ignorable dontneed))
    (cl-ppcre:scan-to-strings pattern astr)
    need))

(declaim (inline ~m))
(defun ~m (astring aregex)
  "Alias to str-detect"
  (str-detect astring aregex))

(declaim (inline ~r))
(defun ~r (astring from to)
  "Alias to str-replace (one"
  (str-replace astring from to))

(declaim (inline ~ra))
(defun ~ra (astring from to)
  "Alias to str-replace-all"
  (str-replace-all astring from to))

(declaim (inline ~s))
(defun ~s (astring aregex &key (limit))
  "Alias to str-split"
  (str-split astring aregex :limit limit))

(declaim (inline ~f))
(defun ~f (alist aregex)
  "Alias to str-subset"
  (str-subset alist aregex))


; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

; very hacky interface to R for emergencies
; because LITERALLY NOTHING ELSE WORKS!

(defun r-get (acommand &key (type *read-default-float-format*) (what :raw))
  "Runs a command through R and returns the result.
    `:type` specifies what parse-float:parse-float should use to parse the result
    `:what` specifies the intended format (:single (atom) :vector, or :raw (default)"
  (let* ((newcom (str-replace-all acommand "'" (format nil "~C" #\QUOTATION_MARK)))
         (result (zsh (format nil "R --silent -e '~A'" newcom))))
    (if (eq what :raw)
      result
      (-<>
        ((lambda (x) x) result)
        (str-split <> "\\n")
        (remove-if-not (lambda (x) (str-detect x "^\\s*\\[\\d+\\]")) <>)
        (str-join "" <>)
        (str-replace-all <> "\\[\\d+\\]" "")
        (str-split <> "\\s+")
        (remove-if (lambda (x) (string= "" x)) <>)
        ((lambda (x)
           (cond
             ((eq what :vector) (mapcar (lambda (y) (parse-float:parse-float y :type type)) x))
             ((eq what :single)
                  (progn
                    (let ((res (mapcar (lambda (y) (parse-float:parse-float y :type type)) x)))
                      (if (> (length res) 1)
                        (error "not vector of length 1")
                        (car res)))))
             (t x))) <>)))))

(defmacro with-r (what &body body)
  "Macro that will take all the strings given in the body and
   run them at once in R. The first argument specifies the
   intended return type (:single :vector :raw)"
  (let ((thecom (gensym)))
    `(let ((,thecom (str-join ";" ',body)))
       (r-get ,thecom :what ,what))))


; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

; alexandria re-exports

(defun alist->hash-table (analist &rest htargs)
  (apply #'alexandria:alist-hash-table (cons analist htargs)))
  ; (alexandria:alist-hash-table analist htargs))

(defun hash-table->alist (analist)
  (alexandria:hash-table-alist analist))

; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

; interesting reader macros

(defun |•-reader| (stream char)
  "Alternate double quote"
  (declare (ignore char))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((char= curr #\Bullet) (push prev chars))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-macro-character #\Bullet #'|•-reader|)


(defun |ensure-not-null| (stream char)
  "Reader macro to check if symbol is null,
   otherwise, pass it on"
  (declare (ignore char))
  (let ((sexp (read stream t)))
    `(progn
       (aif (eval ',sexp)
            it!
            (error "its null")))))

(set-macro-character #\Ø #'|ensure-not-null|)


(defun |if-null->this| (stream char)
  "Reader macro that takes to s-expressions.
   If the first evaluates to not null, it is returned.
   If the first evaluates to null, the second s-expression is returned"
  (declare (ignore char))
  (let ((sexp (read stream t))
        (replacement (read stream t))
        (res  (gensym)))
    `(let ((,res ,sexp))
       (if ,res ,res ,replacement))))

(set-macro-character #\? #'|if-null->this|)


(defun |«-reader| (stream char)
  "Examples:
     « (/ 3 1) or die error! »        ; returns 3
     « (/ 3 0) or warn error! »       ; stderrs error, continues, and returns NIL
     « (/ 3 0) or die error! »        ; dies with error message
     « 3 or die error! »              ; returns 3
     « nil or die error! »            ; dies because atom preceding `or` is NIL"
  (let ((err-mess     "« reader macro not written to specification")
        (ender        "»")
        (before       (read stream))
        (theor        (read stream))
        (theoperator  (read stream))
        (after        (read stream))
        (theend-p     (symbol-name (read stream)))
        (res          (gensym)))
    ; syntax error checking
    (unless (string= theend-p ender) (die err-mess))
    (unless (string= (symbol-name theor) "OR") (die err-mess))
    (cond
      ((consp before)
       (cond
         ((string= "DIE" (symbol-name theoperator))
           `(or-die (,after) ,before))
         ((string= "WARN" (symbol-name theoperator))
           `(or-die (,after :errfun #'advise) ,before))))
      ((atom before)
       (cond
         ((string= "DIE" (symbol-name theoperator))
           `(if ,before ,before (die ,after)))
         ((string= "WARN" (symbol-name theoperator))
           `(if ,before ,before (advise ,after))))))))

(set-macro-character #\« #'|«-reader|)

; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

; more

(defmacro debug-these (avar &rest therest)
  """
  Macro that takes an arbitrary number of arguments,
  prints the symbol, and then prints it's evaluated value
  (for debugging)
  """
  (let ((whole (cons avar therest)))
    `(loop for i in ',whole do (format t "~15S -> ~S~%" i (eval i)))))


(defmacro with-a-file (filename key &body body)
  "Anaphoric macro that binds `stream!` to the stream
   First argument is the filename
   The second argument is one of
     `:w` - write to a file  (clobber if already exists)
     `:a` - append to a file (create if doesn't exist)
     `:r` - read a file      (in text mode)
     `:b` - read a file      (in binary mode [unsigned-byte 8])
    Only provide one of these arguments"
   (let ((dir (cond
                ((eq key :w) :output)       ((eq key :a) :output)
                ((eq key :r) :input)        ((eq key :b) :input)))
         (iex (cond
                ((eq key :w) :supersede)    ((eq key :a) :append)
                ((eq key :r) :append)       ((eq key :b) :append))))
    `(with-open-file (stream! ,filename :direction ,dir :if-exists ,iex
                              ,@(when (eq key :b)
                                  `(':element-type 'unsigned-byte))
                              :if-does-not-exist :create
                              :external-format *clix-external-format*)
       ,@body)))


(defun rnorm (n &key (mean 0) (sd 1))
  "Makes a list of `n` random variates with mean of `mean` and
   standard deviation of `sd`"
  (loop for i from 1 to n collect (+ mean (* sd (alexandria:gaussian-random)))))


(defun delim (anlist &key (what :list) (sep #\Tab))
  "Makes a string with tabs separating values.
   `:what` either :list :listoflist :hash or :alist
   `:sep` the (CHARACTER) separator to use (default is tab)"
  (labels ((join-with-sep      (x) (str-join (format nil "~C" sep) x))
           (join-with-newlines (x) (str-join (format nil "~C" #\Newline) x)))
    (cond
      ((eq :list what)   (str-join (format nil "~C" sep) anlist))
      ((eq :alist what)  (join-with-newlines (loop for i in anlist
                                                   for key = (car i)
                                                   for val = (cdr i)
                                                   collect (join-with-sep (list key val)))))
      ((eq :listoflists what)
                         (join-with-newlines (loop for i in anlist
                                                   collect (join-with-sep i))))
      ((eq :hash what)   (join-with-newlines (loop for key being the hash-keys in anlist
                                                   using (hash-value val)
                                                   collect (join-with-sep (list key val)))))
      (t                 (error "unsupported type")))))


(defmacro defparams (&body body)
  "Declares the arguments to by special defparameter parameters
   with a value on `nil`"
  (labels ((helper (alist)
              (loop for i in alist collect `(defparameter ,i nil))))
    (let ((tmp (helper body)))
     `(progn  ,@tmp))))


(defmacro if->then (&body body)
  "Example:
    (if->then
      (string= *character* •cosmo•)    ->    •kramer•
      (string= *character* •jerry•)    ->    •seinfeld•
      (string= *character* •elaine•)   ->    •benes•
      (string= *character* •george•)   ->    •costanza•)"
  (labels ((group-them (alist)
    (unless (null alist)
      (cons `(,(car alist) ,(caddr alist))
            (group-them (cdddr alist))))))
  (let ((in-3s (group-them body)))
    `(cond ,@(group-them body)))))


(defmacro if-this->then (athing atest adefault &body body)
  "Similar to `if-then` but takes a thing to test, a predicate function,
   and what to return if all else fails
  Example:
    (if-this->then *character* #'string= nil
      •cosmo•   ->  •kramer•
      •george•  ->  •constanza•
      •elaine•  ->  •benes•
      •jerry•   -> •seinfeld•)"
  (with-gensyms (thething thetest thedefault)
    (labels ((group-them (alist)
      (unless (null alist)
        (cons `((funcall ,thetest ,(car alist) ,thething) ,(caddr alist))
              (group-them (cdddr alist))))))
    (let ((in-3s (group-them body)))
      `(let ((,thething ,athing)
             (,thetest  ,atest)
             (,thedefault ,adefault))
         (cond ,@(group-them body)
               (t            ,thedefault)))))))






