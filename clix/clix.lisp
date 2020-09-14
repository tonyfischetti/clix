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
  (:import-from :parse-float :parse-float)
  (:export :fn                :ft                 :info
           :*clix-output-stream*
           :*clix-log-level*  :*clix-curly-test*  :*clix-external-format*
           :*clix-log-file*   :*whitespaces*
           :+red-bold+        :+green-bold+       :+yellow-bold+
           :+blue-bold+       :+magenta-bold+     :+cyan-bold+
           :+reset-terminal-color+                :green
           :red               :yellow             :cyan
           :*clix-zsh*        :with-gensyms       :mac
           :nil!              :aif                :it!
           :alambda           :self!
           :slurp             :barf               :get-size
           :die               :or-die             :or-do
           :die-if-null
           :advise            :error!
           :set-hash          :get-hash           :rem-hash
           :alistp            :set-alist          :get-alist
           :rem-alist         :cmdargs            :clear-screen
           :-<>               :<>                 :zsh
           :universal->unix-time                  :unix->universal-time
           :get-unix-time     :make-pretty-time   :get-current-time
           :with-time         :time-for-humans    :time!
           :progress          :break!             :continue!
           :index!            :value!             :key!
           :for-each/line     :for-each/list      :for-each/hash
           :for-each/vector   :for-each/stream    :for-each/alist
           :for-each/call     :for-each           :forever
           :eval-always       :abbr               :str-join
           :substr            :interpose          :print-hash-table
           :re-compile        :str-split          :str-replace
           :str-replace-all   :str-detect         :str-subset
           :str-scan-to-strings                   :str-trim
           :~m
           :~r                :~ra                :~s
           :~f                :~c
           :debug-these       :with-a-file        :stream!
           :rnorm             :delim              :defparams
           :if->then          :if-this->then      :request
           :parse-xml         :xpath
           :xpath-compile      :use-xml-namespace
           :xpath-string
           :alist->hash-table :hash-table->alist  :hash-keys
           :parse-json        :export-json        :λ
           :string->octets    :octets->string     :make-octet-vector
           :concat-octet-vector
           :parse-html        :$$
           :r-get             :with-r             :parse-float))

(in-package :clix)

(pushnew :clix *features*)


;---------------------------------------------------------;
; formatting

(defmacro fn (&rest everything)
  `(format nil ,@everything))

(defmacro ft (&rest everything)
  `(format t ,@everything))

(defmacro info (&rest everything)
  `(format *error-output* (green ,@everything)))

;---------------------------------------------------------;


;---------------------------------------------------------;
; parameters

(defparameter *clix-output-stream* *terminal-io*)
(defparameter *clix-log-level* 2)
(defparameter *clix-log-file* "clix-log.out")
(defparameter *clix-curly-test* #'eq)

(defparameter *clix-external-format* :UTF-8)

(defvar +red-bold+              (format nil "~c[31;1m" #\ESC))
(defvar +green-bold+            (format nil "~c[32;1m" #\ESC))
(defvar +yellow-bold+           (format nil "~c[33;1m" #\ESC))
(defvar +blue-bold+             (format nil "~c[34;1m" #\ESC))
(defvar +magenta-bold+          (format nil "~c[35;1m" #\ESC))
(defvar +cyan-bold+             (format nil "~c[36;1m" #\ESC))
(defvar +reset-terminal-color+  (format nil "~c[0m"    #\ESC))

(defmacro green   (&rest everything) `(fn "~A~A~A" +green-bold+   (fn ,@everything) +reset-terminal-color+))
(defmacro red     (&rest everything) `(fn "~A~A~A" +red-bold+     (fn ,@everything) +reset-terminal-color+))
(defmacro yellow  (&rest everything) `(fn "~A~A~A" +yellow-bold+  (fn ,@everything) +reset-terminal-color+))
(defmacro cyan    (&rest everything) `(fn "~A~A~A" +cyan-bold+    (fn ,@everything) +reset-terminal-color+))

(defparameter *clix-zsh* "/usr/local/bin/zsh")

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

;---------------------------------------------------------;


;---------------------------------------------------------;
; Stolen from "Practical Common Lisp"
(defmacro with-gensyms ((&rest names) &body body)
  "Why mess with the classics"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

; Stolen from "On Lisp"
(defmacro mac (sexp)
  "Let's you do `(mac (anunquotesmacro))`"
  `(pprint (macroexpand-1 ',sexp)))

; Adapted from "On Lisp"
(defmacro nil! (&rest rest)
  "Sets all the arguments to nil"
  (let ((tmp (mapcar (lambda (x) `(setf ,x nil)) rest)))
    `(progn ,@tmp)))

; I forgot where I stole this from
(defmacro alambda (params &body body)
  "Anaphoric lambda. SELF! is the function"
  `(labels ((self! ,params ,@body))
     #'self!))

; ------------------------------------------------------- ;

; --------------------------------------------------------------- ;
; cl-ppcre wrappers where the arguments are re-arranged to make sense to me

(defmacro re-compile (&rest everything)
  `(cl-ppcre:create-scanner ,@everything))

(defmacro str-split (astr sep &rest everything)
  "Wrapper around cl-ppcre:split with string first"
  `(cl-ppcre:split ,sep ,astr ,@everything))

(defmacro str-replace (astr from to &rest everything)
  "Wrapper around cl-ppcre:regex-replace with string first"
  `(cl-ppcre:regex-replace ,from ,astr ,to ,@everything))

(defmacro str-replace-all (astr from to &rest everything)
  "Wrapper around cl-ppcre:regex-replace-all with string first"
  `(cl-ppcre:regex-replace-all ,from ,astr ,to ,@everything))

(defmacro str-detect (astr pattern &rest everything)
  "Returns true if `pattern` matches `astr`
   Wrapper around cl-ppcre:scan"
  `(if (cl-ppcre:scan ,pattern ,astr ,@everything) t nil))

(declaim (inline str-subset))
(defun str-subset (anlist pattern)
  "Returns all elements that match pattern"
  (let ((ret nil))
    (for-each/list anlist
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

(defun str-trim (astring)
  (string-trim *whitespaces* astring))

(defmacro ~m (&rest everything)
  "Alias to str-detect"
  `(str-detect ,@everything))

(defmacro ~r (&rest everything)
  "Alias to str-replace (one)"
  `(str-replace ,@everything))

(defmacro ~ra (&rest everything)
  "Alias to str-replace-all"
  `(str-replace-all ,@everything))

(defmacro ~s (&rest everything)
  "Alias to str-split"
  `(str-split ,@everything))

(defmacro ~f (&rest everything)
  "Alias to str-subset"
  `(str-subset ,@everything))

(defmacro ~c (&rest everything)
  "Alias to re-compile"
  `(re-compile ,@everything))

; --------------------------------------------------------------- ;


;---------------------------------------------------------;
; convenience


(defmacro aif (test then &optional else)
  "Like IF. IT is bound to TEST."
  `(let ((it! ,test))
     (if it! ,then ,else)))

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


(defun zsh (acommand &key (dry-run nil)
                          (err-fun #'(lambda (code stderr) (error (format nil "~A (~A)" stderr code))))
                          (echo nil)
                          (enc *clix-external-format*)
                          (in  t)
                          (return-string t)
                          (split nil))
  "Runs command `acommand` through the ZSH shell specified by the global *clix-zsh*
   `dry-run` just prints the command (default nil)
   `err-fun` takes a function that takes an error code and the STDERR output
   `echo` will print the command before running it
   `enc` takes a format (default is *clix-external-format* [which is :UTF-8 by default])
   `in` t is inherited STDIN. nil is /dev/null. (default t)
   `return-string` t returns the output string. nil inherits stdout (default t)
   `split` will separate the stdout by newlines and return a list (default: nil)"
  (flet ((strip (astring)
    (if (string= "" astring)
      astring
      (subseq astring 0 (- (length astring) 1)))))
    (when (or echo dry-run)
      (format t "$ ~A~%" acommand))
    (unless dry-run
      (let* ((outs        (if return-string (make-string-output-stream) t))
             (errs        (make-string-output-stream))
             (theprocess  (run-program *clix-zsh* `("-c" ,acommand)
                                       :input in
                                       :output outs
                                       :error  errs
                                       :external-format enc))
             (retcode     (process-exit-code theprocess)))
        (when (> retcode 0)
          (funcall err-fun retcode (strip (get-output-stream-string errs))))
        (when return-string
          (values (if split
                    (~s (get-output-stream-string outs) "\\n")
                    (strip (get-output-stream-string outs)))
                  (strip (get-output-stream-string errs))
                  retcode))))))


(defun get-size (afile &key (just-bytes nil))
  "Uses `du` to return just the size of the provided file.
   `just-bytes` ensures that the size is only counted in bytes (returns integer) [default nil]"
  (let ((result   (~r (zsh (format nil "du ~A '~A'" (if just-bytes "-sb" "") afile)) "\\s+.*$" "")))
    (if just-bytes (parse-integer result) result)))


(defun die (message &key (status 1) (red-p t))
  "Prints MESSAGE to *ERROR-OUTPUT* and quits with a STATUS (default 1)"
  (format *error-output* "~A~A~A~%" (if red-p +red-bold+ "")
                                      (fn message)
                                    (if red-p +reset-terminal-color+ ""))
  #+clisp (ext:exit status)
  #+sbcl  (sb-ext:quit :unix-status status))


(defmacro or-die ((message &key (errfun #'die)) &body body)
  "anaphoric macro that binds ERROR! to the error
   It takes a MESSAGE with can include ERROR! (via
   (format nil...) for example) It also takes ERRFUN
   which it will FUNCALL with the MESSAGE. The default
   is to DIE, but you can, for example, PRINC instead"
  `(handler-case
     (progn
       ,@body)
     (error (error!)
       (funcall ,errfun (format nil "~A" ,message)))))


(defmacro or-do (orthis &body body)
  "anaphoric macro that binds ERROR! to the error.
   If the body fails, the form ORTHIS gets run."
  `(handler-case
     (progn
       ,@body)
      (error (error!)
        ,orthis)))

(defmacro die-if-null (avar &rest therest)
  "Macro to check if any of the supplied arguments are null"
  (let ((whole (cons avar therest)))
    `(loop for i in ',whole
           do (unless (eval i) (die (format nil "Fatal error: ~A is null" i))))))

(defun advise (message &key (yellow-p t))
  "Prints MESSAGE to *ERROR-OUTPUT* but resumes
   (for use with OR-DIE's ERRFUN)"
   (format *error-output* "~A~A~A~%" (if yellow-p +yellow-bold+ "")
                                     message
                                     (if yellow-p +reset-terminal-color+ "")))

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

(defun alistp (something)
  "Test is something is an alist"
  (and (listp something)
       (every #'consp something)))

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


; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; experimental reader macros
(defun ignore-the-errors-wrapper (stream char arg)
  (declare (ignore char))
  (declare (ignore arg))
  (let ((sexp (read stream t)))
    `(ignore-errors ,sexp)))

(set-dispatch-macro-character #\# #\? #'ignore-the-errors-wrapper)


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
  "Reader macro that takes two s-expressions.
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
     « nil or die error! »            ; dies because atom preceding `or` is NIL
     « 3 or do (format t •no~%•)! »   ; returns 3
     « nil or do (format t •no~%•) »  ; prints 'no'"
  (declare (ignore char))
  (let ((err-mess     "« reader macro not written to specification")
        (ender        "»")
        (before       (read stream))
        (theor        (read stream))
        (theoperator  (read stream))
        (after        (read stream))
        (theend-p     (symbol-name (read stream))))
    ; syntax error checking
    (unless (string= theend-p ender) (die err-mess))
    (unless (string= (symbol-name theor) "OR") (die err-mess))
    (cond
      ((consp before)
       (cond
         ((string= "DIE" (symbol-name theoperator))
           `(or-die (,after) ,before))
         ((string= "WARN" (symbol-name theoperator))
           `(or-die (,after :errfun #'advise) ,before))
         ((string= "DO" (symbol-name theoperator))
           `(or-do ,after ,before))))
      ((atom before)
       (cond
         ((string= "DIE" (symbol-name theoperator))
           `(if ,before ,before (die ,after)))
         ((string= "WARN" (symbol-name theoperator))
           `(if ,before ,before (advise ,after)))
         ((string= "DO" (symbol-name theoperator))
           `(if ,before ,before ,after)))))))

(set-macro-character #\« #'|«-reader|)


; universal indexing operator syntax
(defun |{-reader| (stream char)
  (declare (ignore char))
  (let ((inbetween nil))
    (let ((chars nil))
      (do ((prev (read-char stream) curr)
           (curr (read-char stream) (read-char stream)))
          ((char= curr #\}) (push prev chars))
        (push prev chars))
      (setf inbetween (coerce (nreverse chars) 'string)))
    (let ((leido (read-from-string (fn "(~A)" inbetween))))
      `(clix-get ,@leido))))

(defmethod get-at ((this list) that)
  (cond ((alistp this)        (cdr (assoc that this :test *clix-curly-test*)))
        (t                    (nth that this))))

(defmethod get-at ((this simple-vector) that)
  (svref this that))

(defmethod get-at ((this vector) that)
  (aref this that))

(defmethod get-at ((this hash-table) that)
  (gethash that this))

(defmethod get-at ((this structure-object) that)
  (slot-value this that))

(defmethod get-at ((this standard-object) that)
  (slot-value this that))

(defmethod get-at ((this RUNE-DOM::DOCUMENT) that)
  (xpath this that))

(set-macro-character #\{ #'|{-reader|)

(defun (setf get-at) (new this that)
  (cond
    ((simple-vector-p this)         (setf (svref this that) new))
    ((vectorp this)                 (setf (aref this that) new))
    ((hash-table-p this)            (setf (gethash that this) new))
    ((alistp this)                  (setf (cdr (assoc that this :test *clix-curly-test*)) new))
    ((listp this)                   (setf (nth that this) new))
    ((typep this 'structure-object) (setf (slot-value this that) new))
    ((typep this 'standard-object)  (setf (slot-value this that) new))
    ))

(defmacro suc-apply (afun &rest rest)
  (let ((built      nil)
        (thing      (car rest))
        (thefirst   (cadr rest))
        (therest    (cddr rest)))
    (setq built (reduce (lambda (x y) `(,afun ,x ,y)) therest
                        :initial-value `(,afun ,thing ,thefirst)))
    built))

(defmacro clix-get (x &rest rest)
  `(suc-apply get-at ,x ,@rest))


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

(defun make-pretty-time (a-unix-time &key (just-date nil) (just-time nil) (time-sep ":"))
  "Makes a nicely formatted (YYYY-MM-DD HH?:MM:SS) from a UNIX time
   `just-date` will return just the pretty date
   `just-time` will return just the pretty time
   `time-sep`  will use the supplied character to separate the hours minutes and seconds (default ':')"
  (let ((thisuniversaltime (unix->universal-time a-unix-time)))
    (multiple-value-bind (second minute hour date month year)
      (decode-universal-time thisuniversaltime)
      (if (and (not just-date) (not just-time))
        (format nil "~d-~2,'0d-~2,'0d ~d~A~2,'0d~A~2,'0d" year month date hour TIME-SEP minute TIME-SEP second)
        (if just-date
          (format nil "~d-~2,'0d-~2,'0d" year month date)
          (format nil "~d~A~2,'0d~A~2,'0d" hour TIME-SEP minute TIME-SEP second))))))


(defun get-current-time (&key (just-date nil) (just-time nil) (time-sep ":"))
  "Uses `make-pretty-time` to get the current datetime"
  (make-pretty-time (-<> (get-universal-time) universal->unix-time)
                    :just-date just-date :just-time just-time :time-sep time-sep))


(defmacro with-time (&body aform)
  "Anaphoric macro that executes the car of the body and
   binds the seconds of execution time to TIME!. Then
   all the other forms in the body are executed"
  (let ((began      (gensym))
        (ended      (gensym)))
    `(let ((time! nil))
       (setq ,began (get-universal-time))
       ,(car aform)
       (setq ,ended (get-universal-time))
       (setq time! (- ,ended ,began))
       ,@(cdr aform))))

(defun time-for-humans (seconds)
  "Converts SECONDS into minutes, hours, or days (based on magnitude"
  (cond
    ((> seconds 86400)        (format nil "~$ days" (/ seconds 86400)))
    ((> seconds 3600)         (format nil "~$ hours" (/ seconds 3600)))
    ((> seconds 60)           (format nil "~$ minutes" (/ seconds 60)))
    ((< seconds 60)           (format nil "~A seconds" seconds))))


;---------------------------------------------------------;


;---------------------------------------------------------;
; for-each and friends
(declaim (inline progress))
(defun progress (index limit &key (interval 1) (where *error-output*))
  (when (= 0 (mod index interval))
    (format where (yellow "~A of ~A..... [~$%]~%" index limit (* 100 (/ index limit))))))

(defmacro break! ()
  "For use with `for-each`
   It's short for `(return-from this-loop!"
  `(return-from this-loop!))

(defmacro continue! ()
  "For use with `for-each`
   It's short for `(return-from this-pass!"
  `(return-from this-pass!))

(defmacro for-each/line (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((resolved-fn            (gensym))
        (instream               (gensym)))
    `(handler-case
       (let ((index!            0)
             (value!            nil)
             (,resolved-fn      ,a-thing))
         (with-open-file (,instream ,resolved-fn :if-does-not-exist :error
                                    :external-format *clix-external-format*)
           (block this-loop!
              (loop for value! = (read-line ,instream nil)
                    while value! do (progn (incf index!) (block this-pass! ,@body))))))
       (sb-sys:interactive-interrupt ()
         (die "~%Loop aborted. Bailing out.~%")))))


(defmacro for-each/list (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((the-list         (gensym)))
    `(handler-case
      (let ((index!       0)
             (value!      nil)
             (,the-list   ,a-thing))
         (block this-loop!
                (dolist (value! ,the-list)
                  (incf index!)
                  (block this-pass! ,@body))))
       (sb-sys:interactive-interrupt ()
         (die "~%Loop aborted. Bailing out.~%")))))


(defmacro for-each/hash (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((the-hash         (gensym)))
    `(handler-case
       (let ((index!      0)
             (key!        nil)
             (value!      nil)
             (,the-hash   ,a-thing))
         (block this-loop!
                (loop for key! being the hash-keys of ,the-hash
                      do (progn (incf index!)
                                (setq value! (gethash key! ,the-hash))
                                (block this-pass! ,@body)))))
       (sb-sys:interactive-interrupt ()
         (die "~%Loop aborted. Bailing out.~%")))))


(defmacro for-each/vector (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((the-vector       (gensym)))
    `(handler-case
      (let ((index!       0)
             (value!      nil)
             (,the-vector ,a-thing))
         (block this-loop!
                (loop for value! across ,the-vector
                      do (progn (incf index!) (block this-pass! ,@body)))))
       (sb-sys:interactive-interrupt ()
         (die "~%Loop aborted. Bailing out.~%")))))


; USE UNWIND-PROTECT?
(defmacro for-each/stream (the-stream &body body)
  "(see documentation for `for-each`)"
  (let ((instream               (gensym)))
    `(handler-case
       (let ((index!            0)
             (value!            nil)
             (,instream         ,the-stream))
           (block this-loop!
              (loop for value! = (read-line ,instream nil)
                    while value! do (progn (incf index!) (block this-pass! ,@body)))))
       (sb-sys:interactive-interrupt ()
         (die "~%Loop aborted. Bailing out.~%")))))


(defmacro for-each/alist (aalist &body body)
  "(see documentation for `for-each`)"
  (let ((tmp          (gensym))
        (resolved     (gensym)))
    `(handler-case
      (let ((index!          0)
            (,resolved       ,aalist))
         (block this-loop!
                (loop for ,tmp in ,resolved
                      do (progn
                           (incf index!)
                           (setq key! (car ,tmp))
                           (setq value! (cdr ,tmp))
                           (block this-pass! ,@body)))))
       (sb-sys:interactive-interrupt ()
         (die "~%Loop aborted. Bailing out.~%")))))


(defmacro for-each/call (aclosure &body body)
  "This works like `for-each` (see documentation for it) but
   due to differences, it is not automatically dispatched so
   if always needs to be called explicitly). It's only
   argument (besides the body) is a closure that is repeatedly
   `FUNCALL`ed and terminates when the closure returns NIL"
  `(handler-case
     (let ((index!      0)
           (value!      nil))
       (block this-loop!
              (loop for value! = (funcall ,aclosure)
                    while value!
                    do (progn (incf index!)
                              (block this-pass! ,@body)))))
     (sb-sys:interactive-interrupt ()
         (die "~%Loop aborted. Bailing out.~%"))))


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
     `key!`               (the key of the current hash-table entry [only for hash-tables and alists])
     `value!`             (the value of the current element)
     `this-pass!`         (a block that returning from immediately moves to the next iteration)
     `this-loop!`         (a block that returning from exits the loop)
  For convenience, `(continue!)` and `(break!)` will execute `(return-from this-pass!)`
  and `(return-from this-loop!)`, respectively
  If it's a filename, the external format is *clix-external-format* (:UTF-8 by default)
  Oh, it'll die gracefully if Control-C is used during the loops execution.
  And, finally, for extra performance, you can call it's subordinate functions directly.
  They are... for-each/line, for-each/list, for-each/hash, for-each/vector,
  for-each/stream, and for-each/alist"
  (let ((tmp (gensym)))
    `(let ((,tmp      ,a-thing))
      (cond
        ((and (listp ,tmp) (listp (car ,tmp)) (not (alexandria:proper-list-p (car ,tmp))))
                          (for-each/alist ,tmp ,@body))
        ((and (stringp ,tmp) (cl-fad:file-exists-p ,tmp))
                          (for-each/line ,tmp ,@body))
        (t
          (progn
            (etypecase ,tmp
              (hash-table     (for-each/hash      ,tmp      ,@body))
              (vector         (for-each/vector    ,tmp      ,@body))
              (list           (for-each/list      ,tmp      ,@body))
              (stream         (for-each/stream    ,tmp      ,@body)))))))))


(defmacro forever (&body body)
  "Performed BODY forever. Must be terminated by
   RETURN-FROM NIL, or, simple RETURN
   Simple wrapper around `(loop (progn ,@body))`"
  `(handler-case
     (block nil (loop (progn ,@body)))
     (sb-sys:interactive-interrupt ()
        (die "~%Loop aborted. Bailing out.~%"))))


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



; --------------------------------------------------------------- ;
; useful macros
(defmacro debug-these (&rest therest)
  """
  Macro that takes an arbitrary number of arguments,
  prints the symbol, and then prints it's evaluated value
  (for debugging)
  ; https://www.reddit.com/r/Common_Lisp/comments/d0agxj/question_about_macros_and_lexical_scoping/
  """
  (flet ((debug (this)
      `(format *error-output* "~20S -> ~S~%" ',this ,this)))
    `(progn ,@(mapcar #'debug therest))))


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


; ????
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
      •jerry•   ->  •seinfeld•)"
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

; --------------------------------------------------------------- ;


; --------------------------------------------------------------- ;
; HTML/XML stuff
(abbr request drakma:http-request)

(defmacro request (&rest everything)
  `(drakma:http-request ,@everything))

(defun parse-xml (astring)
  (cxml:parse astring (cxml-dom:make-dom-builder)))

(defun xpath (doc anxpath &key (all t) (compiled-p nil) (text nil))
  (let ((result (if compiled-p
                  (xpath:evaluate-compiled anxpath doc)
                  (xpath:evaluate anxpath doc))))
    (unless (xpath:node-set-empty-p result)
      (if (and all text)
        (mapcar (lambda (x) (xpath:string-value x)) (xpath:all-nodes result))
        (if (and all (not text))
          (xpath:all-nodes result)
          (if (and (not all) text)
            (xpath:string-value result)
            result))))))

(defmacro xpath-compile (&rest everything)
  `(xpath:compile-xpath ,@everything))

(defmacro use-xml-namespace (anns)
  `(setq xpath::*dynamic-namespaces*
         (cons
           (cons nil ,anns)
           xpath::*dynamic-namespaces*)))

(abbr xpath-string xpath:string-value)

; --------------------------------------------------------------- ;


; --------------------------------------------------------------- ;
; other abbreviations and shortcuts

(abbr alist->hash-table alexandria:alist-hash-table)
(abbr hash-table->alist alexandria:hash-table-alist)
(abbr hash-keys alexandria:hash-table-keys)
(abbr parse-json yason:parse)
(abbr export-json yason:encode)


(defmacro λ (&body body)
  `(lambda ,@body))

(defmacro octets->string (&rest everything)
  `(sb-ext:octets-to-string ,@everything))

(defmacro string->octets (&rest everything)
  `(sb-ext:string-to-octets ,@everything))

(defmacro make-octet-vector (n)
  `(make-array ,n :element-type '(unsigned-byte 8)))

(defmacro concat-octet-vector (&rest everything)
  `(concatenate '(vector (unsigned-byte 8)) ,@everything))

(abbr parse-html plump:parse)
(abbr $$ lquery:$)

; (abbr ds-bind destructuring-bind)
; (abbr mv-bind multiple-value-bind)
; (abbr print# print-hash-table)
; (abbr get# gethash (key hashtable &optional default))
; (abbr set# sethash)
; (abbr getset# getsethash)
; (abbr rem# remhash)

; --------------------------------------------------------------- ;



;---------------------------------------------------------;
; Experimental logging and reader macros
(defun prettify-time-output (thetimeoutput)
  (subseq thetimeoutput 0 (- (length thetimeoutput) 4)))

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
         (with-a-file *clix-log-file* :a
           (format stream!
                   "--------------------~%[~A-~A-~A ~2,'0d:~2,'0d:~2,'0d]~%~%FORM:~%~A~%"
                   ,year ,month ,date ,hour ,minute ,second
                   ; (write-to-string ',sexp))
                   (format nil "λ ~S~%" ',sexp))
           (let ((daoutputstream (make-string-output-stream)))
             (let ((*trace-output* daoutputstream))
               (setq thereturnvalue (progn (time ,sexp))))
                 (setq thetimingresults (prettify-time-output (get-output-stream-string daoutputstream))))
           (format stream! "RETURNED:~%~A~%" thereturnvalue)
           (format stream! "~%~A~%--------------------~%~%~%" thetimingresults)
           (finish-output stream!)
           thereturnvalue)))))


(defun clix-log-just-echo (stream char arg)
  ;;;;;; HOW UNHYGENIC IS THIS???!!
  (declare (ignore char))
  (let ((sexp               (read stream t))
    ;       (thetime            (get-universal-time))
    ;       (thereturnvalue     nil)
    ;       (thetimingresults   nil))
      `(progn
         (with-a-file *clix-log-file* :a
           (format t "~S~%" ',sexp)
           (format stream! "~%λ ~S~%" ',sexp)
           (let* ((daoutputstream   (make-string-output-stream))
                  (*trace-output*   daoutputstream)
                  (thereturnvalue   (progn (time ,sexp))))
             (finish-output stream!)
             ,thereturnvalue))))))


(defun clix-log (stream char arg)
  (cond ((= *clix-log-level* 2)    (clix-log-verbose   stream char arg))
        ((= *clix-log-level* 1)    (clix-log-just-echo stream char arg))
        (                          nil)))


(set-dispatch-macro-character #\# #\! #'clix-log)

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

