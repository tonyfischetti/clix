;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;;      Personal common lisp utilities              ;;
;;                                                  ;;
;;              Tony Fischetti                      ;;
;;              tony.fischetti@gmail.com            ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :clix
  (:use :common-lisp)
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
           :alambda           :self!              :get-size
           :slurp             :barf               :round-to
           :with-hash-entry   :if-hash-entry      :if-not-hash-entry
           :entry!            :die                :or-die
           :or-do             :die-if-null        :advise
           :error!            :alistp             :cmdargs
           :clear-screen      :-<>                :<>
           :zsh               :universal->unix-time
           :unix->universal-time                  :get-unix-time
           :make-pretty-time  :get-current-time   :with-time
           :time-for-humans    :time!             :progress
           :break!            :continue!          :index!
           :value!            :key!
           :for-each/line     :for-each/list      :for-each/hash
           :for-each/vector   :for-each/stream    :for-each/alist
           :for-each/call     :for-each           :forever
           :eval-always       :abbr               :str-join
           :substr            :interpose          :print-hash-table
           :re-compile        :str-split          :str-replace
           :str-replace-all   :str-detect         :str-subset
           :str-extract
           :str-scan-to-strings                   :str-trim
           :~m                :~r                 :~ra
           :~s                :~f                 :~c
           :~e
           :debug-these       :with-a-file        :stream!
           :rnorm             :delim              :defparams
           :request           :parse-xml          :parse-xml-file
           :xpath             :xpath-compile      :use-xml-namespace
           :xpath-string      :alist->hash-table  :hash-table->alist
           :hash-keys         :parse-json         :parse-json-file
           :export-json       :λ                  :string->octets
           :octets->string    :make-octet-vector  :concat-octet-vector
           :parse-html        :$$                 :r-get
           :with-r            :parse-float        :get-terminal-columns
           :+ansi-escape-up+  :+ansi-escape-left-all+
           :make-ansi-escape  :ansi-clear-line    :ansi-up-line
           :ansi-left-all     :+ansi-escape-left-one+
           :ansi-left-one     :progress-bar       :with-loading
           :flatten           :take               :group
           :mkstr             :create-symbol      :create-keyword
           :walk-replace-sexp :give-choices       :slurp-lines))

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
(defparameter *clix-curly-test* #'equal)

(defparameter *clix-external-format* :UTF-8)


(defun make-ansi-escape (anum &optional (decoration 'bold))
  (format nil "~c[~A~Am" #\ESC anum (cond
                                      ((eq decoration 'bold)        ";1")
                                      ((eq decoration 'underline)   ";4")
                                      ((eq decoration 'reversed)    ";7")
                                      (t                            ""))))

(defvar +reset-terminal-color+  (make-ansi-escape 0 nil))
(defvar +magenta-bold+          (make-ansi-escape 35))
(defvar +red-bold+              (make-ansi-escape 31))
(defvar +yellow-bold+           (make-ansi-escape 33))
(defvar +green-bold+            (make-ansi-escape 32))
(defvar +cyan-bold+             (make-ansi-escape 36))
(defvar +blue-bold+             (make-ansi-escape 34))

(defvar +ansi-escape-up+        (format nil "~c[1A" #\ESC))
(defvar +ansi-escape-left-all+  (format nil "~c[500D" #\ESC))
(defvar +ansi-escape-left-one+ (format nil "~c[1D" #\ESC))

(defun do-with-color (acolor thestring &rest everything)
  (apply #'format nil
         (format nil "~A~A~A" acolor thestring +reset-terminal-color+)
         everything))

(defun green  (thestring &rest things) (apply #'do-with-color +green-bold+ thestring things))
(defun red    (thestring &rest things) (apply #'do-with-color +red-bold+ thestring things))
(defun yellow (thestring &rest things) (apply #'do-with-color +yellow-bold+ thestring things))
(defun cyan   (thestring &rest things) (apply #'do-with-color +cyan-bold+ thestring things))



(defparameter *clix-zsh* "/usr/local/bin/zsh")

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

;---------------------------------------------------------;


;---------------------------------------------------------;
; Some utilities

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

(defmacro abbr (short long)
  `(defmacro ,short (&rest everything)
     `(,',long ,@everything)))

(defun flatten (alist)
  " Flattens a list (possibly inefficiently)"
  (if alist
    (if (listp (car alist))
      (append (flatten (car alist)) (flatten (cdr alist)))
      (cons (car alist) (flatten (cdr alist))))))

(defun take (alist n &optional (acc nil))
  "Takes `n` from beginning of `alist` and returns that in a
   list. It also returns the remainder of the list (use
   `multiple-value-bind` with it"
  (when (and (> n 0) (null alist)) (error "not enough to take"))
  (if (= n 0)
    (values (nreverse acc) alist)
    (take (cdr alist) (- n 1) (cons (car alist) acc))))

(defun group (alist &optional (n 2) (acc nil))
  "Turn a (flat) list into a list of lists of length `n`"
  (if (null alist)
    (nreverse acc)
    (multiple-value-bind (eins zwei) (take alist n)
      (group zwei n (cons eins acc)))))

; stolen from "Let Over Lambda"
(defun mkstr (&rest args)
  "PRINCs `args` into a string and returns it"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun create-symbol (&rest args)
  "Interns an UP-cased string as a symbol. Uses `mkstr` to
   make a string out of all of the `args`"
  (values (intern (string-upcase (apply #'mkstr args)))))

(defun create-keyword (&rest args)
  "Interns an UP-cased string as a keyword symbol. Uses `mkstr` to
   make a string out of all of the `args`"
  (values (intern (string-upcase (apply #'mkstr args)) :keyword)))

(defun walk-replace-sexp (alist oldform newform &key (test #'equal))
  "Walks sexpression substituting `oldform` for `newform`.
   It works with lists and well as atoms. Checks equality with `test`
   (which is #'EQUAL by default)"
  (if alist
    (let ((thecar (car alist)))
      (if (listp thecar)
        (if (tree-equal thecar oldform :test test)
          (cons newform (walk-replace-sexp (cdr alist) oldform newform :test test))
          (cons (walk-replace-sexp thecar oldform newform :test test)
                (walk-replace-sexp (cdr alist) oldform newform :test test)))
        (let ((rplment (if (funcall test thecar oldform) newform thecar)))
          (cons rplment (walk-replace-sexp (cdr alist) oldform newform :test test)))))))

; ------------------------------------------------------- ;

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



; ;---------------------------------------------------------;
; for-each and friends
(declaim (inline progress))
(defun progress (index limit &key (interval 1) (where *error-output*) (newline-p t))
  (when (= 0 (mod index interval))
    (format where (yellow "~A of ~A..... [~$%]"
                          index limit (* 100 (/ index limit))))
    (when newline-p (format where "~%"))))

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
      (#+sbcl sb-sys:interactive-interrupt
       #+ecl  ext:interactive-interrupt
        ()
         (die "~%Loop aborted. Bailing out.~%")))))


(defmacro for-each/list (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((the-list         (gensym)))
    `(handler-case
      (let ((index!       0)
             (value!      nil)
             (,the-list   ,a-thing))
         (declare (ignorable value!))
         (block this-loop!
                (dolist (value! ,the-list)
                  (incf index!)
                  (block this-pass! ,@body))))
      (#+sbcl sb-sys:interactive-interrupt
       #+ecl  ext:interactive-interrupt
        ()
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
      (#+sbcl sb-sys:interactive-interrupt
       #+ecl  ext:interactive-interrupt
        ()
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
      (#+sbcl sb-sys:interactive-interrupt
       #+ecl  ext:interactive-interrupt
        ()
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
      (#+sbcl sb-sys:interactive-interrupt
       #+ecl  ext:interactive-interrupt
        ()
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
      (#+sbcl sb-sys:interactive-interrupt
       #+ecl  ext:interactive-interrupt
        ()
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
      (#+sbcl sb-sys:interactive-interrupt
       #+ecl  ext:interactive-interrupt
        ()
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
    (#+sbcl sb-sys:interactive-interrupt
     #+ecl  ext:interactive-interrupt
      ()
       (die "~%Loop aborted. Bailing out.~%"))))

;---------------------------------------------------------;



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

(defun str-subset (anlist pattern)
  "Returns all elements that match pattern"
  (remove-if-not (lambda (x) (str-detect x pattern)) anlist))

(defun str-extract (astr pattern)
  "If one match, it returns the register group as a string.
   If more than one match/register-group, returns a list
   of register groups. If there is a match but no register
   group, it will still return nil"
  (multiple-value-bind (dontneed need)
    (cl-ppcre:scan-to-strings pattern astr)
    (let ((ret (coerce need 'list)))
      (if (= (length ret) 1)
        (car ret) ret))))

(defun str-scan-to-strings (astr pattern)
  "Wrapper around cl-ppcre:scan-to-strings with string first
   and only returns the important part (the vector of matches)"
  (multiple-value-bind (dontneed need)
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

(defmacro ~e (&rest everything)
  "Alias to str-extract"
  `(str-extract ,@everything))


; --------------------------------------------------------------- ;


;---------------------------------------------------------;
; convenience

(defmacro aif (test then &optional else)
  "Like IF. IT is bound to TEST."
  `(let ((it! ,test))
     (if it! ,then ,else)))


; IS IT MAYBE BIGGER THAN IT NEEDS TO BE BECAUSE MULTIBYTE?
(defun slurp (path)
  "Reads file at PATH into a single string"
  (with-open-file (stream path :if-does-not-exist :error)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun slurp-lines (afilename)
  "Reads lines of a file into a list"
  (with-open-file (tmp afilename :if-does-not-exist :error
                       :external-format *clix-external-format*)
    (loop for value = (read-line tmp nil)
          while value collect value)))

(defun barf (path contents &key (printfn #'write-string) (overwrite nil))
  "Outputs CONTENTS into filename PATH with function PRINTFN
   (default WRITE-STRING) and appends by default (controllable by
   by boolean OVERWRITE)"
  (with-open-file (stream path :direction :output
                          :if-exists (if overwrite :supersede :append)
                          :if-does-not-exist :create)
    (funcall printfn contents stream)))


#+sbcl
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
             (theprocess  (sb-ext:run-program *clix-zsh* `("-c" ,acommand)
                                              :input in
                                              :output outs
                                              :error  errs
                                              :external-format enc))
             (retcode     (sb-ext:process-exit-code theprocess)))
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


(defun round-to (number precision &optional (what #'round))
  "Round `number` to `precision` decimal places. Stolen from somewhere"
  (let ((div (expt 10 precision)))
    (float (/ (funcall what (* number div)) div))))


(defmacro with-hash-entry ((ahash akey) &body body)
  "Establishes a lexical environment for referring to the _value_ of
   key `akey` on the hash table `ahash` using the anaphor `entry!`.
   So, you can setf `entry!` and the hash-table (for that key) will
   by modified."
  (with-gensyms (thehash thekey)
    `(let ((,thehash ,ahash)
           (,thekey  ,akey))
       (symbol-macrolet
         ((entry! (gethash ,thekey ,thehash)))
         ,@body))))

(defmacro if-hash-entry ((ahash akey) then &optional else)
  "Executes `then` if there's a key `akey` in hash-table `ahash` and
   `else` (optional) if not. For convenience, an anaphor `entry!` is
   introduced that is setf-able."
  (with-gensyms (thehash thekey)
    `(let ((,thehash ,ahash)
           (,thekey  ,akey))
       (with-hash-entry (,thehash ,thekey)
         (if entry! ,then ,else)))))

; GOTTA BE A BETTER WAY!
(defmacro if-not-hash-entry ((ahash akey) then &optional else)
  "Executes `then` if there is _NOT_ key `akey` in hash-table `ahash` and
   `else` (optional) if exists. For convenience, an anaphor `entry!` is
   introduced that is setf-able."
  (with-gensyms (thehash thekey)
    `(let ((,thehash ,ahash)
           (,thekey  ,akey))
       (with-hash-entry (,thehash ,thekey)
         (if (not entry!) ,then ,else)))))


(defun advise (message &key (yellow-p t))
  "Prints MESSAGE to *ERROR-OUTPUT* but resumes
   (for use with OR-DIE's ERRFUN)"
   (format *error-output* "~A~A~A~%" (if yellow-p +yellow-bold+ "")
                                     message
                                     (if yellow-p +reset-terminal-color+ "")))


(defun alistp (something)
  "Test is something is an alist"
  (and (listp something)
       (every #'consp something)))


(defun cmdargs ()
  "A multi-implementation function to return argv (program name is CAR)"
  (or
   #+CLISP (cons "program_name" *args*)
   #+SBCL sb-ext:*posix-argv*
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


;---------------------------------------------------------;
; Stolen or inspired by https://github.com/vseloved/rutils/

(defmacro eval-always (&body body)
  "Wrap BODY in eval-when with all keys (compile, load and execute) mentioned."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


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
  (let ((*print-pretty*	t)
        (i              0))
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
; HTML/XML stuff

(defmacro request (&rest everything)
  `(drakma:http-request ,@everything))

(defun parse-xml (astring)
  (cxml:parse astring (cxml-dom:make-dom-builder)))

(defun parse-xml-file (afile)
  (cxml:parse-file afile (cxml-dom:make-dom-builder)))

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

; (defmethod get-at ((this simple-vector) that)
;   (svref this that))

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





; --------------------------------------------------------------- ;
; useful macros / functions
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


(defun get-terminal-columns ()
  "Retrieves the number of columns in terminal by querying
   `$COLUMNS` environment variable. Returns
   (values num-of-columns t) if successful and (values 200 nil)
   if not"
  (let ((raw-res (ignore-errors (parse-integer (zsh "echo $COLUMNS")))))
    (if raw-res (values raw-res t) (values 200 nil))))

(defun ansi-up-line (&optional (where *error-output*))
  (format where "~A" +ansi-escape-up+))

(defun ansi-left-all (&optional (where *error-output*))
  (format where "~A" +ansi-escape-left-all+))

(defun ansi-clear-line (&optional (where *error-output*))
  (ansi-left-all)
  (format where "~A" (make-string (get-terminal-columns) :initial-element #\Space)))

(defun ansi-left-one (&optional (where *error-output*))
  (format where "~A" +ansi-escape-left-one+))


(defun progress-bar (index limit &key (interval 1)
                                      (where *error-output*)
                                      (width 50)
                                      (one-line t)
                                      (out-of nil))
  (when (or (= index limit) (and (= 0 (mod index interval))))
    (let* ((perc-done (/ index limit))
           (filled    (round (* perc-done width))))
      (when one-line
        (ansi-up-line     where)
        (ansi-clear-line  where)
        (ansi-left-all    where))
      (format where (yellow "~&|~A~A| ~$%~A"
              (make-string filled :initial-element #\=)
              (make-string (max 0 (- width filled)) :initial-element #\Space)
              (float (* 100 perc-done))
              (if out-of (fn "~C~A/~A" #\Tab index limit) "")))
      (force-output where))))


(defun loading-forever ()
  (let ((counter -1))
    (forever
      (incf counter)
      (setq counter (mod counter 4))
      (let ((rune (case counter
                    (0  "-")
                    (1  "\\")
                    (2  "|")
                    (t  "/"))))
        (format t "~A" rune)
        (force-output)
        (ansi-left-one *standard-output*)
        (sleep 0.1)))))


(defmacro with-loading (&body body)
  "This function runs `body` in a separate thread
   and also starts a thread that displays a spinner.
   When the `body` thread finishes, it kills the
   spinner thread. Here's an example....
   ```
    (for-each `(heaven or las vegas)
      (ft •processing: ~10A~C• value! #\Tab)
      (with-loading
        (sleep 3)))
    ```
    its particularly neat combined with
    ```
    (progress index! 5 :newline-p nil :where *standard-output*)
    (ft •~C• #\Tab #\Tab)
    ``` "
  (with-gensyms (tmp long-thread loading-thread wrapper the-return)
    `(progn
       (defun ,tmp ()
         ,@body)
       (let ((,long-thread      (bt:make-thread #',tmp
                                                :name "long-thread"))
             (,loading-thread   (bt:make-thread #'loading-forever
                                                :name "loading-thread")))
         (setq ,the-return (bt:join-thread ,long-thread))
         (bt:destroy-thread ,loading-thread)
         (terpri)
         ,the-return))))


(defun give-choices (choices &key (limit 37)
                                  (num-p nil)
                                  (mode :table)
                                  (sep nil))
  "Uses `smenu` (must be installed) to give the user some choices in
   a list (princs the elements). The user's choice(s) are returned
   unless they Control-C, in which case it return `nil`. You can also
   use '/' to search through the choices!
   It's (smenu) is very flexible and this function offers a lot
   of optional keyword parameters
   `limit` sets the limit of choices (and presents a scroll bar)
   `num-p` if true, puts a number next to the choices for easy
           selection (default nil)
   `mode` :table (default), :columns, :lines, and nil
   `sep` if not nil, it will allow the user to select multiple choices (with
         't') and this string will separate them all"
  (let ((tmpvar   (fn "tmp~A"   (get-unix-time)))
        (xchoice  (fn "'~A'"    (str-join "'\\n'" choices)))
        (xmode    (case mode
                    (:columns   "-c")
                    (:table     "-t")
                    (:lines     "-l")
                    (otherwise  ""))))
    (let ((response
            (zsh (fn "~A=$(echo -e \"~A\" | smenu ~A -n~A ~A ~A); echo $~A"
                     tmpvar xchoice (if num-p "-N" "")
                     limit xmode
                     (if sep (fn "-T '~A'" sep) "")
                     tmpvar) :echo nil)))
      (if (string= response "") nil response))))

; --------------------------------------------------------------- ;




; --------------------------------------------------------------- ;
; other abbreviations and shortcuts

(abbr alist->hash-table alexandria:alist-hash-table)
(abbr hash-table->alist alexandria:hash-table-alist)
(abbr hash-keys alexandria:hash-table-keys)
(abbr parse-json yason:parse)
(abbr export-json yason:encode)

(defun parse-json-file (afile)
  (with-a-file afile :r
    (yason:parse stream!)))


(defmacro λ (&body body)
  `(lambda ,@body))

#+sbcl  (defmacro octets->string (&rest everything)
          `(sb-ext:octets-to-string ,@everything))

#+sbcl  (defmacro string->octets (&rest everything)
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

; REALLY LOOK INTO THIS BECAUSE THERE ARE A LOT OF WARNINGS AND IT SUCKS
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


; REALLY LOOK INTO THIS BECAUSE THERE ARE A LOT OF WARNINGS AND IT SUCKS
; (defun clix-log-just-echo (stream char arg)
;   ;;;;;; HOW UNHYGENIC IS THIS???!!
;   (declare (ignore char))
;   (let ((sexp               (read stream t))
;     ;       (thetime            (get-universal-time))
;     ;       (thereturnvalue     nil)
;     ;       (thetimingresults   nil))
;       `(progn
;          (with-a-file *clix-log-file* :a
;            (format t "~S~%" ',sexp)
;            (format stream! "~%λ ~S~%" ',sexp)
;            (let* ((daoutputstream   (make-string-output-stream))
;                   (*trace-output*   daoutputstream)
;                   (thereturnvalue   (progn (time ,sexp))))
;              (finish-output stream!)
;              ,thereturnvalue))))))


(defun clix-log (stream char arg)
  (cond ((= *clix-log-level* 2)    (clix-log-verbose   stream char arg))
        ; ((= *clix-log-level* 1)    (clix-log-just-echo stream char arg))
        (                          nil)))


(set-dispatch-macro-character #\# #\! #'clix-log)

; --------------------------------------------------------------- ;



; --------------------------------------------------------------- ;
; very hacky "interface" to R for emergencies
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

