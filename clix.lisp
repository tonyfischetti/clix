;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;;      Personal common lisp utilities              ;;
;;                                                  ;;
;;              Tony Fischetti                      ;;
;;              tony.fischetti@gmail.com            ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;---------------------------------------------------------;
; IO

(defun slurp (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun barf (path contents &key (printfn #'write-string) (overwrite nil))
  (with-open-file (stream path :direction :output
                          :if-exists (if overwrite :supersede :append)
                          :if-does-not-exist :create)
    (funcall printfn contents stream)))
;---------------------------------------------------------;

;---------------------------------------------------------;
; convenience

(defmacro add-to-hash (the-hash key value)
  `(setf (gethash ,key ,the-hash) ,value))

(defmacro explain (text &rest body)
  `(progn (format t "~A~%" ,text) ,@body))

(defmacro or-die (message &rest body)
  "anaphoric macro that binds err to the error"
  `(handler-case
     (progn
       ,@body)
     (error (err)
       (progn
        (format *error-output* "~A~%" ,message)
        (sb-ext:quit :unix-status 1)))))


(defmacro die-if-null (avar &rest therest)
  `(when (not (and ,avar ,@therest))
     (progn
       (format *error-output* "Error: at least one of the arguments is NIL~%")
       (sb-ext:quit :unix-status 1))))

;---------------------------------------------------------;



;---------------------------------------------------------;
; iterator

(defun gen-size (a-thing)
  (etypecase a-thing
    (vector        (length a-thing))
    (list          (length a-thing))
    (hash-table    (hash-table-count a-thing))))


(defmacro for-each ((index item a-thing &key (progress? nil) (offset "")) &body body)
  (cond
    (progress?
      (let ((len (gensym)))
        `(let ((,len (gen-size ,a-thing)))
           (for-each (,index ,item ,a-thing)
             (when ,progress?
               (format t "~AOn ~A of ~A........... ~,2F%~%"
                       ,offset (+ 1 ,index) ,len (* 100 (/ (+ 1 ,index) ,len))))
             ,@body))))
    (t
      `(etypecase ,a-thing
         (list          (for-each-list   (,index ,item ,a-thing) ,@body))
         (vector        (for-each-vector (,index ,item ,a-thing) ,@body))
         (hash-table    (for-each-hash   (,index ,item ,a-thing) ,@body))))))


(defmacro for-each-list ((index item a-list) &body body)
  `(let ((,index -1)) (dolist (,item ,a-list) (incf ,index) ,@body)))

(defmacro for-each-vector ((index item a-vector) &body body)
  `(let ((,index -1)) (loop for ,item across ,a-vector
                            do (progn (incf ,index) ,@body))))

(defmacro for-each-hash ((index item a-hash) &body body)
  `(let ((,index -1)) (loop for ,item being the hash-keys of ,a-hash
                            do (progn (incf ,index) ,@body))))
;---------------------------------------------------------;


(defun clear ()
   "A multi-implementation function equivalent for the C function system"
   #+clisp (shell "clear")
   #+ecl (si:system "clear")
   #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*)
   #+clozure (ccl:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*))



(defmacro for-each-line ((index line afilename) &body body)
  (let ((instream (gensym)))
    `(let ((,index -1)
           (,instream (open ,afilename :if-does-not-exist nil)))
       (loop for ,line = (read-line ,instream nil)
             while ,line do (progn (incf ,index) ,@body))
       (close ,instream))))


