;; Copyright (c) 2003 Nikodemus Siivola
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :linedit)


(defvar *yellow-bold*   (format nil "~c[33;1m" #\ESC))
(defvar *green-bold*    (format nil "~c[32;1m" #\ESC))
(defvar *cyan-bold*     (format nil "~c[36;1m" #\ESC))
(defvar *reset*         (format nil "~c[0m" #\ESC))



#-sbcl
(error "Attempt to load an SBCL specific file in anothr implementation.")

(let (prompt-fun read-form-fun)
  (declare (type (or null function) prompt-fun read-form-fun))

  (macrolet ((enforce-consistent-state ()
                                       `(invariant (or (and prompt-fun read-form-fun)
                                                       (not (or prompt-fun read-form-fun))))))

    (defun uninstall-repl ()
      "Uninstalls the Linedit REPL, restoring original handlers."
      (enforce-consistent-state)
      (if prompt-fun
        (setf sb-int:*repl-prompt-fun* prompt-fun
              sb-int:*repl-read-form-fun* read-form-fun
              prompt-fun nil
              read-form-fun nil)
        (warn "UNINSTALL-REPL failed: No Linedit REPL present."))
      nil)

    (defun install-repl (&key wrap-current eof-quits)
      "Installs the Linedit at REPL. Original input handlers can be
      preserved with the :WRAP-CURRENT T."
      (enforce-consistent-state)
      (when prompt-fun
        (warn "INSTALL-REPL failed: Linedit REPL already installed.")
        (return-from install-repl nil))
      (setf prompt-fun sb-int:*repl-prompt-fun*
            read-form-fun sb-int:*repl-read-form-fun*)
      (flet ((repl-reader (in out)
                          (declare (type stream out)
                                   (ignore in))
                          ; (fresh-line out)
                          ; (fresh-line out)
                          ; (format t "~%in: ~A~%out: ~A~%" in out)
                          ; (format *terminal-io* "~%")
                          ; (format *terminal-io* "~A" *green-bold*)
                          (format t "~A~A" *reset* *green-bold*)
                          (let ((prompt (with-output-to-string (s)
                                          (funcall prompt-fun s))))
                            (handler-case
                              (linedit:formedit
                                ; :prompt1 (format nil "~c[32;1m~c[1mλ~c[0m "
                                ; :prompt1 (format nil "~Aλ ~A" *yellow-bold* *green-bold*)
                                :prompt1 (format nil "λ ~A" *yellow-bold*)
                                ; :prompt1 (format nil "λ ")
                                ; :prompt1 (format nil "~c[32mλ ~c[0m" #\ESC #\ESC)
                                :prompt2 (format nil "~A+   ~A" *green-bold* *yellow-bold*))
                                ; :prompt2 "+   ") 
                              (end-of-file (e)
                                           (if eof-quits
                                             (and (fresh-line) (eof-handler))
                                             ;; Hackins, I know.
                                             "#.''end-of-file"))))
                          ; (format t "~A" *cyan-bold*)
                          ))
        (setf sb-int:*repl-prompt-fun* (constantly ""))
        (setf sb-int:*repl-read-form-fun*	      
              (if wrap-current

                (lambda (in out)
                  (declare (type stream out in))
                  ;; FIXME: Yich.
                  (terpri)
                  (with-input-from-string (in (meta-escape (repl-reader in out)))
                  ; (with-input-from-string (in (repl-reader in out))
                    (funcall read-form-fun in out)))

                (lambda (in out)
                  (declare (type stream out in))
                  (handler-case (read-from-string (repl-reader in out))
                    (end-of-file () 
                                 ;; We never get here if eof-quits is true, so...
                                 (fresh-line)
                                 (write-line "#<end-of-file>")
                                 (values)))))))
      t)))

(defun eof-handler ()
  (format *terminal-io* "One love~%")
  (finish-output *terminal-io*)
  (end-of-file ()
               (fresh-line)
               (sb-ext:quit)))
