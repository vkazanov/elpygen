;;; elpygen.el --- Implement a function or a method using symbol name and arguments under the point
;;; -*- lexical-binding: t; -*-


;;; Commentary:
;;

(require 'yasnippet)
(require 'python)

(require 'cl)
(require 'subr-x)

;;; Code:

(defconst elpygen-varname-re "[a-zA-Z]\\w*")

(defconst elpygen-funcall-re "[[:alnum:]_.]*[[:space:]]*(")

(defconst elpygen-function-template "def ${1:`fun`}(${2:`args`}):
    ${0:pass}
")

(defconst elpygen-method-template "def ${1:`fun`}(self, ${2:`args`}):
    ${0:pass}
")

(defun elpygen-implement ()
  "Implement a function or a method using the symbol name and
call arguments under the point."
  (interactive)
  (when (python-syntax-comment-or-string-p)
    (user-error "Cannot extract symbols in comments/strings "))
  (when (not (looking-at-p elpygen-funcall-re))
    (user-error "This doesn't look like a function/method call"))
  (if-let (name (elpygen--get-def-name))
      (if (elpygen--symbol-method-p name)
          (elpygen--implement-method name)
        (elpygen--implement-function name))
    (user-error "Failed to find a suitable symbol")))

(defun elpygen--symbol-method-p (symbol-name)
  "Check if a symbol is a method call.
Argument SYMBOL-NAME the name of the symbol to check."
  (string-prefix-p "self." symbol-name t))

(defun elpygen--implement-function (name)
  "Insert a function stub.
Argument NAME the name of the function to insert."
  (let ((arglist (elpygen--get-arglist)))
    (elpygen--prepare-function-insert-point)
    (elpygen--insert-template elpygen-function-template
                              name
                              arglist)))

(defun elpygen--implement-method (name)
  "Insert a method stub into the current class.
Argument NAME is the name of method to insert."
  (unless (elpygen--within-method-p)
    (user-error "Can only implement a method from within a method of a class"))
  (let ((arglist (elpygen--get-arglist)))
    (elpygen--prepare-method-insert-point)
    (elpygen--insert-template elpygen-method-template
                              (seq-subseq name 5)
                              arglist)))

(defun elpygen--within-method-p ()
  "Check if the point is inside a method."
  (when-let ((defun-info (python-info-current-defun))
             (defun-info-parts (split-string defun-info "\\."))
             (typed-defun-info (python-info-current-defun t)))
    ;; Should be a method (i.e. def) and should be within a class, i.e. defun info should contain at
    ;; least two parts, with the first capitalised. Should work most of the time.
    (and (string-prefix-p "def" typed-defun-info)
         (>= (length defun-info-parts) 2))))

(defun elpygen--insert-template (template name arglist)
  "Insert a TEMPLATE into the current buffer.
Argument NAME is the name of the function/method to insert.
Argument ARGLIST is the argument list of the function/method."
  (yas-expand-snippet template nil nil
                      `((fun ,name)
                        (args ,(elpygen--format-args arglist)))))

(defun elpygen--get-def-name ()
  "Retrieve a symbol under the point."
  (with-syntax-table python-dotty-syntax-table
    (when-let ((funname (thing-at-point 'symbol)))
      (substring-no-properties funname))))

(defun elpygen--get-arglist ()
  "Retrieve the argument list of the symbol at point."
  (save-excursion
    (with-syntax-table python-dotty-syntax-table
      (when (symbol-at-point)
        (end-of-thing 'symbol)
        (skip-chars-forward "[:blank:]")
        (when (looking-at-p "(")
          (when-let (sexp (thing-at-point 'sexp))
            (elpygen--parse-arg-str sexp)))))))

(defun elpygen--parse-arg-str (arg-str)
  "Make a list of arguments from ARG-STR."
  (split-string (substring-no-properties arg-str 1 -1)
                "," t split-string-default-separators))

(defun elpygen--format-args (arg-list)
  "Build a string from the list of argument names.
Argument ARG-LIST is the list of argument names."
  (let ((counter 0))
    (cl-flet
        ((format-arg (arg)
                     (if (string-match-p elpygen-varname-re arg)
                         arg
                       (incf counter)
                       (concat "arg" (number-to-string counter)))))
      (string-join (mapcar #'format-arg arg-list) ", "))))

(defun elpygen--prepare-function-insert-point ()
  "Move the point to a place suitable for function insertion."
  (beginning-of-line)
  (forward-line)
  (while (> (current-indentation) 0)
    (python-nav-end-of-block)
    (forward-line))
  (newline 2))

(defun elpygen--prepare-method-insert-point ()
  "Move the point to a place suitable for method insertion."
  (end-of-defun)
  (open-line 1)
  (python-indent-line-function))


(provide 'elpygen)

;;; elpygen.el ends here
