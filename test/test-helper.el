;;; test-helper.el --- Helpers for elpygen-test.el

;; NOTE: A slightly modified copy from emacs/lisp/progmodes/python-tests.el
(defmacro elpygen-with-temp-python-buffer (contents &rest body)
  "Create a `python-mode' enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((python-indent-guess-indent-offset nil))
       (python-mode)
       (yas-minor-mode)
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))

;;; test-helper.el ends here
