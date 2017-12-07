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

;; NOTE: A slightly modified copy from emacs/lisp/progmodes/python-tests.el
(defun elpygen-look-at (string &optional num restore-point)
  "Move point at beginning of STRING in the current buffer.
Optional argument NUM defaults to 1 and is an integer indicating
how many occurrences must be found, when positive the search is
done forwards, otherwise backwards.  When RESTORE-POINT is
non-nil the point is not moved but the position found is still
returned.  When searching forward and point is already looking at
STRING, it is skipped so the next STRING occurrence is selected."
  (let* ((num (or num 1))
         (starting-point (point))
         (string (regexp-quote string))
         (search-fn (if (> num 0) #'re-search-forward #'re-search-backward))
         (deinc-fn (if (> num 0) #'1- #'1+))
         (found-point))
    (prog2
        (catch 'exit
          (while (not (= num 0))
            (when (and (> num 0)
                       (looking-at string))
              ;; Moving forward and already looking at STRING, skip it.
              (forward-char (length (match-string-no-properties 0))))
            (and (not (funcall search-fn string nil t))
                 (throw 'exit t))
            (when (> num 0)
              ;; `re-search-forward' leaves point at the end of the
              ;; occurrence, move back so point is at the beginning
              ;; instead.
              (forward-char (- (length (match-string-no-properties 0)))))
            (setq
             num (funcall deinc-fn num)
             found-point (point))))
        found-point
      (and restore-point (goto-char starting-point)))))

;;; test-helper.el ends here
