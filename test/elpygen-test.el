;;; -*- lexical-binding: t; -*-
;;; elpygen-test.el --- Tests for elpygen

(require 'elpygen)

(ert-deftest elpygen-symbol-method-p-test ()
  (should (elpygen-symbol-method-p "self.function_name"))
  (should (elpygen-symbol-method-p "self.function"))
  (should (not (elpygen-symbol-method-p "function_name")))
  (should (not (elpygen-symbol-method-p "function"))))

(ert-deftest elpygen-parse-arg-str-test ()
  (should (equal '("1" "abc" "2")
                 (elpygen-parse-arg-str "(1, abc, 2)"))))

(ert-deftest elpygen-format-args-test ()
  (should (equal ""
                 (elpygen-format-args '())))
  (should (equal "arg1, abc, arg2"
                 (elpygen-format-args '("1" "abc" "2")))))

(defconst elpygen-function-template-test-function-stub "def funname(arg1, arg2):
    pass
")

(ert-deftest elpygen-insert-template-test ()
  (elpygen-with-temp-python-buffer ""
    (elpygen-insert-template "funname" '("arg1" "arg2"))
    (should (equal elpygen-function-template-test-function-stub
                   (buffer-substring-no-properties (point-min) (point-max))))))

;;; elpygen-test.el ends here
