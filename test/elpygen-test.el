;;; -*- lexical-binding: t; -*-
;;; elpygen-test.el --- Tests for elpygen

(require 'elpygen)

(ert-deftest symbol-method-p-test ()
  (should (elpygen-symbol-method-p "self.function_name"))
  (should (elpygen-symbol-method-p "self.function"))
  (should (not (elpygen-symbol-method-p "function_name")))
  (should (not (elpygen-symbol-method-p "function"))))

(ert-deftest parse-arg-str-test ()
  (should (equal '("1" "abc" "2")
                 (elpygen-parse-arg-str "(1, abc, 2)"))))

(ert-deftest format-args-test ()
  (should (equal ""
                 (elpygen-format-args '())))
  (should (equal "arg1, abc, arg2"
                 (elpygen-format-args '("1" "abc" "2")))))

;;; elpygen-test.el ends here
