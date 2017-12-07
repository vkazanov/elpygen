;;; -*- lexical-binding: t; -*-
;;; elpygen-test.el --- Tests for elpygen

(require 'elpygen)

(ert-deftest symbol-method-p-test ()
  (should (elpygen-symbol-method-p "self.function_name"))
  (should (elpygen-symbol-method-p "self.function"))
  (should (not (elpygen-symbol-method-p "function_name")))
  (should (not (elpygen-symbol-method-p "function"))))

;;; elpygen-test.el ends here
