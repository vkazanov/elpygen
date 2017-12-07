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

(ert-deftest elpygen-get-def-name-test ()
  (elpygen-with-temp-python-buffer
   "
function_name()

self.method_name()

"
   (elpygen-look-at "n_name")
   (should (equal "function_name"
                  (elpygen-get-def-name)))

   (elpygen-look-at "thod")
   (should (equal "self.method_name"
                  (elpygen-get-def-name)))

   (forward-line)
   (should (eq nil
               (elpygen-get-def-name)))))

(ert-deftest elpygen-get-arglist-test ()
  (elpygen-with-temp-python-buffer "
with_empty_args()

no_args

something else

plenty_of_args(arg1, arg2)

self.method_name(marg1)

"
    (elpygen-look-at "empty_args")
    (should (equal '()
                   (elpygen-get-arglist)))

    (elpygen-look-at "no_args")
    (should (equal '()
                   (elpygen-get-arglist)))

    (elpygen-look-at "of_args")
    (should (equal '("arg1" "arg2")
                   (elpygen-get-arglist)))

    (elpygen-look-at "elf.")
    (should (equal '("marg1")
                   (elpygen-get-arglist)))))

;;; elpygen-test.el ends here
