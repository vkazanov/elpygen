;;; -*- lexical-binding: t; -*-
;;; elpygen-test.el --- Tests for elpygen

(require 'elpygen)

(ert-deftest elpygen--symbol-method-p-test ()
  (should (elpygen--symbol-method-p "self.function_name"))
  (should (elpygen--symbol-method-p "self.function"))
  (should (not (elpygen--symbol-method-p "function_name")))
  (should (not (elpygen--symbol-method-p "function"))))

(ert-deftest elpygen--parse-arg-str-test ()
  (should (equal '("1" "abc" "2")
                 (elpygen--parse-arg-str "(1, abc, 2)"))))

(ert-deftest elpygen--format-args-test ()
  (should (equal ""
                 (elpygen--format-args '())))
  (should (equal "arg1, abc, arg2"
                 (elpygen--format-args '("1" "abc" "\"str\"")))))

(ert-deftest elpygen--insert-template-test ()
  (elpygen-with-temp-python-buffer ""
    (elpygen--insert-template elpygen-function-template
                              "funname"
                              '("arg1" "arg2"))
    (let ((expected "def funname(arg1, arg2):
    pass
"))
      (should (equal expected
                     (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest elpygen--get-def-name-test ()
  (elpygen-with-temp-python-buffer
      "
function_name()

self.method_name()

"
    (elpygen-look-at "n_name")
    (should (equal "function_name"
                   (elpygen--get-def-name)))

    (elpygen-look-at "thod")
    (should (equal "self.method_name"
                   (elpygen--get-def-name)))

    (forward-line)
    (should (eq nil
                (elpygen--get-def-name)))))

(ert-deftest elpygen--get-arglist-test ()
  (elpygen-with-temp-python-buffer "
with_empty_args()

no_args

something else

plenty_of_args(arg1, arg2)

self.method_name(marg1)

"
    (elpygen-look-at "empty_args")
    (should (equal '()
                   (elpygen--get-arglist)))

    (elpygen-look-at "no_args")
    (should (equal '()
                   (elpygen--get-arglist)))

    (elpygen-look-at "of_args")
    (should (equal '("arg1" "arg2")
                   (elpygen--get-arglist)))

    (elpygen-look-at "elf.")
    (should (equal '("marg1")
                   (elpygen--get-arglist)))))

(ert-deftest elpygen--within-method-p-test ()
  (elpygen-with-temp-python-buffer "

a_symbol

a_funcall()

bad_method_call()

def definition():
    in_definition()

class Class:
    static_property

    def method(self, smth):
        self.new_method(arg1)

after_class
"
    (elpygen-look-at "_sym")
    (should (not (elpygen--within-method-p)))

    (forward-line)
    (should (not (elpygen--within-method-p)))

    (elpygen-look-at "in_definition")
    (should (not (elpygen--within-method-p)))

    (elpygen-look-at "_funcall")
    (should (not (elpygen--within-method-p)))

    (elpygen-look-at "atic_property")
    (should (not (elpygen--within-method-p)))

    (elpygen-look-at "w_meth")
    (should (elpygen--within-method-p))

    (forward-line)
    (should (not (elpygen--within-method-p)))

    (elpygen-look-at "ter_class")
    (should (not (elpygen--within-method-p)))))

(ert-deftest elpygen--prepare-function-insert-point-simple-test ()
  (elpygen-with-temp-python-buffer "
a_funcall()
other_call()
"
    (elpygen-look-at "_fun")
    (elpygen--prepare-function-insert-point)
    (should (= 5 (line-number-at-pos)))
    (should (equal "
a_funcall()


other_call()
"
                   (buffer-substring-no-properties (point-min)
                                                   (point-max))))))


(ert-deftest elpygen--prepare-function-insert-point-block-test ()
  (elpygen-with-temp-python-buffer "
if True:
    a_funcall()
other_call()
"
    (elpygen-look-at "_fun")
    (elpygen--prepare-function-insert-point)
    (should (= 6 (line-number-at-pos)))
    (should (equal "
if True:
    a_funcall()


other_call()
"
                   (buffer-substring-no-properties (point-min)
                                                   (point-max))))))

(ert-deftest elpygen--prepare-function-insert-point-function-test ()
  (elpygen-with-temp-python-buffer "
def function(arg, arg):
    a_funcall()
    pass
other_call()
"
    (elpygen-look-at "_fun")
    (elpygen--prepare-function-insert-point)
    (should (= 7 (line-number-at-pos)))
    (should (equal "
def function(arg, arg):
    a_funcall()
    pass


other_call()
"
                   (buffer-substring-no-properties (point-min)
                                                   (point-max))))))

(ert-deftest elpygen--prepare-function-insert-point-method-test ()
  (elpygen-with-temp-python-buffer "
class Bla():
    def function(arg, arg):
        a_funcall()
        pass
other_call()
"
    (elpygen-look-at "_funcall")
    (elpygen--prepare-function-insert-point)
    (should (= 8 (line-number-at-pos)))
    (should (equal "
class Bla():
    def function(arg, arg):
        a_funcall()
        pass


other_call()
"
                   (buffer-substring-no-properties (point-min)
                                                   (point-max))))))


(ert-deftest elpygen--elpygen--prepare-method-insert-point-simple-test ()
  (elpygen-with-temp-python-buffer "
class Bla():

    def function(self, arg):
        self.other_call(arg1)
        if True:
            block_call()


    def next_function(self, arg):
        self.call(arg2)


class NextClass():
    pass
"
    (elpygen-look-at "ther_call")
    (elpygen--prepare-method-insert-point)
    (should (= 9 (line-number-at-pos)))
    (should (= 4 (current-column)))

    (elpygen-look-at ".call")
    (elpygen--prepare-method-insert-point)
    (should (= 14 (line-number-at-pos)))
    (should (= 4 (current-column)))))



;;; elpygen-test.el ends here
