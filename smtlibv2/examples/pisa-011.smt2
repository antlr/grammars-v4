; taken from https://github.com/z3str/Z3-str/tree/master/benchmark

(set-logic QF_S)
(set-option :produce-models true)


(declare-fun s () String)
(declare-fun f () String)
(declare-fun ret () String)


(assert (= ret (str.replace s "<" "&lt;") ) )

(assert (= f (str.++  "jquery.js"  "\""" "></script>" ) ) )

(assert (= s  (str.++ "<script src=\""" f ) ) )


(check-sat)
(get-model)