; taken from https://github.com/z3str/Z3-str/tree/master/benchmark

(set-logic QF_S)
(set-option :produce-models true)


(declare-fun s () String)
(declare-fun ret0 () String)
(declare-fun ret1 () String)


(assert (= ret0 (str.replace (str.replace  s  "<"  "&lt;")  ">"  "&gt;") ) )

(assert (= ret1 (str.++  ret0  "<br/>") ) )

(assert (= s "<script type = \""text/javascript\"">") )

(check-sat)
(get-model)
