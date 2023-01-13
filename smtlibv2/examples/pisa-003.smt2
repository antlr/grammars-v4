; taken from https://github.com/z3str/Z3-str/tree/master/benchmark

(set-logic QF_S)
(set-option :produce-models true)

(declare-fun s () String)
(declare-fun var () String)
(declare-fun ret () String)



(assert (ite (or (str.contains s "<") (str.contains s ">") )
             (= ret "x" )
             (= ret s)
        )
)

(assert (= var (str.++ "<scr" "ipt") ) )

(assert (str.contains s var) )

(assert (not (= ret "x") ) )


(check-sat)
(get-model)