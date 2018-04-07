; taken from https://github.com/z3str/Z3-str/tree/master/benchmark

(set-logic QF_S)
(set-option :produce-models true)

(declare-fun s () String)
(declare-fun ret () String)


(assert (ite (or (str.contains s "<") (str.contains s ">") )
             (= ret "x" )
             (= ret s)
        )
)

(assert (str.contains s "sc") )

(assert (str.contains s "ript scr=") )

(assert (not (= ret "x") ) )


(check-sat)
(get-model)