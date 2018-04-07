; taken from https://github.com/z3str/Z3-str/tree/master/benchmark

(set-logic QF_S)
(set-option :produce-models true)

(declare-fun s0  ()  String)
(declare-fun s1  () String)
(declare-fun s2  () String)
(declare-fun s3  () String)
(declare-fun s4  () String)
(declare-fun ret  () String)



(assert (= s1 (str.replace s0 "&" "&amp;") ) )

(assert (= s2 (str.replace s1 "&nbsp;"  " ") ) )

(assert (= s3 (str.replace s2 "\"""  "&quot;") ) )

(assert (= s4 (str.replace s3 "<"  "&lt;") ) )

(assert (= ret (str.replace s4 ">"  "&gt;") ) )

(assert (or (str.contains ret "<")  (str.contains ret ">") ) )

(check-sat)
(get-model)
