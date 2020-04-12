(ns rewrite.core-test
  (:require [clojure.test :refer :all]
            [rewrite.core :refer :all]))

(defmacro def-parser-test
  [name parser & cases]
  (if (not= (mod (count cases) 2) 0)
    (throw (IllegalArgumentException. "cases must have even length"))
    
    (let [cases' (loop [[inp exp & cs] cases acc []]
                   (if (nil? exp)
                     acc
                     (recur cs (conj acc [exp inp]))))]
      (list* 'deftest
             name
             (map
              (fn [case]
                `(is (= ~(first case) ~(list parser (second case)))))
              cases')))))

(def-parser-test test-parse-constraint parse-constraint
  "false"  [:positive [:false]] 
  "true"  [:positive [:true]] 
  "not false"  [:negative [:false]] 
  "not true"  [:negative [:true]] 
  "{foo x}"  [:positive "foo" [:const "x"]] 
  "not {foo x}"  [:negative "foo" [:const "x"]] 
  "{foo false}"  [:positive "foo" [:const "false"]])
  

(def-parser-test test-parse-term parse-term
  "X" [:var "X"]
  "x" [:const "x"]
  "(f y)" [:func "f" [:const "y"]]
  "(if Cond Then Else)" [:func "if" [:var "Cond"] [:var "Then"] [:var "Else"]])

(def-parser-test test-parse-assertion #(parse-rewr % :start :assertion)
  "assert {foo x}, not false."
  [:assertion [:positive "foo" [:const "x"]] [:negative [:false]]]
  "assert not {nat N}, not {zero (s N)}."
  [:assertion
   [:negative "nat" [:var "N"]]
   [:negative "zero" [:func "s" [:var "N"]]]])

(def-parser-test test-parse-declaration #(parse-rewr % :start :declaration)
  "declare zero nat."
  [:declaration "zero" "nat"])
                                                   

