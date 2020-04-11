(ns rewrite.core-test
  (:require [clojure.test :refer :all]
            [rewrite.core :refer :all]))

(deftest parse-test
  (is (= [:var "X"] (parse-term "X")))
  (is (= [:const "x"] (parse-term "x")))
  (is (= [:func "f" [:const "y"]] (parse-term "(f y)")))
  (is (= [:const "1"] (parse-term "1"))))
