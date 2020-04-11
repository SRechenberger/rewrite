(ns rewrite.core
  (:gen-class)
  (:require [instaparse.core :as insta])
  (:use [clojure.core.match :only [match]]
        [clojure.string :only [join]]))

(def rewr-program-grammar-file "resources/program.grammar")

(def rewr-term-grammar-file "resources/term.grammar")

(def rewr-term-grammar (slurp rewr-term-grammar-file))

(def rewr-program-grammar
  (str (slurp rewr-program-grammar-file)
       rewr-term-grammar))

(def parse-rewr
  (insta/parser rewr-program-grammar))

(defn parse-term
  [input]
  (first ((insta/parser rewr-term-grammar) input)))


(defn match-term
  [term pattern]
  (match [term pattern]
         [_ [:var variable]]
         {variable term}
         
         [[:const x] [:const p]]
         (when (= x p) {})

         [([:func f & terms] :seq) ([:func p & patterns] :seq)]
         (when (and (= f p) (= (count terms) (count patterns)))
           (let [matched (apply vector (map match-term terms patterns))]
             (when-not (some #(= nil %) matched)
               (apply merge matched))))
         
         :else
         nil))


(defn apply-substitution
  "Applies a substitution to the given term"
  [subst term]
  (match [term]
         [[:var v]] (subst v)
         [[:const _]] term
         [([:func f & terms] :seq)]
         (into []
               (concat [:func f]
                       (map #(apply-substitution subst %) terms)))))


(defn apply-rule
  [term head body]
  (let [matching (match-term term head)]
    (when matching
      (apply-substitution matching body))))


(defn apply-first-matching-rule-to-term
  [term rules]
  (loop [[[head body] & rs] rules]
    (when head
      (let [next-term (apply-rule term head body)]
        (if next-term
          next-term
          (recur rs))))))


(defn prepare-program
  [program]
  (map #(list (% 1) (% 2))
       (drop 1 program)))


(declare eval-one-step)


(defn eval-subterms-one-step
  [terms rules]
  (loop [[t & ts] terms
         acc []]
    (when t
      (let [t' (eval-one-step t rules)]
        (if t'
          (concat (conj acc t') ts)
          (recur ts (conj acc t)))))))
    

(defn eval-one-step
  [term program]
  (let [term' (apply-first-matching-rule-to-term term program)]
    (if term'
      term'
      (match [term]
             [([:func f & terms] :seq)]
             (when-let [subterms (eval-subterms-one-step terms program)]
               (vec (list* :func f subterms))) 
             :else
             nil))))

    
(defn eval-rewr
  [program term]
  (loop [t term term' (eval-one-step term program)]
    (if-not term'
      t
      (if (= term' t)
        term'
        (recur term' (eval-one-step term' program))))))


(defn print-rewr
  [term]
  (match [term]
         [[:const z]] z
         [([:func f & terms] :seq)]
         (str "(" f " " (join " " (map print-rewr terms)) ")")))

(def exit-term (parse-term "exit"))

(defn rewr-read-eval-print
  [program]
  (let [term (parse-term (read-line))]
    (when-not (= term exit-term)
      ; (println (eval-rewr program term))
      (println (print-rewr (eval-rewr program term)))
      true)))


(defn -main
  [& args]
  (let [prg-file (first args)]
    (if prg-file
      (let [prg (prepare-program (parse-rewr (slurp prg-file)))]
        (while (rewr-read-eval-print prg))))))
