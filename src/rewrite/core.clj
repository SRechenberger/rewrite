(ns rewrite.core
  (:gen-class)
  (:require [instaparse.core :as insta]
            [rolling-stones.core :as sat :refer [! at-least at-most exactly]])
  (:use [clojure.core.match :only [match]]
        [clojure.string :only [join]]
        [clojure.java.shell :only [sh]]))


;;; Parser
(def rewr-program-grammar-file
  "resources/program.grammar")

(def rewr-program-grammar
  (slurp rewr-program-grammar-file))

(def parse-rewr
  (insta/parser rewr-program-grammar))

(defn parse-term
  [input]
  (first (parse-rewr input :start :term)))

(defn parse-constraint
  [input]
  (first (parse-rewr input :start :constraint)))


;;; Evaluation
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
         [nil] term
         [[:var v]] (subst v)
         [[:const _]] term
         [([:func f & terms] :seq)]
         (vec (concat [:func f]
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

;;; Analysis
(def solver-file "resources/test/solver.pl")

(defn free-vars
  [term]
  (match [term]
         [[:const _]] []
         [[:var x]] [x]
         [([:func _ & terms] :seq)] (vec (apply concat (map free-vars terms)))))

(defn derive-assertions-from-rules
  [rules properties]
  (if (or (seq? properties) (list? properties))
    (apply concat (map #(derive-assertions-from-rules rules %) properties))
    (apply concat
           (map (fn [[head body]]
                  (let [free (free-vars head)
                        subst (apply merge
                                     (map (fn [x] {x [:var (str (gensym x))]}) free))]
                    [[[:negative properties (apply-substitution subst head)]
                      [:positive properties (apply-substitution subst body)]]
                     [[:positive properties (apply-substitution subst head)]
                      [:negative properties (apply-substitution subst body)]]]))
                rules))))

(defn analyze-program
  [program]
  (let [[rules assertions properties]
        (loop [[stmt & stmts] program
               [rs as ps] [[] [] []]]
          (if (nil? stmt)
            [rs as ps]                   
            (case (first stmt)
              :rule (recur stmts [(conj rs (rest stmt)) as ps])
              :assertion (recur stmts [rs (conj as (rest stmt)) ps])
              :declaration (recur stmts [rs as (rest stmt)]))))
        all-assertions
        (concat assertions
                (derive-assertions-from-rules rules properties))
        stones-cnf
        (map (partial map #(case (first %)
                             :negative (! (rest %))
                             :positive (rest %))) all-assertions)]
    (loop [[s & ss] (sat/solutions-symbolic-cnf stones-cnf)]
      (when s
        (println "Input:")
        (println (join "\n" (map #(str "  " %) (stones->prolog s))))
        (println "Output:")
        (let [result ((comp (partial feed-to-prolog solver-file)
                            stones->prolog)
                      s)]
          (if (= (result :exit) 0)
            (do
              (println "SAT")
              (print (result :out)))
            (do
              (println "UNSAT")
              (recur ss))))))))
          

(defn stones->prolog
  [constraints]
  (map #(if (sat/negative? %)
          (let [literal (sat/negate %)]
                         ; (println literal)
                         (sexpr->func (str "not_" (first literal))
                                      (second literal)))
                             
          (sexpr->func (first %) (second %)))
       constraints))

(defn sexpr->func
  [constr term]
  (print-rewr-prolog-style [:func constr term]))
  
(defn print-rewr-prolog-style
  [term]
  (match [term]
         [[:const z]] z
         [[:var x]] x
         [([:func f & terms] :seq)]
         (str f "(" (join ", " (map print-rewr-prolog-style terms)) ")")))

(defn feed-to-prolog
  [solver-file terms]
  (sh "swipl" "-l" solver-file "-q" "-g" "main"
      :in (str (join ", " terms) ".")))

;;; CLI
(defn print-rewr
  [term]
  (match [term]
         [[:const z]] z
         [[:var x]] x
         [([:func f & terms] :seq)]
         (str "(" f " " (join " " (map print-rewr terms)) ")")))

(def exit-term (parse-term "exit"))

(defn rewr-read-eval-print
  [program]
  (let [term (parse-term (read-line))]
    (when-not (= term exit-term)
      (println (print-rewr (eval-rewr program term)))
      true)))


(defn -main
  [& args]
  (let [prg-file (first args)]
    (if prg-file
      (let [program (parse-rewr (slurp prg-file))
            rules (filter #(= :rule (first %)) (drop 1 program))                                
            rules' (prepare-program rules)]
        (while (rewr-read-eval-print rules'))))))
