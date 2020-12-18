(ns advent-of-code-2020.day18
  (:require [clojure.string :as str]))

"
--- Day 18: Operation Order ---
As you look out the window and notice a heavily-forested continent slowly appear over the horizon, you are interrupted by the child sitting next to you. They're curious if you could help them with their math homework.

Unfortunately, it seems like this \"math\" follows different rules than you remember.

The homework (your puzzle input) consists of a series of expressions that consist of addition (+), multiplication (*), and parentheses ((...)). Just like normal math, parentheses indicate that the expression inside must be evaluated before it can be used by the surrounding expression. Addition still finds the sum of the numbers on both sides of the operator, and multiplication still finds the product.

However, the rules of operator precedence have changed. Rather than evaluating multiplication before addition, the operators have the same precedence, and are evaluated left-to-right regardless of the order in which they appear.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are as follows:

1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
      9   + 4 * 5 + 6
         13   * 5 + 6
             65   + 6
                 71
Parentheses can override this order; for example, here is what happens if parentheses are added to form 1 + (2 * 3) + (4 * (5 + 6)):

1 + (2 * 3) + (4 * (5 + 6))
1 +    6    + (4 * (5 + 6))
     7      + (4 * (5 + 6))
     7      + (4 *   11   )
     7      +     44
            51
Here are a few more examples:

2 * 3 + (4 * 5) becomes 26.
5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 437.
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 12240.
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 13632.
Before you can help with the homework, you need to understand it yourself. Evaluate the expression on each line of the homework; what is the sum of the resulting values?
"

(def expressions (map #(str/replace % " " "") (str/split (slurp "src/advent_of_code_2020/day18.input") #"\r\n")))

(defn match-bracket [s]
  (loop [contained ""
         remaining (rest s)
         unmatched 1]
    (if (zero? unmatched) [(apply str (drop-last contained)) (apply str remaining)]
        (condp = (first remaining)
          \( (recur (str contained (first remaining)) (rest remaining) (inc unmatched))
          \) (recur (str contained (first remaining)) (rest remaining) (dec unmatched))
          (recur (str contained (first remaining)) (rest remaining) unmatched)))))

(defn step [left remaining]
  (let [[head & tail] remaining]
    (cond
      (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} head)
      (step (left (read-string (str head))) (apply str tail))
      (#{\+} head)
      (step (fn [n] (+ left n)) (apply str tail))
      (#{\*} head)
      (step (fn [n] (* left n)) (apply str tail))
      (#{\(} head)
      (let [[expr tail] (match-bracket remaining)
            value (step identity expr)]
        (step (left value) tail))
      :else left)))

(defn eval-expr [expr]
  (step identity expr))

;; (reduce + (map eval-expr expressions))
;; => 67800526776934

"
--- Part Two ---
You manage to answer the child's questions and they finish part 1 of their homework, but get stuck when they reach the next section: advanced math.

Now, addition and multiplication have different precedence levels, but they're not the ones you're familiar with. Instead, addition is evaluated before multiplication.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are now as follows:

1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
  3   *   7   * 5 + 6
  3   *   7   *  11
     21       *  11
         231
Here are the other examples from above:

1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
2 * 3 + (4 * 5) becomes 46.
5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.
What do you get if you add up the results of evaluating the homework problems using these new rules?
"

(defn build-tree [lhs unparsed]
  (let [[head & tail] unparsed]
    (cond
      (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} head)
      (if (and (= (first lhs) *) (seq tail) (= (first tail) \+))
        (concat lhs `(~(build-tree `(identity ~(read-string (str head))) tail)))
        (build-tree (concat lhs `(~(read-string (str head)))) tail))
      (#{\+} head)
      (build-tree `(~+ ~lhs) (apply str tail))
      (#{\*} head)
      (build-tree `(~* ~lhs) (apply str tail))
      (#{\(} head)
      (let [[expr tail] (match-bracket unparsed)
            subtree (build-tree '(identity) expr)]
        (if (and (= (first lhs) *) (seq tail) (= (first tail) \+))
          (concat lhs `(~(build-tree subtree tail)))          
          (build-tree (concat lhs `(~subtree)) tail)))
      :else lhs)))

(defn eval-expr-with-precedence [expr]
  (eval (build-tree '(identity) expr)))

;; (reduce + (map eval-expr-with-precedence expressions))
;; => 340789638435483

