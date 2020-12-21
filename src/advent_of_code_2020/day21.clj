(ns advent-of-code-2020.day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

"
--- Day 21: Allergen Assessment ---
You reach the train's last stop and the closest you can get to your vacation island without getting wet. There aren't even any boats here, but nothing can stop you now: you build a raft. You just need a few days' worth of food for your journey.

You don't speak the local language, so you can't read any ingredients lists. However, sometimes, allergens are listed in a language you do understand. You should be able to use this information to determine which ingredient contains which allergen and work out which foods are safe to take with you on your trip.

You start by compiling a list of foods (your puzzle input), one food per line. Each line includes that food's ingredients list followed by some or all of the allergens the food contains.

Each allergen is found in exactly one ingredient. Each ingredient contains zero or one allergen. Allergens aren't always marked; when they're listed (as in (contains nuts, shellfish) after an ingredients list), the ingredient that contains each listed allergen will be somewhere in the corresponding ingredients list. However, even if an allergen isn't listed, the ingredient that contains that allergen could still be present: maybe they forgot to label it, or maybe it was labeled in a language you don't know.

For example, consider the following list of foods:

mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
The first food in the list has four ingredients (written in a language you don't understand): mxmxvkd, kfcds, sqjhc, and nhms. While the food might contain other allergens, a few allergens the food definitely contains are listed afterward: dairy and fish.

The first step is to determine which ingredients can't possibly contain any of the allergens in any food in your list. In the above example, none of the ingredients kfcds, nhms, sbzzf, or trh can contain an allergen. Counting the number of times any of these ingredients appear in any ingredients list produces 5: they all appear once each except sbzzf, which appears twice.

Determine which ingredients cannot possibly contain any of the allergens in your list. How many times do any of those ingredients appear?
"

(defn make-food [food]
  (let [[ingredients allergens] (str/split food #"\(")
        ingredients (set (str/split ingredients #" "))
        allergens (mapcat rest (re-seq #" ([a-z]+)" allergens))]
    {:ingredients ingredients
     :allergens allergens}))

(def foods (map make-food (str/split (slurp "src/advent_of_code_2020/day21.input") #"\r\n")))

(def ingredients (apply set/union (map :ingredients foods)))

(def allergens (apply set/union (map (comp set :allergens) foods)))

(defn ingredients-with-allergen [foods]
  (letfn [(intersect-ingredients [ingredients food] (set/intersection ingredients (:ingredients food)))]
    (set (mapcat (fn [a]
                   (let [foods-with-allergen (filter #(some #{a} (:allergens %)) foods)]
                     (reduce intersect-ingredients (:ingredients (first foods-with-allergen)) foods-with-allergen)))
                 allergens))))

(def safe-ingredients (set/difference ingredients (ingredients-with-allergen foods)))

;; (reduce + (map (fn [food] (count (set/intersection safe-ingredients (:ingredients food)))) foods))
;; => 2493

"
--- Part Two ---
Now that you've isolated the inert ingredients, you should have enough information to figure out which ingredient contains which allergen.

In the above example:

mxmxvkd contains dairy.
sqjhc contains fish.
fvjkl contains soy.
Arrange the ingredients alphabetically by their allergen and separate them by commas to produce your canonical dangerous ingredient list. (There should not be any spaces in your canonical dangerous ingredient list.) In the above example, this would be mxmxvkd,sqjhc,fvjkl.

Time to stock your raft with supplies. What is your canonical dangerous ingredient list?
"

(defn ingredients-by-allergen [foods]
  (letfn [(intersect-ingredients [ingredients food] (set/intersection ingredients (:ingredients food)))]
    (into {} (map (fn [a]
                    (let [foods-with-allergen (filter #(some #{a} (:allergens %)) foods)]
                      [a (reduce intersect-ingredients (:ingredients (first foods-with-allergen)) foods-with-allergen)]))
                  allergens))))

;; (ingredients-by-allergen foods)
;; =>
;; {"eggs" #{"jxx"},
;;  "sesame" #{"tsnkknk" "dklgl"},
;;  "peanuts" #{"dklgl" "pmvfzk" "zzt"},
;;  "wheat" #{"tlgrhdh" "tsnkknk" "pmvfzk"},
;;  "dairy" #{"kqv" "zzt"},
;;  "shellfish" #{"kqv" "tsnkknk" "qdlpbt" "pmvfzk"},
;;  "nuts" #{"dklgl" "zzt"},
;;  "fish" #{"jxx" "zzt"}}

;; Solution: kqv,jxx,zzt,dklgl,pmvfzk,tsnkknk,qdlpbt,tlgrhdh
