(ns advent-of-code-2020.day12
  (:require [clojure.string :as str]))

"
--- Day 12: Rain Risk ---
Your ferry made decent progress toward the island, but the storm came in faster than anyone expected. The ferry needs to take evasive actions!

Unfortunately, the ship's navigation computer seems to be malfunctioning; rather than giving a route directly to safety, it produced extremely circuitous instructions. When the captain uses the PA system to ask if anyone can help, you quickly volunteer.

The navigation instructions (your puzzle input) consists of a sequence of single-character actions paired with integer input values. After staring at them for a few minutes, you work out what they probably mean:

Action N means to move north by the given value.
Action S means to move south by the given value.
Action E means to move east by the given value.
Action W means to move west by the given value.
Action L means to turn left the given number of degrees.
Action R means to turn right the given number of degrees.
Action F means to move forward by the given value in the direction the ship is currently facing.
The ship starts by facing east. Only the L and R actions change the direction the ship is facing. (That is, if the ship is facing east and the next instruction is N10, the ship would move north 10 units, but would still move east if the following action were F.)

For example:

F10
N3
F7
R90
F11
These instructions would be handled as follows:

F10 would move the ship 10 units east (because the ship starts by facing east) to east 10, north 0.
N3 would move the ship 3 units north to east 10, north 3.
F7 would move the ship another 7 units east (because the ship is still facing east) to east 17, north 3.
R90 would cause the ship to turn right by 90 degrees and face south; it remains at east 17, north 3.
F11 would move the ship 11 units south to east 17, south 8.
At the end of these instructions, the ship's Manhattan distance (sum of the absolute values of its east/west position and its north/south position) from its starting position is 17 + 8 = 25.

Figure out where the navigation instructions lead. What is the Manhattan distance between that location and the ship's starting position?
"

(def instructions (str/split (slurp "src/advent_of_code_2020/day12.input") #"\r\n"))

(defn north [position dist]
  (mapv + position [0 dist]))

(defn south [position dist]
  (mapv + position [0 (* -1 dist)]))

(defn east [position dist]
  (mapv + position [dist 0]))

(defn west [position dist]
  (mapv + position [(* -1 dist) 0]))

(defn forward [position direction dist]
  (mapv + position (mapv #(* dist %) direction)))

(defn left [direction degrees]
  (nth (iterate #(condp = %
                    [1 0] [0 1]
                    [0 1] [-1 0]
                    [-1 0] [0 -1]
                    [0 -1] [1 0])
                 direction)
        (quot degrees 90)))

(defn right [direction degrees]
  (nth (iterate #(condp = %
                    [1 0] [0 -1]
                    [0 -1] [-1 0]
                    [-1 0] [0 1]
                    [0 1] [1 0])
                 direction)
       (quot degrees 90)))

(defn execute [[position direction] instruction]
  (let [[code & arg] instruction
        arg (read-string (apply str arg))]
    (condp = code
      \N [(north position arg) direction]
      \E [(east position arg) direction]
      \S [(south position arg) direction]
      \W [(west position arg) direction]
      \L [position (left direction arg)]
      \R [position (right direction arg)]
      \F [(forward position direction arg) direction])))

(defn navigate [instructions]
  (reduce execute [[0 0] [1 0]] instructions))

(defn manhattan [[x y]]
  (+ (Math/abs x) (Math/abs y)))

;; (manhattan (first (navigate instructions)))
;; => 415

