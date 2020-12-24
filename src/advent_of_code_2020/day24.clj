(ns advent-of-code-2020.day24
  (:require [clojure.string :as str]))

"
--- Day 24: Lobby Layout ---
Your raft makes it to the tropical island; it turns out that the small crab was an excellent navigator. You make your way to the resort.

As you enter the lobby, you discover a small problem: the floor is being renovated. You can't even reach the check-in desk until they've finished installing the new tile floor.

The tiles are all hexagonal; they need to be arranged in a hex grid with a very specific color pattern. Not in the mood to wait, you offer to help figure out the pattern.

The tiles are all white on one side and black on the other. They start with the white side facing up. The lobby is large enough to fit whatever pattern might need to appear there.

A member of the renovation crew gives you a list of the tiles that need to be flipped over (your puzzle input). Each line in the list identifies a single tile that needs to be flipped by giving a series of steps starting from a reference tile in the very center of the room. (Every line starts from the same reference tile.)

Because the tiles are hexagonal, every tile has six neighbors: east, southeast, southwest, west, northwest, and northeast. These directions are given in your list, respectively, as e, se, sw, w, nw, and ne. A tile is identified by a series of these directions with no delimiters; for example, esenee identifies the tile you land on if you start at the reference tile and then move one tile east, one tile southeast, one tile northeast, and one tile east.

Each time a tile is identified, it flips from white to black or from black to white. Tiles might be flipped more than once. For example, a line like esew flips a tile immediately adjacent to the reference tile, and a line like nwwswee flips the reference tile itself.

Here is a larger example:

sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
In the above example, 10 tiles are flipped once (to black), and 5 more are flipped twice (to black, then back to white). After all of these instructions have been followed, a total of 10 tiles are black.

Go through the renovation crew's list and determine which tiles they need to flip. After all of the instructions have been followed, how many tiles are left with the black side up?
"

(defn parse-instructions [s]
  (loop [s (seq s)
         instructions []]
    (if (seq s)
      (condp = (first s)
        \e (recur (rest s) (conj instructions :east))
        \w (recur (rest s) (conj instructions :west))
        \n (condp = (second s)
             \w (recur (rest (rest s)) (conj instructions :northwest))
             \e (recur (rest (rest s)) (conj instructions :northeast)))
        \s (condp = (second s)
             \w (recur (rest (rest s)) (conj instructions :southwest))
             \e (recur (rest (rest s)) (conj instructions :southeast))))
      instructions)))

(def input (map parse-instructions (str/split (slurp "src/advent_of_code_2020/day24.input") #"\r\n")))

(defn identified-tile [instructions]
  (reduce (fn [[x y] instruction]
            (condp = instruction
              :east [(inc x) y]
              :west [(dec x) y]
              :southeast [(+ x (mod y 2)) (dec y)]
              :southwest [(- x (mod (inc y) 2)) (dec y)]
              :northeast [(+ x (mod y 2)) (inc y)]
              :northwest [(- x (mod (inc y) 2)) (inc y)]))
          [0 0] instructions))

;; (count (filter #(= 1 (mod % 2)) (vals (frequencies (map identified-tile input)))))
;; => 455
