(ns advent-of-code-2020.day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

"
--- Day 20: Jurassic Jigsaw ---
The high-speed train leaves the forest and quickly carries you south. You can even see a desert in the distance! Since you have some spare time, you might as well see if there was anything interesting in the image the Mythical Information Bureau satellite captured.

After decoding the satellite messages, you discover that the data actually contains many small images created by the satellite's camera array. The camera array consists of many cameras; rather than produce a single square image, they produce many smaller square image tiles that need to be reassembled back into a single image.

Each camera in the camera array returns a single monochrome image tile with a random unique ID number. The tiles (your puzzle input) arrived in a random order.

Worse yet, the camera array appears to be malfunctioning: each image tile has been rotated and flipped to a random orientation. Your first task is to reassemble the original image by orienting the tiles so they fit together.

To show how the tiles should be reassembled, each tile's image data includes a border that should line up exactly with its adjacent tiles. All tiles have this border, and the border lines up exactly when the tiles are both oriented correctly. Tiles at the edge of the image also have this border, but the outermost edges won't line up with any other tiles.

For example, suppose you have the following nine tiles:

Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
By rotating, flipping, and rearranging them, you can find a square arrangement that causes all adjacent borders to line up:

#...##.#.. ..###..### #.#.#####.
..#.#..#.# ###...#.#. .#..######
.###....#. ..#....#.. ..#.......
###.##.##. .#.#.#..## ######....
.###.##### ##...#.### ####.#..#.
.##.#....# ##.##.###. .#...#.##.
#...###### ####.#...# #.#####.##
.....#..## #...##..#. ..#.###...
#.####...# ##..#..... ..#.......
#.##...##. ..##.#..#. ..#.###...

#.##...##. ..##.#..#. ..#.###...
##..#.##.. ..#..###.# ##.##....#
##.####... .#.####.#. ..#.###..#
####.#.#.. ...#.##### ###.#..###
.#.####... ...##..##. .######.##
.##..##.#. ....#...## #.#.#.#...
....#..#.# #.#.#.##.# #.###.###.
..#.#..... .#.##.#..# #.###.##..
####.#.... .#..#.##.. .######...
...#.#.#.# ###.##.#.. .##...####

...#.#.#.# ###.##.#.. .##...####
..#.#.###. ..##.##.## #..#.##..#
..####.### ##.#...##. .#.#..#.##
#..#.#..#. ...#.#.#.. .####.###.
.#..####.# #..#.#.#.# ####.###..
.#####..## #####...#. .##....##.
##.##..#.. ..#...#... .####...#.
#.#.###... .##..##... .####.##.#
#...###... ..##...#.. ...#..####
..#.#....# ##.#.#.... ...##.....
For reference, the IDs of the above tiles are:

1951    2311    3079
2729    1427    2473
2971    1489    1171
To check that you've assembled the image correctly, multiply the IDs of the four corner tiles together. If you do this with the assembled tiles from the example above, you get 1951 * 3079 * 2971 * 1171 = 20899048083289.

Assemble the tiles into an image. What do you get if you multiply together the IDs of the four corner tiles?
"

(defn transpose [rows]
  (for [col (range (count (first rows)))]
    (apply str (for [row rows] (nth row col)))))

(defn edges [rows]
  (conj []
        (first rows) (last rows)
        (first (transpose rows)) (last (transpose rows))))

(defn make-tile [tile]
  (->> (str/split tile #"\r\n")
       (#(assoc {}
                :id (re-find #"\d+" (first %))
                :rows (rest %)
                :edges (edges (rest %))))))

(def tiles (map make-tile (str/split (slurp "src/advent_of_code_2020/day20.input") #"\r\n\r\n")))

(defn flip-h [tile]
  (let [rows (map #(apply str (reverse %)) (:rows tile))]
    (assoc tile
           :rows rows
           :edges (edges rows))))

(defn flip-v [tile]
  (let [rows (transpose (map #(apply str (reverse %)) (transpose (:rows tile))))]
    (assoc tile
           :rows rows
           :edges (edges rows))))

(defn flip-hv [tile]
  (flip-v (flip-h tile)))

(defn matches [tile edges]
  (some #(if (some edges (:edges %)) %)
        [tile
         (flip-h tile)
         (flip-v tile)
         (flip-hv tile)]))

;; (reduce (fn [x t] (* x (read-string (:id t)))) 1 (filter #(= 3 (count (keep (fn [tile] (matches % (set (:edges tile)))) tiles))) tiles))
;; => 18411576553343

"
--- Part Two ---
Now, you're ready to check the image for sea monsters.

The borders of each tile are not part of the actual image; start by removing them.

In the example above, the tiles become:

.#.#..#. ##...#.# #..#####
###....# .#....#. .#......
##.##.## #.#.#..# #####...
###.#### #...#.## ###.#..#
##.#.... #.##.### #...#.##
...##### ###.#... .#####.#
....#..# ...##..# .#.###..
.####... #..#.... .#......

#..#.##. .#..###. #.##....
#.####.. #.####.# .#.###..
###.#.#. ..#.#### ##.#..##
#.####.. ..##..## ######.#
##..##.# ...#...# .#.#.#..
...#..#. .#.#.##. .###.###
.#.#.... #.##.#.. .###.##.
###.#... #..#.##. ######..

.#.#.### .##.##.# ..#.##..
.####.## #.#...## #.#..#.#
..#.#..# ..#.#.#. ####.###
#..####. ..#.#.#. ###.###.
#####..# ####...# ##....##
#.##..#. .#...#.. ####...#
.#.###.. ##..##.. ####.##.
...###.. .##...#. ..#..###
Remove the gaps to form the actual image:

.#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###
Now, you're ready to search for sea monsters! Because your image is monochrome, a sea monster will look like this:

                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
When looking for this pattern in the image, the spaces can be anything; only the # need to match. Also, you might need to rotate or flip your image before it's oriented correctly to find sea monsters. In the above image, after flipping and rotating it to the appropriate orientation, there are two sea monsters (marked with O):

.####...#####..#...###..
#####..#..#.#.####..#.#.
.#.#...#.###...#.##.O#..
#.O.##.OO#.#.OO.##.OOO##
..#O.#O#.O##O..O.#O##.##
...#.#..##.##...#..#..##
#.##.#..#.#..#..##.#.#..
.###.##.....#...###.#...
#.####.#.#....##.#..#.#.
##...#..#....#..#...####
..#.##...###..#.#####..#
....#.##.#.#####....#...
..##.##.###.....#.##..#.
#...#...###..####....##.
.#.##...#.##.#.#.###...#
#.###.#..####...##..#...
#.###...#.##...#.##O###.
.O##.#OO.###OO##..OOO##.
..O#.O..O..O.#O##O##.###
#.#..##.########..#..##.
#.#####..#.#...##..#....
#....##..#.#########..##
#...#.....#..##...###.##
#..###....##.#...##.##.#
Determine how rough the waters are in the sea monsters' habitat by counting the number of # that are not part of a sea monster. In the above example, the habitat's water roughness is 273.

How many # are not part of a sea monster?
"

(def corners (filter #(= 3 (count (keep (fn [tile] (matches % (set (:edges tile)))) tiles))) tiles))

(def top-left (some (fn [corner] (if (and (some #(matches % #{(nth (:edges corner) 1)}) (remove #{corner} tiles))
                                          (some #(matches % #{(nth (:edges corner) 3)}) (remove #{corner} tiles)))
                                   corner)) corners))

(defn flip-h [tile]
  (let [rows (map #(apply str (reverse %)) (:rows tile))]
    (assoc tile
           :rows rows
           :edges (edges rows))))


(defn rotate-east [tile]
  (let [rows (transpose (map #(apply str (reverse %)) (:rows tile)))]
    (assoc tile
           :rows rows
           :edges (edges rows))))

(defn rotate-west [tile]
  (let [rows (map #(apply str (reverse %)) (transpose (:rows tile)))]
    (assoc tile
           :rows rows
           :edges (edges rows))))

(defn fit-left
  "Given a tile known to match the given edge, transform the tile so that the matching edge is on the left"
  [tile edge]
  (letfn [(matches-left [tile] (if (#{edge} (nth (:edges tile) 2)) tile))]
    (or (matches-left tile)
        (matches-left (rotate-east tile))
        (matches-left (rotate-west tile))
        (matches-left (flip-hv tile))
        (matches-left (flip-v tile))
        (matches-left (flip-v (rotate-east tile)))
        (matches-left (flip-v (rotate-west tile)))
        (matches-left (flip-h tile)))))

(defn fit-top
  "Given a tile known to match the given edge, transform the tile so that the matching edge is on the top"
  [tile edge]
  (letfn [(matches-top [tile] (if (#{edge} (nth (:edges tile) 0)) tile))]
    (or (matches-top tile)
        (matches-top (rotate-east tile))
        (matches-top (rotate-west tile))
        (matches-top (flip-hv tile))
        (matches-top (flip-v tile))
        (matches-top (flip-v (rotate-east tile)))
        (matches-top (flip-v (rotate-west tile)))
        (matches-top (flip-h tile)))))

(def tiling
  (loop [completed-rows []
         unmatched-tiles (remove #{top-left} tiles)
         current-row [top-left]
         current-edge (nth (:edges (last current-row)) 3)]
    (if-let [next-tile (fit-left (some #(matches % #{current-edge}) unmatched-tiles) current-edge)]
      (recur completed-rows
             (remove #(= (:id %) (:id next-tile)) unmatched-tiles)
             (conj current-row next-tile)
             (nth (:edges next-tile) 3))
      (let [next-edge (nth (:edges (first current-row)) 1)]
        (if-let [next-tile (fit-top (some #(matches % #{next-edge}) unmatched-tiles) next-edge)]
          (recur (conj completed-rows current-row)
                 (remove #(= (:id %) (:id next-tile)) unmatched-tiles)
                 [next-tile]
                 (nth (:edges next-tile) 3))
          (conj completed-rows current-row))))))

(defn interior [tile]
  (map (comp #(apply str %) rest drop-last) (rest (drop-last (:rows tile)))))

(def grid (mapcat (fn [row] (apply (partial map str) (map interior row))) tiling))

(defn pattern-match [r1 r2 r3]
  (count (filter #{true}
                 (for [i (range (- (count r1) 19))]
                   (letfn [(match-subrow [ns row] (every? #{\#} ((apply juxt (map (fn [n] #(nth % n)) ns)) row)))]
                     (and (match-subrow [18] (subs r1 i))
                          (match-subrow [0 5 6 11 12 17 18 19] (subs r2 i))
                          (match-subrow [1 4 7 10 13 16] (subs r3 i))))))))

(defn count-patterns [grid]
  (reduce + (map pattern-match grid (rest grid) (rest (rest grid)))))

(def grid-tile
  "grid, but as a tile so that we can transform it without rewriting the transform functions"
  {:id "grid"
   :rows grid
   :edges (edges grid)})

(defn count-monsters
  "Counts the number of matches of the monster pattern on the correctly transformed grid"
  [grid-tile]
  (letfn [(return-if [tile] (let [monsters (count-patterns (:rows tile))] (if (> monsters 0) monsters)))]
    (or (return-if grid-tile)
        (return-if (rotate-east grid-tile))
        (return-if (rotate-west grid-tile))
        (return-if (flip-hv grid-tile))
        (return-if (flip-v grid-tile))
        (return-if (flip-v (rotate-east grid-tile)))
        (return-if (flip-v (rotate-west grid-tile)))
        (return-if (flip-h grid-tile)))))

;; (count-monsters grid-tile)
;; => 43

;; (reduce + (map #(count (filter #{\#} %)) grid))
;; => 2647

;; (- 2647 (* 15 43))
;; => 2002
