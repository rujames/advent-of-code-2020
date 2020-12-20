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

(defn transform [tile f]
  (let [rows (f (:rows tile))]
    (assoc tile
           :rows rows
           :edges (edges rows))))

(defn flip-h [rows]
  (map #(apply str (reverse %)) rows))

(defn flip-v [rows]
  (transpose (map #(apply str (reverse %)) (transpose rows))))

(defn flip-hv [rows]
  (flip-v (flip-h rows)))

(defn matches [tile edges]
  (some #(let [tile (transform tile %)] (if (some edges (:edges tile)) tile))
        [identity
         flip-h
         flip-v
         flip-hv]))

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

(defn rotate-east [rows]
  (transpose (map #(apply str (reverse %)) rows)))

(defn rotate-west [rows]
  (map #(apply str (reverse %)) (transpose rows)))

(defn fit-side
  [side tile edge]
  (letfn [(matches-side [tile] (if (#{edge} (nth (:edges tile) (condp = side :top 0 :left 2))) tile))]
    (or (matches-side tile)
        (matches-side (transform tile rotate-east))
        (matches-side (transform tile rotate-west))
        (matches-side (transform tile flip-hv))
        (matches-side (transform tile flip-v))
        (matches-side (transform tile (comp flip-v rotate-east)))
        (matches-side (transform tile (comp flip-v rotate-west)))
        (matches-side (transform tile flip-h)))))

(def tiling
  (loop [completed-rows []
         unmatched-tiles (remove #{top-left} tiles)
         current-row [top-left]
         current-edge (nth (:edges (last current-row)) 3)]
    (if-let [next-tile (fit-side :left (some #(matches % #{current-edge}) unmatched-tiles) current-edge)]
      (recur completed-rows
             (remove #(= (:id %) (:id next-tile)) unmatched-tiles)
             (conj current-row next-tile)
             (nth (:edges next-tile) 3))
      (let [next-edge (nth (:edges (first current-row)) 1)]
        (if-let [next-tile (fit-side :top (some #(matches % #{next-edge}) unmatched-tiles) next-edge)]
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

(defn count-monsters
  [grid]
  (letfn [(return-if [grid] (let [monsters (count-patterns grid)] (if (> monsters 0) monsters)))]
    (or (return-if grid)
        (return-if (rotate-east grid))
        (return-if (rotate-west grid))
        (return-if (flip-hv grid))
        (return-if (flip-v grid))
        (return-if (flip-v (rotate-east grid)))
        (return-if (flip-v (rotate-west grid)))
        (return-if (flip-h grid)))))

;; (count-monsters grid)
;; => 43

;; (reduce + (map #(count (filter #{\#} %)) grid))
;; => 2647

;; (- 2647 (* 15 43))
;; => 2002
