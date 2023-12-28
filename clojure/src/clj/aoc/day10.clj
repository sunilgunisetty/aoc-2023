(ns aoc.day10
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))

;; --- Day 10: Pipe Maze ---
;; You use the hang glider to ride the hot air from Desert Island all the way up to the floating metal island. This island is surprisingly cold and there definitely aren't any thermals to glide on, so you leave your hang glider behind.

;; You wander around for a while, but you don't find any people or animals. However, you do occasionally find signposts labeled "Hot Springs" pointing in a seemingly consistent direction; maybe you can find someone at the hot springs and ask them where the desert-machine parts are made.

;; The landscape here is alien; even the flowers and trees are made of metal. As you stop to admire some metal grass, you notice something metallic scurry away in your peripheral vision and jump into a big pipe! It didn't look like any animal you've ever seen; if you want a better look, you'll need to get ahead of it.

;; Scanning the area, you discover that the entire field you're standing on is densely packed with pipes; it was hard to tell at first because they're the same metallic silver color as the "ground". You make a quick sketch of all of the surface pipes you can see (your puzzle input).

;; The pipes are arranged in a two-dimensional grid of tiles:

;; | is a vertical pipe connecting north and south.
;; - is a horizontal pipe connecting east and west.
;; L is a 90-degree bend connecting north and east.
;; J is a 90-degree bend connecting north and west.
;; 7 is a 90-degree bend connecting south and west.
;; F is a 90-degree bend connecting south and east.
;; . is ground; there is no pipe in this tile.
;; S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
;; Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.

;; For example, here is a square loop of pipe:

;; .....
;; .F-7.
;; .|.|.
;; .L-J.
;; .....
;; If the animal had entered this loop in the northwest corner, the sketch would instead look like this:

;; .....
;; .S-7.
;; .|.|.
;; .L-J.
;; .....
;; In the above diagram, the S tile is still a 90-degree F bend: you can tell because of how the adjacent pipes connect to it.

;; Unfortunately, there are also many pipes that aren't connected to the loop! This sketch shows the same loop as above:

;; -L|F7
;; 7S-7|
;; L|7||
;; -L-J|
;; L|-JF
;; In the above diagram, you can still figure out which pipes form the main loop: they're the ones connected to S, pipes those pipes connect to, pipes those pipes connect to, and so on. Every pipe in the main loop connects to its two neighbors (including S, which will have exactly two pipes connecting to it, and which is assumed to connect back to those two pipes).

;; Here is a sketch that contains a slightly more complex main loop:

;; ..F7.
;; .FJ|.
;; SJ.L7
;; |F--J
;; LJ...
;; Here's the same example sketch with the extra, non-main-loop pipe tiles also shown:

;; 7-F7-
;; .FJ|7
;; SJLL7
;; |F--J
;; LJ.LJ
;; If you want to get out ahead of the animal, you should find the tile in the loop that is farthest from the starting position. Because the animal is in the pipe, it doesn't make sense to measure this by direct distance. Instead, you need to find the tile that would take the longest number of steps along the loop to reach from the starting point - regardless of which way around the loop the animal went.

;; In the first example with the square loop:

;; .....
;; .S-7.
;; .|.|.
;; .L-J.
;; .....
;; You can count the distance each tile in the loop is from the starting point like this:

;; .....
;; .012.
;; .1.3.
;; .234.
;; .....
;; In this example, the farthest point from the start is 4 steps away.

;; Here's the more complex loop again:

;; ..F7.
;; .FJ|.
;; SJ.L7
;; |F--J
;; LJ...
;; Here are the distances for each tile on that loop:

;; ..45.
;; .236.
;; 01.78
;; 14567
;; 23...
;; Find the single giant loop starting at S. How many steps along the loop does it take to get from the starting position to the point farthest from the starting position?

(def input (->> "input/day10.txt" io/resource slurp))
(def sample-input (->> "input/day10-sample1.txt" io/resource slurp))
(def sample-input2 (->> "input/day10-sample2.txt" io/resource slurp))

(defn process-input
  [input]
  (->> input string/split-lines))

(defn find-start
  [input]
  (loop [lines input x 0 y -1]
    (let [line (first lines)]
      (if-let [y (string/index-of line "S")]
        [x y]
        (recur (rest lines) (inc x) -1)))))

(def up-symbols #{\| \7 \F})
(def down-symbols #{\| \L \J})
(def left-symbols #{\- \L \F})
(def right-symbols #{\- \J \7})

(defn move-up?
  [input x y]
  (let [curr-sym (get-in input [x y])]
    (when (and (#{\S \| \J \L} curr-sym) (up-symbols (get-in input [(dec x) y])))
      [(dec x) y])))

(defn move-down?
  [input x y]
  (let [curr-sym (get-in input [x y])]
    (when (and (#{\S \| \7 \F} curr-sym) (down-symbols (get-in input [(inc x) y])))
      [(inc x) y])))

(defn move-left?
  [input x y]
  (let [curr-sym (get-in input [x y])]
    (when (and (#{\S \- \J \7} curr-sym) (left-symbols (get-in input [x (dec y)])))
      [x (dec y)])))

(defn move-right?
  [input x y]
  (let [curr-sym (get-in input [x y])]
    (when (and (#{\S \- \L \F} curr-sym) (right-symbols (get-in input [x (inc y)])))
      [x (inc y)])))

(defn possible-paths
  [input [x y]]
  (reduce (fn [acc f]
            (if-let [res (f input x y)]
              (conj acc res)
              acc))
          []
          [move-up? move-down? move-left? move-right?]))

(defn find-path
  [input]
  (let [processed-input   (process-input input)
        [start-x start-y] (find-start processed-input)]
    (loop [res [] queue [[start-x start-y]] seen #{}]
      (if (and (seq queue)
               (not (contains? seen (first queue))))
        (let [paths (remove #(seen %) (possible-paths processed-input (first queue)))]
          (recur (conj res (first queue)) (concat (rest queue) paths) (conj seen (first queue))))
        res))))

(defn day10-part1
  [input]
  (/ (count (find-path input)) 2))

;; --- Part Two ---
;; You quickly reach the farthest point of the loop, but the animal never emerges. Maybe its nest is within the area enclosed by the loop?

;; To determine whether it's even worth taking the time to search for such a nest, you should calculate how many tiles are contained within the loop. For example:

;; ...........
;; .S-------7.
;; .|F-----7|.
;; .||.....||.
;; .||.....||.
;; .|L-7.F-J|.
;; .|..|.|..|.
;; .L--J.L--J.
;; ...........
;; The above loop encloses merely four tiles - the two pairs of . in the southwest and southeast (marked I below). The middle . tiles (marked O below) are not in the loop. Here is the same loop again with those regions marked:

;; ...........
;; .S-------7.
;; .|F-----7|.
;; .||OOOOO||.
;; .||OOOOO||.
;; .|L-7OF-J|.
;; .|II|O|II|.
;; .L--JOL--J.
;; .....O.....
;; In fact, there doesn't even need to be a full tile path to the outside for tiles to count as outside the loop - squeezing between pipes is also allowed! Here, I is still within the loop and O is still outside the loop:

;; ..........
;; .S------7.
;; .|F----7|.
;; .||OOOO||.
;; .||OOOO||.
;; .|L-7F-J|.
;; .|II||II|.
;; .L--JL--J.
;; ..........
;; In both of the above examples, 4 tiles are enclosed by the loop.

;; Here's a larger example:

;; .F----7F7F7F7F-7....
;; .|F--7||||||||FJ....
;; .||.FJ||||||||L7....
;; FJL7L7LJLJ||LJ.L-7..
;; L--J.L7...LJS7F-7L7.
;; ....F-J..F7FJ|L7L7L7
;; ....L7.F7||L7|.L7L7|
;; .....|FJLJ|FJ|F7|.LJ
;; ....FJL-7.||.||||...
;; ....L---J.LJ.LJLJ...
;; The above sketch has many random bits of ground, some of which are in the loop (I) and some of which are outside it (O):

;; OF----7F7F7F7F-7OOOO
;; O|F--7||||||||FJOOOO
;; O||OFJ||||||||L7OOOO
;; FJL7L7LJLJ||LJIL-7OO
;; L--JOL7IIILJS7F-7L7O
;; OOOOF-JIIF7FJ|L7L7L7
;; OOOOL7IF7||L7|IL7L7|
;; OOOOO|FJLJ|FJ|F7|OLJ
;; OOOOFJL-7O||O||||OOO
;; OOOOL---JOLJOLJLJOOO
;; In this larger example, 8 tiles are enclosed by the loop.

;; Any tile that isn't part of the main loop can count as being enclosed by the loop. Here's another example with many bits of junk pipe lying around that aren't connected to the main loop at all:

;; FF7FSF7F7F7F7F7F---7
;; L|LJ||||||||||||F--J
;; FL-7LJLJ||||||LJL-77
;; F--JF--7||LJLJ7F7FJ-
;; L---JF-JLJ.||-FJLJJ7
;; |F|F-JF---7F7-L7L|7|
;; |FFJF7L7F-JF7|JL---7
;; 7-L-JL7||F7|L7F-7F7|
;; L.L7LFJ|||||FJL7||LJ
;; L7JLJL-JLJLJL--JLJ.L
;; Here are just the tiles that are enclosed by the loop marked with I:

;; FF7FSF7F7F7F7F7F---7
;; L|LJ||||||||||||F--J
;; FL-7LJLJ||||||LJL-77
;; F--JF--7||LJLJIF7FJ-
;; L---JF-JLJIIIIFJLJJ7
;; |F|F-JF---7IIIL7L|7|
;; |FFJF7L7F-JF7IIL---7
;; 7-L-JL7||F7|L7F-7F7|
;; L.L7LFJ|||||FJL7||LJ
;; L7JLJL-JLJLJL--JLJ.L
;; In this last example, 10 tiles are enclosed by the loop.

;; Figure out whether you have time to search for the nest by calculating the area within the loop. How many tiles are enclosed by the loop?

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

;; replace unnecessary pipes with \.
(defn replace-unnecessary-pipes
  [input]
  (let [parsed-input (process-input input)
        paths        (find-path input)]
    (vec
     (map-indexed
      (fn [idx line]
        (let [idx-to-keep (->> paths (filter (fn [[a _]] (= a idx))) (map (fn [[_ b]] b)) (into #{}))
              idxs        (remove (fn [x] (idx-to-keep x)) (range (count line)))]
          (reduce (fn [acc v] (replace-at acc v ".")) line idxs)))
      parsed-input))))

(defn start-value
  [start-x start-y parsed-input]
  (let [possible-s-values #{\| \- \7 \J \F \L}]
    (reduce
     (fn [acc [f syms]]
       (if (f parsed-input start-x start-y)
         (clojure.set/intersection acc syms)
         acc))
     possible-s-values
     [[move-up? #{\| \J \L}]
      [move-down? #{\| \7 \F}]
      [move-left? #{\- \J \7}]
      [move-right? #{\- \F \L}]])))

(defn replace-start
  [start-x start-y start-symbol parsed-input]
  (update parsed-input start-x #(replace-at % start-y (str start-symbol))))

(defn pre-process
  [input]
  (let [parsed-input        (replace-unnecessary-pipes input)
        [start-x start-y]   (find-start parsed-input)
        start-value         (first (start-value start-x start-y (process-input input)))
        pre-processed-input (replace-start start-x start-y start-value parsed-input)]
    pre-processed-input))

(defn find-index-of-dot
  [line]
  (->>  line
        (map-indexed (fn [idx ch] [idx ch]))
        (filter (fn [[_ ch]] (= ch \.)))
        (map first)))

(defn find-included
  [line]
  (let [index-of-dot (find-index-of-dot line)]
    (loop [res [] idxs index-of-dot]
      (if (seq idxs)
        (let [s (apply str (drop (first idxs) line))
              c (re-seq #"L\-*7|F\-*J|\|" s)]
          (if (even? (count c))
            (recur res (rest idxs))
            (recur (conj res (first idxs)) (rest idxs))))
        res))))

(defn day10-part2
  [input]
  (->> input
       pre-process
       (map find-included)
       (map count)
       (reduce + 0)))
