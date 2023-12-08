(ns aoc.day3
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))

;; --- Day 3: Gear Ratios ---
;; You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

;; It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

;; "Aaah!"

;; You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

;; The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

;; The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

;; Here is an example engine schematic:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..
;; In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

;; Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

(defn find-neighbors
  [curr-idx [start end]]
  (concat
   [[curr-idx (dec start)]
    [curr-idx (inc end)]]
   (mapv (fn [v] [(dec curr-idx) v]) (range (dec start) (+ 2 end)))
   (mapv (fn [v] [(inc curr-idx) v]) (range (dec start) (+ 2 end)))))

(defn filter-engine-part-numbers
  [idx-map current-idx [val position]]
  (let [neighbors (find-neighbors current-idx position)]
    (if (seq (->> neighbors (map #(get-in idx-map %)) (remove nil?) (remove #{\.})))
      (Integer/parseInt val)
      0)))

(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn process-line
  [idx-map curr-idx line]
  (let [numbers (some->> line
                         (re-pos #"\d+")
                         (map (fn [[idx val]]
                                [val [idx (+ idx (dec (count val)))]])))]
    (some->> numbers (map #(filter-engine-part-numbers idx-map curr-idx %)) (apply +))))

(defn day-3-part-1
  []
  (with-open [rdr (io/reader (io/resource "input/day3.txt"))]
    (let [res (map-indexed (fn [idx item] [idx item]) (doall (line-seq rdr)))]
      (reduce (fn [acc [curr-idx line]]
                (let [result (process-line (into {} res) curr-idx line)]
                  (+ acc (or result 0)))
                )
              0 res))))

;; (day-3-part-1)

;; --- Part Two ---
;; The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.

;; You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.

;; Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.

;; The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.

;; This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.

;; Consider the same engine schematic again:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..
;; In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces 467835.

;; What is the sum of all of the gear ratios in your engine schematic?



(defn construct-position-gear
  [current-idx position]
  [[current-idx (dec position)]
   [current-idx (inc position)]
   [(dec current-idx) (dec position)]
   [(dec current-idx)  position]
   [(dec current-idx)  (inc position)]
   [(inc current-idx) (dec position)]
   [(inc current-idx)  position]
   [(inc current-idx)  (inc position)]])

(defn find-gear-ratio
  [no-map positions]
  (let [r
        (->> positions
             (map #(get-in no-map %))
             (remove nil?)
             (into #{})
             (map #(Integer/parseInt %)))]
    (if (= (count r) 2)
      (apply * r)
      0)))

(defn process-line-gear
  [no-map curr-idx line]
  (let [star-positions (->> line
                            (re-pos #"\*")
                            (mapcat (fn [[v _]] [v] )))
        positions (map
                   #(construct-position-gear curr-idx %) star-positions)]
    (map #(find-gear-ratio no-map %) positions)))

(defn construct-range
  [[k v]]
  (map vector (range k (+ k(count v))) (repeat v)))

(defn preprocess-numbers
  [input]
  (->> input
       (map #(re-pos #"\d+" %))
       (map-indexed (fn [idx val]
                      [idx (into {} (mapcat construct-range val))]))
       (into {})))

(defn gear-ratio
  [input]
  (let [res (map-indexed (fn [idx item] [idx item]) input)
        no-map (preprocess-numbers input)]
    (reduce (fn [acc [curr-idx line]]
              (apply + acc (process-line-gear no-map curr-idx line)))
            0
            res)))

(defn day-3-part-2
  []
  (with-open [rdr (io/reader (io/resource "input/day3.txt"))]
    (gear-ratio (line-seq rdr))))

;; (day-3-part-2)
