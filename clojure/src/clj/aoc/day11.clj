(ns aoc.day11
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))


(def input (->> "input/day11.txt" io/resource slurp string/split-lines))
(def sample-input (->> "input/day11-sample.txt" io/resource slurp string/split-lines))

(defn galaxies-position
  [input]
  (map-indexed
   (fn [idx val]
     (->> val
          (map-indexed (fn [idx g] [idx g]))
          (filter (fn [[_ v]] (= v \#)))
          (map first)
          (map vector (repeat idx) )))
   input))

(defn galaxies-pair
  [n]
  (->> (for [i     (range 1 (inc n))
             j     (range 1 (inc n))
             :when (not= i j)]
         #{i j})
       (into #{})
       (map sort)
       (sort-by first)))

(defn total-galaxies
  [cosmic]
  (reduce (fn [acc val]
            (+ acc (count (re-seq #"#" val))))
          0 cosmic))

(defn galaxies-loc-map
  [input]
  (->> input
       galaxies-position
       (filter seq)
       (reduce (fn [acc val] (concat acc val)))
       (map-indexed (fn [idx val] [(inc idx) val]))
       (into {})))

(defn find-empty-rows
  [cosmic]
  (->> cosmic
       (map-indexed (fn [idx val] (when (nil? (re-seq #"#" val)) idx)))
       (remove nil?)))

(defn find-empty-cols
  [cosmic]
  (loop [idx 0 data cosmic res []]
    (if (seq (first data))
      (let [cols (->> data (map first) (filter #(= % \#)))]
        (if (empty? cols)
          (recur (inc idx) (map #(apply str %) (map rest data)) (conj res idx))
          (recur (inc idx) (map #(apply str %) (map rest data)) res)))
      res)))

(defn find-distance
  [[src dest] galaxy-no->loc empty-rows empty-cols scale-factor]
  (let [src-cord          (get galaxy-no->loc src)
        dest-cord         (get galaxy-no->loc dest)
        [[r1 c1] [r2 c2]] (sort-by first [src-cord dest-cord])
        [sc1 sc2]         (sort [c1 c2])
        empties-bw-rows   (count (filter (fn [r] (and (< r r2) (> r r1))) empty-rows))
        empties-bw-cols   (count (filter (fn [c] (and (< c sc2) (> c sc1))) empty-cols))]
    (+ (+ (- (abs (- r2 r1)) empties-bw-rows) (* scale-factor empties-bw-rows))
       (+ (- (abs (- c2 c1)) empties-bw-cols) (* scale-factor empties-bw-cols)))))


(defn day11-part1
  [input]
  (let [galaxies->loc (galaxies-loc-map input)
        empty-rows    (find-empty-rows input)
        empty-cols    (find-empty-cols input)
        pairs         (->> input total-galaxies galaxies-pair)]
    (reduce (fn [acc pair]
              (+ acc (find-distance pair galaxies->loc empty-rows empty-cols 2))) 0 pairs)))

(defn day11-part2
  [input]
  (let [galaxies->loc (galaxies-loc-map input)
        empty-rows    (find-empty-rows input)
        empty-cols    (find-empty-cols input)
        pairs         (->> input total-galaxies galaxies-pair)]
    (reduce (fn [acc pair]
              (+ acc (find-distance pair galaxies->loc empty-rows empty-cols 1000000))) 0 pairs)))
