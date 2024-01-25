(ns aoc.day16
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))

(def sample-input (->> "input/day16-sample.txt" io/resource slurp string/split-lines))

(def input (->> "input/day16.txt" io/resource slurp string/split-lines))


(defn north [x y] [(dec x) y])

(defn south [x y] [(inc x) y])

(defn west [x y] [x (dec y)])

(defn east [x y] [x (inc y)])

(defn direction
  [travelling-direction ch [x y]]
  (cond
    (and (= travelling-direction :right) (#{\. \-} ch)) [[:right (east x y)]]
    (and (= travelling-direction :right) (#{\\} ch))    [[:down (south x y)]]
    (and (= travelling-direction :right) (#{\/} ch))    [[:up (north x y)]]
    (and (= travelling-direction :right) (#{\|} ch))    [[:up (north x y)] [:down (south x y)]]
    (and (= travelling-direction :left)  (#{\. \-} ch)) [[:left (west x y)]]
    (and (= travelling-direction :left)  (#{\\} ch))    [[:up (north x y)]]
    (and (= travelling-direction :left)  (#{\/} ch))    [[:down (south x y)]]
    (and (= travelling-direction :left)  (#{\|} ch))    [[:up (north x y)] [:down (south x y)]]
    (and (= travelling-direction :up)    (#{\. \|} ch)) [[:up (north x y)]]
    (and (= travelling-direction :up)    (#{\\} ch))    [[:left (west x y)]]
    (and (= travelling-direction :up)    (#{\/} ch))    [[:right (east x y)]]
    (and (= travelling-direction :up)    (#{\-} ch))    [[:right (east x y)] [:left (west x y)]]
    (and (= travelling-direction :down)  (#{\. \|} ch)) [[:down (south x y)]]
    (and (= travelling-direction :down)  (#{\\} ch))    [[:right (east x y)]]
    (and (= travelling-direction :down)  (#{\/} ch))    [[:left (west x y)]]
    (and (= travelling-direction :down)  (#{\-} ch))    [[:right (east x y)] [:left (west x y)]]
    :else
    (throw
     (Exception.
      (str "Invalid Option: Travelling Direction: " travelling-direction ", Ch: "ch ", X: "x ", Y: " y)))))

(defn traverse
  [entry input]
  (loop [seen  #{}
         queue [entry]]
    (if (seq queue)
      (let [[curr-direction position] (first queue)
            current-ch                (get-in input position)]
        (if current-ch
          (if-not (contains? seen [curr-direction position])
            (recur
             (conj seen [curr-direction position])
             (concat (into [] (rest queue)) (direction curr-direction current-ch position)))
            (recur seen (into [] (rest queue))))
          (recur seen (into [] (rest queue)))))
      seen)))

(defn tiles-energized
  [entry input]
  (->> input (traverse entry) (map second) (into #{}) count))

(defn day16-part1
  [input]
  (tiles-energized [:right [0 0]] input))

(defn right-entry
  [input]
  (map (fn [v] [:right v]) (map vector (range (count input)) (repeat 0) )))

(defn left-entry
  [input]
  (map (fn [v] [:left v]) (map vector (range (count input)) (repeat (dec (count input))))))

(defn top-entry
  [input]
  (map (fn [v] [:down v]) (map vector (repeat 0) (range (count input)))))

(defn bottom-entry
  [input]
  (map (fn [v] [:up v]) (map vector (repeat (dec (count input))) (range (count input)))))

;; Takes 7.5 seconds
(defn day16-part2
  [input]
  (let [entries (mapcat (fn [f] (f input)) [top-entry bottom-entry left-entry right-entry])]
    (apply max (map #(tiles-energized % input) entries))))
