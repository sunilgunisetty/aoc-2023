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

#_(defn traverse
    [input]
    (loop [seen   #{}
           result []
           queue  [[:right [0 0]]]]
      (if (seq queue)
        (let [[curr-direction position] (first queue)
              current-ch                (get-in input position)]
          (if current-ch
            (if-not (contains? seen [curr-direction position])
              (recur (conj seen [curr-direction position]) (conj result position) (concat (into [] (rest queue)) (direction curr-direction current-ch position)))
              (recur seen result (into [] (rest queue))))
            (recur seen result (into [] (rest queue)))))
        [result seen])))


(defn traverse
  [input]
  (loop [seen  #{}
         queue [[:right [0 0]]]]
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

(defn day16-part1
  [input]
  (->> input traverse (map second) (into #{}) count))
