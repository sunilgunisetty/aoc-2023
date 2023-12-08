(ns aoc.others
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))


(defn parse-line [line]
  (let [[_ n target actual] (re-matches #"Card\W+(\d+): (.+) \| (.+)" line)]
    {:n (parse-long n)
     :target (mapv parse-long (str/split target #"\W+"))
     :actual (mapv parse-long (str/split actual #"\W+"))}))

(defn read-input [filename]
  (let [lines (str/split-lines (slurp (io/resource filename)))]
    (mapv parse-line lines)))

(def full-input (read-input "input/day4.txt"))
#_(def sample1 (read-input "day04/sample1.txt"))


(defn sum [ns]
  (reduce + 0 ns))

(defn score [nmatches]
  (int (Math/pow 2 (dec nmatches))))

(defn count-matches [card]
  (let [target? (set (:target card))]
    (count (filter target? (:actual card)))))

(defn simple-score [card]
  (score (count-matches card)))

(defn part1 [input]
  (sum (map simple-score input)))

(defn add-to-counts [counts n wins]
  ;; figure out cards I win, making sure such a card exists
  (let [cards-to-win (filter counts (range (+ 1 n) (+ 1 n wins)))]
    ;; add (counts n) copies to each card I win
    (reduce (fn [counts n']
              (update counts n' #(+ % (counts n))))
            counts
            cards-to-win)))

(defn part2 [input]
  ;; counts is map of card number to the number of copies of card we have
  ;; start with 1 copy of each card
  ;; if I have x copies of a card and y wins, add x to the next y cards
  (loop [cards input
         counts (zipmap (range 1 (inc (count input)))
                        (repeatedly (constantly 1)))]
    (if (empty? cards)
      (sum (vals counts)) ;; sum up all the copies
      (let [card (first cards)
            wins (count-matches card)]
        (recur (rest cards)
               (add-to-counts counts (:n card) wins))))))
