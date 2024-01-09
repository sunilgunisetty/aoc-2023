(ns aoc.day12
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))

(def sample-input (->> "input/day12-sample.txt" io/resource slurp))
(def input (->> "input/day12.txt" io/resource slurp))


;; --- Day 12: Hot Springs ---
;; You finally reach the hot springs! You can see steam rising from secluded areas attached to the primary, ornate building.

;; As you turn to enter, the researcher stops you. "Wait - I thought you were looking for the hot springs, weren't you?" You indicate that this definitely looks like hot springs to you.

;; "Oh, sorry, common mistake! This is actually the onsen! The hot springs are next door."

;; You look in the direction the researcher is pointing and suddenly notice the massive metal helixes towering overhead. "This way!"

;; It only takes you a few more steps to reach the main gate of the massive fenced-off area containing the springs. You go through the gate and into a small administrative building.

;; "Hello! What brings you to the hot springs today? Sorry they're not very hot right now; we're having a lava shortage at the moment." You ask about the missing machine parts for Desert Island.

;; "Oh, all of Gear Island is currently offline! Nothing is being manufactured at the moment, not until we get more lava to heat our forges. And our springs. The springs aren't very springy unless they're hot!"

;; "Say, could you go up and see why the lava stopped flowing? The springs are too cold for normal operation, but we should be able to find one springy enough to launch you up there!"

;; There's just one problem - many of the springs have fallen into disrepair, so they're not actually sure which springs would even be safe to use! Worse yet, their condition records of which springs are damaged (your puzzle input) are also damaged! You'll need to help them repair the damaged records.

;; In the giant field just outside, the springs are arranged into rows. For each row, the condition records show every spring and whether it is operational (.) or damaged (#). This is the part of the condition records that is itself damaged; for some springs, it is simply unknown (?) whether the spring is operational or damaged.

;; However, the engineer that produced the condition records also duplicated some of this information in a different format! After the list of springs for a given row, the size of each contiguous group of damaged springs is listed in the order those groups appear in the row. This list always accounts for every damaged spring, and each number is the entire size of its contiguous group (that is, groups are always separated by at least one operational spring: #### would always be 4, never 2,2).

;; So, condition records with no unknown spring conditions might look like this:

;; #.#.### 1,1,3
;; .#...#....###. 1,1,3
;; .#.###.#.###### 1,3,1,6
;; ####.#...#... 4,1,1
;; #....######..#####. 1,6,5
;; .###.##....# 3,2,1
;; However, the condition records are partially damaged; some of the springs' conditions are actually unknown (?). For example:

;; ???.### 1,1,3
;; .??..??...?##. 1,1,3
;; ?#?#?#?#?#?#?#? 1,3,1,6
;; ????.#...#... 4,1,1
;; ????.######..#####. 1,6,5
;; ?###???????? 3,2,1
;; Equipped with this information, it is your job to figure out how many different arrangements of operational and broken springs fit the given criteria in each row.

;; In the first line (???.### 1,1,3), there is exactly one way separate groups of one, one, and three broken springs (in that order) can appear in that row: the first three unknown springs must be broken, then operational, then broken (#.#), making the whole row #.#.###.

;; The second line is more interesting: .??..??...?##. 1,1,3 could be a total of four different arrangements. The last ? must always be broken (to satisfy the final contiguous group of three broken springs), and each ?? must hide exactly one of the two broken springs. (Neither ?? could be both broken springs or they would form a single contiguous group of two; if that were true, the numbers afterward would have been 2,3 instead.) Since each ?? can either be #. or .#, there are four possible arrangements of springs.

;; The last line is actually consistent with ten different arrangements! Because the first number is 3, the first and second ? must both be . (if either were #, the first number would have to be 4 or higher). However, the remaining run of unknown spring conditions have many different ways they could hold groups of two and one broken springs:

;; ?###???????? 3,2,1
;; .###.##.#...
;; .###.##..#..
;; .###.##...#.
;; .###.##....#
;; .###..##.#..
;; .###..##..#.
;; .###..##...#
;; .###...##.#.
;; .###...##..#
;; .###....##.#
;; In this example, the number of possible arrangements for each row is:

;; ???.### 1,1,3 - 1 arrangement
;; .??..??...?##. 1,1,3 - 4 arrangements
;; ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
;; ????.#...#... 4,1,1 - 1 arrangement
;; ????.######..#####. 1,6,5 - 4 arrangements
;; ?###???????? 3,2,1 - 10 arrangements
;; Adding all of the possible arrangement counts together produces a total of 21 arrangements.

;; For each row, count all of the different arrangements of operational and broken springs that meet the given criteria. What is the sum of those counts?


(defn process-input
  [input]
  (->> input
       string/split-lines
       (map #(string/split % #" "))
       (map (fn [[pattern groups]]
              [(string/replace pattern #"\.{2,}" ".")
               (mapv #(Integer/parseInt %) (string/split groups #","))]))))

;; Base case
;; If ways vector is empty and pattern includes #, return 0 -> This means there is a another broken spring hence invalid
;; If ways vector is empty and pattern does not include # -> This means we successfully processed input return 1
;; If length of pattern is less than all the sum of ways then this is invalid and return 0. ex if length of pattern is 5 and
;; [1 2] then 5 < [sum of [1 2] + lenght [1 2] - 1]
;; else dispatch of \. \# \?
;;   \. skip the character and move on to next one
;;   \#
;;       - if length pattern less than first group and first group of characters from start of pattern does not contain \. (valid spring)
;;         than this combination is invalid and return 0
;;       - if length of pattern is > first group and next symbol after the first group is = \# (broken spring) than its invalid and return 0
;;       - if length of pattern = first item of group and only one item left in group than this is valid combination and return 1
;;       - else remove first group + 1 elements from pattern and remove first element from group and recur
;;   \? For this we have to condider no of ways if \? is replaced with \# and no of ways if we don'e consider this character

(defn process-hash
  [pattern ways]
  (cond
    (or (< (count pattern) (first ways))
        (not (empty? (filter #(= % \.) (take (first ways) pattern)))))                0
    (and (> (count pattern) (first ways)) (= (first (drop (first ways) pattern)) \#)) 0
    (and (= (count pattern) (first ways)) (= (count ways)) 1)                         1
    :else
    (process-pattern (apply str (drop (inc (first ways)) pattern)) (rest ways))))

(def memo-process-hash (memoize process-hash))

(defn process-pattern
  [pattern ways]
  (cond
    (and (empty? ways) (not (string/includes? pattern "#")))  1
    (and (empty? ways) (string/includes? pattern "#"))        0
    (< (count pattern) (+ (dec (count ways)) (apply + ways))) 0
    :else
    (condp = (first pattern)
      \. (process-pattern (apply str (rest pattern)) ways)
      \# (memo-process-hash pattern ways)
      \? (+ (process-pattern (apply str (rest pattern)) ways) (memo-process-hash pattern ways)))))

(def memo-process-pattern (memoize process-pattern))

(defn calculate-result
  [processed-input]
  (reduce (fn [acc [pattern groups]] (+ acc (memo-process-pattern pattern groups))) 0 processed-input))

(defn day12-part1
  [input]
  (->> input
       process-input
       calculate-result))


;; --- Part Two ---
;; As you look out at the field of springs, you feel like there are way more springs than the condition records list. When you examine the records, you discover that they were actually folded up this whole time!

;; To unfold the records, on each row, replace the list of spring conditions with five copies of itself (separated by ?) and replace the list of contiguous groups of damaged springs with five copies of itself (separated by ,).

;; So, this row:

;; .# 1
;; Would become:

;; .#?.#?.#?.#?.# 1,1,1,1,1
;; The first line of the above example would become:

;; ???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3
;; In the above example, after unfolding, the number of possible arrangements for some rows is now much larger:

;; ???.### 1,1,3 - 1 arrangement
;; .??..??...?##. 1,1,3 - 16384 arrangements
;; ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
;; ????.#...#... 4,1,1 - 16 arrangements
;; ????.######..#####. 1,6,5 - 2500 arrangements
;; ?###???????? 3,2,1 - 506250 arrangements
;; After unfolding, adding all of the possible arrangement counts together produces 525152.

;; Unfold your condition records; what is the new sum of possible arrangement counts?

(defn process-input-part-2
  [input]
  (->> input
       process-input
       (map (fn [[pattern groups]]
              [(string/join "?" (repeat 5 pattern))
               (apply concat (repeat 5 groups))]))))

(defn day12-part2
  [input]
  (->> input
       process-input-part-2
       calculate-result))



;; TODO refactor this code
