(ns day-04-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-04 :as d]
   [util :as u]))

(def input (u/load-input "day-04.txt"))

(def split-test-input ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                       "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                       "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                       "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                       "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                       "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(def test-input (str/join "\n" split-test-input))

(deftest part-1
  (is (= {:hand #{86 48 41 17 83}, :win #{86 48 31 6 17 9 83 53}}
         (d/parse-line (first split-test-input))))

  (is (= 13 (d/sum-points test-input)))

  (is (= 23941 (d/sum-points input))))

(deftest part-2
  (is (= 30 (d/sum-cards test-input)))

  (is (= 5571760 (d/sum-cards input))))