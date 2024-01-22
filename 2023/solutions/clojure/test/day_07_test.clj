(ns day-07-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-07 :as d]
   [util :as u]))

(def input (u/load-input "day-07.txt"))

(def split-test-input ["32T3K 765"
                       "T55J5 684"
                       "KK677 28"
                       "KTJJT 220"
                       "QQQJA 483"])

(def test-input (str/join "\n" split-test-input))

(deftest part-1
  (let [first-hand (-> split-test-input
                       first
                       (str/split #" ")
                       first)]
    (is (= "32:3=" (d/masssage-hand-str first-hand)))

    (is (= 2 (d/get-hand-type first-hand))))

  (is (= [765 2 "32:3="] (d/parse-line (first split-test-input))))

  (is (= 6440 (d/find-total-winnings test-input)))

  (is (= 250347426 (d/find-total-winnings input))))

(deftest part-2
  (let [last-hand (-> split-test-input
                      last
                      (str/split #" ")
                      first)]
    (is (= "<<<;>" (d/masssage-hand-str last-hand)))

    (is (= 4 (d/get-hand-type last-hand))))

  (is (= [483 6 "<<<0>"] (d/parse-line' (last split-test-input))))

  (is (= 5905 (d/find-total-winnings' test-input)))

  (is (= 251224870 (d/find-total-winnings' input))))