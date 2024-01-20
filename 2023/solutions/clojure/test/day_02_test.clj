(ns day-02-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-02 :as d]
   [util :as u]))

(def input (u/load-input "day-02.txt"))

(def split-test-input
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(def test-input (str/join "\n" split-test-input))

(def limits {:red 12 :green 13 :blue 14})

(deftest part-1
  (is (= {:green 8 :blue 1}  (d/parse-hand "1 blue, 8 green")))

  (is (= [1 [{:red 4 :blue 3} {:red 1 :green 2 :blue 6} {:green 2}]]
         (d/parse-game (first split-test-input))))

  (is (= true (->> (first split-test-input)
                   d/parse-game
                   (d/possible-game? limits))))

  (is (= false (->> (nth split-test-input 2)
                    d/parse-game
                    (d/possible-game? limits))))

  (is (= 8 (d/total-bounded-ids test-input)))

  (is (= 2164 (d/total-bounded-ids input))))

(deftest part-2
  (is (= {:red 4 :green 2 :blue 6}
         (->> (first split-test-input)
              d/parse-game
              second
              d/get-max-hand)))

  (is (= 2286 (d/total-product-maxes test-input)))

  (is (= 69929 (d/total-product-maxes input))))