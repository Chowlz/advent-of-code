(ns day-08-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-08 :as d]
   [util :as u]))

(def input (u/load-input "day-08.txt"))

(deftest part-1
  (let [split-test-input ["RL"
                          ""
                          "AAA = (BBB, CCC)"
                          "BBB = (DDD, EEE)"
                          "CCC = (ZZZ, GGG)"
                          "DDD = (DDD, DDD)"
                          "EEE = (EEE, EEE)"
                          "GGG = (GGG, GGG)"
                          "ZZZ = (ZZZ, ZZZ)"]
        test-input (str/join "\n" split-test-input)
        [_ _ & rest] split-test-input]
    (is (= {"AAA" ["BBB" "CCC"]
            "BBB" ["DDD" "EEE"]
            "CCC" ["ZZZ" "GGG"]
            "DDD" ["DDD" "DDD"]
            "EEE" ["EEE" "EEE"]
            "GGG" ["GGG" "GGG"]
            "ZZZ" ["ZZZ" "ZZZ"]}
           (d/parse-lines rest)))

    (is (= 2 (d/find-num-steps test-input)))

    (is (= 11567 (d/find-num-steps input))))

  )

(deftest part-2
  (let [split-test-input ["LR"
                          ""
                          "11A = (11B, XXX)"
                          "11B = (XXX, 11Z)"
                          "11Z = (11B, XXX)"
                          "22A = (22B, XXX)"
                          "22B = (22C, 22C)"
                          "22C = (22Z, 22Z)"
                          "22Z = (22B, 22B)"
                          "XXX = (XXX, XXX)"]
        test-input (str/join "\n" split-test-input)
        [_ _ & rest] split-test-input]
    (is (= {"11A" ["11B" "XXX"]
            "11B" ["XXX" "11Z"]
            "11Z" ["11B" "XXX"]
            "22A" ["22B" "XXX"]
            "22B" ["22C" "22C"]
            "22C" ["22Z" "22Z"]
            "22Z" ["22B" "22B"]
            "XXX" ["XXX" "XXX"]}
           (d/parse-lines rest)))


    (is (= 2 (d/count-steps "LR"
                            {"11A" ["11B" "XXX"]
                             "11B" ["XXX" "11Z"]
                             "11Z" ["11B" "XXX"]
                             "22A" ["22B" "XXX"]
                             "22B" ["22C" "22C"]
                             "22C" ["22Z" "22Z"]
                             "22Z" ["22B" "22B"]
                             "XXX" ["XXX" "XXX"]}
                            "11A")))

    (is (= 6 (d/least-commong-multiple [2 3])))

    (is (= 6 (d/find-num-steps' test-input)))

    (is (= 9858474970153 (d/find-num-steps' input)))))