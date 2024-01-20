(ns day-01-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-01 :as d]
   [util :as u]))

(def input (u/load-input "day-01.txt"))

(deftest part-1
  (let [split-test-input ["1abc2"
                          "pqr3stu8vwx"
                          "a1b2c3d4e5f"
                          "treb7uchet"]
        test-input (str/join "\n" split-test-input)]

    (is (= 12 (d/find-calibration-value (first split-test-input))))

    (is (= 14 (d/find-calibration-value "abc1234foo")))

    (is (= 142 (d/sum-calibration-values test-input)))

    (is (= 54927 (d/sum-calibration-values input)))))

(deftest part-2
  (let [split-test-input ["two1nine"
                          "eightwothree"
                          "abcone2threexyz"
                          "xtwone3four"
                          "4nineeightseven2"
                          "zoneight234"
                          "7pqrstsixteen"]
        test-input (str/join "\n" split-test-input)]

    (is (= 29 (d/find-calibration-value' (first split-test-input))))

    (is (= 281 (d/sum-calibration-values' test-input)))

    (is (= 54581 (d/sum-calibration-values' input)))))