(ns day-06-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-06 :as d]
   [util :as u]))

(def input (u/load-input "day-06.txt"))

(def split-test-input ["Time:      7  15   30"
                       "Distance:  9  40  200"])

(def test-input (str/join "\n" split-test-input))

(deftest part-1
  (is (= [7 15 30] (d/parse-line (first split-test-input) #"Time:\s+")))

  (is (= [9 40 200] (d/parse-line (last split-test-input) #"Distance:\s+")))

  (is (= {7 9 15 40 30 200} (d/parse-input split-test-input)))

  (is (= 4 (d/count-win-possibilities 7 9)))

  (is (= 288 (d/multiply-win-possibilities test-input)))

  (is (= 6209190 (d/multiply-win-possibilities input))))

(deftest part-2
  (is (= 71530 (d/parse-line' (first split-test-input) #"Time:\s+")))

  (is (= 940200 (d/parse-line' (last split-test-input) #"Distance:\s+")))

  (is (= [71530 940200] (d/parse-input' split-test-input)))

  (is (= 71503 (d/find-win-possibilities test-input)))

  (is (= 28545089 (d/find-win-possibilities input))))