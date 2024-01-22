(ns day-09-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-09 :as d]
   [util :as u]))

(def input (u/load-input "day-09.txt"))

(def split-test-input ["0 3 6 9 12 15"
                       "1 3 6 10 15 21"
                       "10 13 16 21 30 45"])

(def test-input (str/join "\n" split-test-input))

(deftest part-1
  (let [expected-parsed-line [10 13 16 21 30 45]]
    (is (= expected-parsed-line (d/parse-line (last split-test-input))))

    (is (= 68 (d/predict-next-values [(last expected-parsed-line)] expected-parsed-line)))

    (is (= [3 3 5 9 15] (d/diff-values expected-parsed-line))))

  (is (= 114 (d/sum-extrapolated-values test-input)))

  (is (= 1974232246 (d/sum-extrapolated-values input))))

(deftest part-2
  (let [expected-parsed-line [10 13 16 21 30 45]]
    (is (= 40 (d/predict-prev-values [(last expected-parsed-line)] expected-parsed-line))))

  (is (= 2 (d/sum-extrapolated-values' test-input)))

  (is (= 928 (d/sum-extrapolated-values' input))))