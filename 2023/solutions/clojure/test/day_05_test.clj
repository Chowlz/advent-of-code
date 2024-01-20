(ns day-05-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-05 :as d]
   [util :as u]))

(def input (u/load-input "day-05.txt"))

(def test-input (str/join "\n" ["seeds: 79 14 55 13"
                                ""
                                "seed-to-soil map:"
                                "50 98 2"
                                "52 50 48"
                                ""
                                "soil-to-fertilizer map:"
                                "0 15 37"
                                "37 52 2"
                                "39 0 15"
                                ""
                                "fertilizer-to-water map:"
                                "49 53 8"
                                "0 11 42"
                                "42 0 7"
                                "57 7 4"
                                ""
                                "water-to-light map:"
                                "88 18 7"
                                "18 25 70"
                                ""
                                "light-to-temperature map:"
                                "45 77 23"
                                "81 45 19"
                                "68 64 13"
                                ""
                                "temperature-to-humidity map:"
                                "0 69 1"
                                "1 0 69"
                                ""
                                "humidity-to-location map:"
                                "60 56 37"
                                "56 93 4"]))

(deftest part-1
  (let [[seeds-str & map-strs] (str/split test-input #"\n\n")
        mappings (->> map-strs
                      (map d/parse-mapping)
                      (into {}))]
    (is (= [79 14 55 13] (d/parse-seeds seeds-str)))

    (is (= {:seed-to-soil [[[98 99] 50] [[50 97] 52]]}
           (d/parse-mapping (first map-strs))))

    (is (= 1 (d/find-des [[[98 99] 50] [[50 97] 52]] 1)))

    (is (= 82 (d/seed->location mappings 79))))

  (is (= 35 (d/find-lowest-location test-input)))

  (is (= 806029445 (d/find-lowest-location input))))

(deftest part-2
  (let [[seeds-str & _] (str/split test-input #"\n\n")
        rules [[[98 99] 50] [[50 97] 52]]]
    (is (= [[79 92] [55 67]] (d/gen-seed-ranges seeds-str)))

    (is (= [[81 94] [57 69]] (d/find-all-des-ranges rules (d/gen-seed-ranges seeds-str)))))

  (is (= 46 (d/find-lowest-location-from-ranges test-input)))

  (is (= 59370572 (d/find-lowest-location-from-ranges input))))