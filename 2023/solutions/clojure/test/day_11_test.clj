(ns day-11-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-11 :as d]
   [util :as u]))

(def input (u/load-input "day-11.txt"))

(def split-test-input ["...#......"
                       ".......#.."
                       "#........."
                       ".........."
                       "......#..."
                       ".#........"
                       ".........#"
                       ".........."
                       ".......#.."
                       "#...#....."])

(def test-input (str/join "\n" split-test-input))

(deftest part-1
  (is (= '((\a \d \g) (\b \e \h) (\c \f \i))
         (d/transpose '((\a \b \c) (\d \e \f) (\g \h \i)))))

  (let [expanded-universe (map seq ["....#........"
                                    ".........#..."
                                    "#............"
                                    "............."
                                    "............."
                                    "........#...."
                                    ".#..........."
                                    "............#"
                                    "............."
                                    "............."
                                    ".........#..."
                                    "#....#......."])]
    (is (= expanded-universe (->> split-test-input
                                  (map seq)
                                  d/expand-universe)))

    (is (= [[0 4] [1 9] [2 0] [5 8] [6 1] [7 12] [10 9] [11 0] [11 5]]
           (d/find-galaxy-coords expanded-universe))))

  (is (= 9 (d/distance [6 1] [11 5])))

  (is (= 15 (d/distance [0 4] [10 9])))

  (is (= 17 (d/distance [2 0] [7 12])))

  (is (= 5 (d/distance [11 0] [11 5])))

  (is (= 36 (count (d/pairs [[0 4] [1 9] [2 0] [5 8] [6 1] [7 12] [10 9] [11 0] [11 5]]))))

  (is (= 374 (d/sum-distances test-input)))

  (is (= 9522407 (d/sum-distances input))))

(deftest part-2
  (let [{:keys [xs ys universe] :as expanded-universe} {:xs #{2 5 8}
                                                        :ys #{7 3}
                                                        :universe
                                                        '((\. \. \. \# \. \. \. \. \. \.)
                                                          (\. \. \. \. \. \. \. \# \. \.)
                                                          (\# \. \. \. \. \. \. \. \. \.)
                                                          (\. \. \. \. \. \. \. \. \. \.)
                                                          (\. \. \. \. \. \. \# \. \. \.)
                                                          (\. \# \. \. \. \. \. \. \. \.)
                                                          (\. \. \. \. \. \. \. \. \. \#)
                                                          (\. \. \. \. \. \. \. \. \. \.)
                                                          (\. \. \. \. \. \. \. \# \. \.)
                                                          (\# \. \. \. \# \. \. \. \. \.))}]
    (is (= expanded-universe (->> split-test-input
                                  (map seq)
                                  d/expand-universe')))

    (is (= 9 (d/distance' [5 1] [9 4] ys xs 2)))

    (is (= 15 (d/distance' [0 3] [8 7] ys xs 2)))

    (is (= 17 (d/distance' [2 0] [6 9] ys xs 2)))

    (is (= 5 (d/distance' [10 0] [10 4] ys xs 2))))

  (is (= 374 (d/sum-distances' test-input 2)))

  (is (= 1030 (d/sum-distances' test-input 10)))

  (is (= 8410 (d/sum-distances' test-input 100)))

  (is (= 544723432977 (d/sum-distances' input 1000000))))