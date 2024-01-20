(ns day-03-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [day-03 :as d]
   [util :as u]))

(def input (u/load-input "day-03.txt"))

(def split-test-input ["467..114.."
                       "...*......"
                       "..35..633."
                       "......#..."
                       "617*......"
                       ".....+.58."
                       "..592....."
                       "......755."
                       "...$.*...."
                       ".664.598."])

(def test-input (str/join "\n" split-test-input))

(deftest part-1
  (is (and (d/char-is-digit? \0) (d/char-is-digit? \1)))

  (is (not (and (d/char-is-digit? \A) (d/char-is-digit? \$))))

  (is (= [["467" \6 \7 \. \. "114" \1 \4 \. \.]
          [\. \. \. \* \. \. \. \. \. \.]
          [\. \. "35" \5 \. \. "633" \3 \3 \.]
          [\. \. \. \. \. \. \# \. \. \.]
          ["617" \1 \7 \* \. \. \. \. \. \.]
          [\. \. \. \. \. \+ \. "58" \8 \.]
          [\. \. "592" \9 \2 \. \. \. \. \.]
          [\. \. \. \. \. \. "755" \5 \5 \.]
          [\. \. \. \$ \. \* \. \. \. \.]
          [\. "664" \6 \4 \. "598" \9 \8 \.]]
         (map d/parse-line split-test-input)))

  (is (= [[{:x 0, :y 0} {:data 467, :length 3}]
          [{:x 1, :y 0} {:data \6}]
          [{:x 2, :y 0} {:data \7}]
          [{:x 3, :y 0} {:data \.}]
          [{:x 4, :y 0} {:data \.}]]
         (take 5 (d/gen-coords split-test-input))))

  (is (= [{:x -1, :y 0}
          {:x 3, :y 0}
          {:x -1, :y -1}
          {:x 0, :y -1}
          {:x 1, :y -1}
          {:x 2, :y -1}
          {:x 3, :y -1}
          {:x -1, :y 1}
          {:x 0, :y 1}
          {:x 1, :y 1}
          {:x 2, :y 1}
          {:x 3, :y 1}]
         (d/get-surrounding {:x 0 :y 0 :data 467 :length 3})))

  (is (= 4361 (d/sum-parts test-input)))

  (is (= 539433 (d/sum-parts input))))

(deftest part-2
  (is (= 467835 (d/sum-gears test-input)))

  (is (= 75847567 (d/sum-gears input))))