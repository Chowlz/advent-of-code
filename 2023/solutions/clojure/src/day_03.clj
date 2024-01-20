(ns day-03
  (:require
   [clojure.string :as str]))

; Part 1

(defn char-is-digit? [c] (<= 48 (int c) 57))

(defn parse-line [line]
  (->> line
       (map-indexed vector)
       (reduce (fn [{:keys [idx] :as acc} [i char]]
                 (if (char-is-digit? char)
                   (if (nil? idx)
                     (-> acc
                         (assoc :idx i)
                         (update :line conj (str char)))
                     (-> acc
                         (update :line conj char)
                         (update-in [:line idx] str char)))
                   (if (nil? idx)
                     (update acc :line conj char)
                     (-> acc
                         (assoc :idx nil)
                         (update :line conj char)))))
               {:idx nil
                :line []})
       :line))

(defn gen-coords [input]
  (for [[y row] (map-indexed vector (map parse-line input))
        [x cell] (map-indexed vector row)
        :let [data-is-string (string? cell)
              data (if data-is-string (Integer/parseInt cell) cell)]]
    [{:x x :y y} (cond-> {:data data}
                   data-is-string (assoc :length (count cell)))]))

(defn get-surrounding [{:keys [x y length]}]
  (let [length (or length 1)
        l-bound (- x 1)
        r-bound (+ x length)]
    (concat [{:x l-bound :y y}
             {:x r-bound :y y}]
            (map (fn [x] {:x x :y (dec y)}) (range l-bound (inc r-bound)))
            (map (fn [x] {:x x :y (inc y)}) (range l-bound (inc r-bound))))))

(defn has-adjacent-symbol? [schematic coord]
  (->> coord
       get-surrounding
       (keep #(get schematic %))
       (every? #(= \. (:data %)))
       not))

(defn sum-parts [input]
  (let [coords (-> input str/split-lines gen-coords)
        schematic (into {} coords)
        has-adjacent-sym? (partial has-adjacent-symbol? schematic)
        xf (comp (filter (fn [[_ v]] (number? (:data v))))
                 (map #(apply merge %))
                 (filter has-adjacent-sym?)
                 (map :data))]
    (transduce xf + schematic)))

; Part 2

(defn get-valid-gear-part-coords [schematic coord]
  (let [coords (->> coord
                    get-surrounding
                    (keep #(get schematic %))
                    (map :ref)
                    (filter identity)
                    (into #{}))]
    (when (= 2 (count coords))
      coords)))

(defn sum-gears [input]
  (let [coords (-> input str/split-lines gen-coords)
        schematic (into {} coords)
        numbers (->> schematic
                     (filter (fn [[_ v]] (number? (:data v))))
                     (map #(apply merge %)))
        gears (->> schematic
                   (filter (fn [[_ v]] (= \* (:data v))))
                   (map #(apply merge %)))
        ref-schematic (reduce (fn [schematic {:keys [x y length]}]
                                (reduce (fn [schematic x']
                                          (assoc-in schematic [{:x x' :y y} :ref] {:x x :y y}))
                                        schematic
                                        (range x (+ x length))))
                              schematic
                              numbers)
        xf (comp (map #(get-valid-gear-part-coords ref-schematic %))
                 (filter identity)
                 (map (fn [coords]
                        (->> coords
                             (map #(get-in schematic [% :data])))))
                 (map #(apply * %)))]
    (transduce xf + gears)))