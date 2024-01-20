(ns util
  (:require
   [clojure.java.io :as io]))

(defn load-input [input]
  (->> input
       (io/file "../../inputs")
       slurp))