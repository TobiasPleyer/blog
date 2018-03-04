(ns post41-clojure.core
  (:gen-class))

(require '[clojure.string :as str])

(defn read-data
  "Reads CSV data from the given file and creates a vector of tuples"
  [filename]
  (take 5
    (map #(str/split % #",")
      (rest (str/split-lines (slurp filename))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [filename (first args)
        lines (read-data filename)]
    (for [line lines] (println line))))
