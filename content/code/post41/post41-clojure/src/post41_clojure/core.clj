(ns post41-clojure.core
  (:require [clojure.string :as str])
  (:gen-class))


(defn convert-row
  "Converts one row (vector of string) to a vector [Float Int Int Int]
  It is expected that the vector consists of exactly 4 elements."
  [vec]
  (let [t  (Double/parseDouble (get vec 0))
        d1 (Integer/parseInt   (get vec 1))
        d2 (Integer/parseInt   (get vec 2))
        v  (Integer/parseInt   (get vec 3))]
    [t d1 d2 v]))

(defn get-col
  "Extract the n-th column from a vector of vectors"
  [n vec]
  (mapv (fn [v] (get v n)) vec))

(defn read-data
  "Reads the contents of the given file, splits it into lines and then splits
  each row into its column values (still strings). Finally the values are cast
  (converted) to the corresponding types."
  [filename]
  (map convert-row
    (rest
      (map #(str/split % #",")
           (str/split-lines (slurp filename))))))

(defn make-time-diffs
  "Create a new vector from a vector of time values where the previous time is
  subtracted from the successor, thus leading a time difference vector."
  [times]
  (map vector (range)
    (map #(- (first %) (second %))
      (map list (subvec times 1) (subvec times 0 (- (count times) 1))))))

(defn make-package-indices
  "Finds the start and end indices of the individual data packages based on the
  threshold logic."
  [time_diffs]
  (into [0]
    (vec
      (mapv #(+ (first %) 1)
        (filterv (fn [v] (> (second v) 1e-4)) time_diffs)))))

(defn make-package-bounds
  "Creates the data package boundary pairs (start, end) which delimit the range
  of one data package in the data stream"
  [pkg_idxs]
  (map vector (subvec pkg_idxs 0 (- (count pkg_idxs) 1)) (subvec pkg_idxs 1)))

(defn extract-data
  "Extract the data from the given data vectors based on the given bounds and a
  predicate to filter the bounds"
  [pred bounds data]
  (let [data_bounds (keep-indexed #(if (pred %1) %2) bounds)]
    (for [[start end] data_bounds]
      (for [idx (range start end)]
        (get data idx)))))

(defn -main
  "Main program for the demo project."
  [& args]
  (let [filename (first args)
        values (read-data filename)
        times (get-col 0 values)
        raw_data1 (get-col 1 values)
        raw_data2 (get-col 2 values)
        valid (get-col 3 values)
        time_diffs (make-time-diffs times)
        pkg_idxs (make-package-indices time_diffs)
        pkg_bounds (make-package-bounds pkg_idxs)
        data1 (extract-data even? pkg_bounds raw_data1)
        data2 (extract-data odd? pkg_bounds raw_data2)]
    (doseq [string ["Data1" data1 "Data2" data2]]
      (println string))))
