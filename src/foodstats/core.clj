(ns foodstats.core
  (:require [clojure.string :as cstr]
            [foodstats.util :refer :all]))

(defrecord Purchase [date product price])

(defn entry
  [date [product price]]
  (let [splt (cstr/split date #"\.")
        [d m y] (map #(Integer/parseInt %) splt)]
    (->Purchase [d m y] product price)))

(defn parse-item
  [[c :as s]] ;first letter of string as char
  (let [toNum #(Double/parseDouble (cstr/replace (cstr/replace % \, \.) \− \-))]
    (cond
      (= c \") (let [[i _ p] (re-seq #"[\w\,\-\h]+" s)] [i (toNum p)])
      :else [(first (re-seq #"[^,]+" s)) (toNum (first (re-seq #"−?\d+,\d+" s)))])))

(defn parse-block
  [s]
  (let [date (first (re-seq #"\d{1,2}\.\d{1,2}\.\d{4}" s))
        items (rest (cstr/split s #"\n"))]
    (map (fn [l] (entry date (parse-item l))) items)))

(defn inc-in-map
  "Increments value of a key in map or inserts value 1 to key."
  [k coll]
  (let [e (coll k)]
    (if e (assoc coll k (inc e)) (conj coll [k 1]))))

(defn pairs
  [coll k1 k2]
  (map (fn [{k k1 v k2}] [k v]) coll))

(defn values-to-seq
  "Takes a collection of maps and a key and returns a lazy seq of values in that key. Optionally takes a function that is called with each value as parameter."
  ([coll k]
   (map #(k %) coll))
  ([coll k f]
   (map #(f (k %)) coll)))

(defn better
  "Takes a function and two map entries. Function must take two parameters and return true or false. The values passed to the function are the values from two map entry parameters. Returns the one with greater value."
  [f [_ v1 :as x] [_ v2 :as y]]
  (if (f v1 v2) x y))

(defn find-best-value
  "Takes a map or a collection of map entries and a function that compares two values. Function must return true or false. The first value passed into the function is the next entry, and the second is the entry with current best value. Returns the map entry that containts the best value. E.g. the > function will produce the map entry with highest value."
  ([coll f]
  (let [[e1 e2 & tail] (seq coll) pf (partial better f)]
    (if (nil? e2) 
      e1
      (reduce #(pf %1 %2) (pf e1 e2) tail)))))

(defn count-groupby
  "Takes a collection maps and returns a map of months in year and count of occurrences."
  [coll]
  (reduce #(inc-in-map %2 %1) {} (values-to-seq coll :date month-and-year)))

(defn middle
  "Takes the count of a collection and returns the middle index or indices in a vector."
  [c]
  (let [half (/ c 2) coerced (int half)]
    (if (integer? half)
      [coerced (inc coerced)]
      [coerced])))

(defn high
  [coll]
  (find-best-value (pairs coll :date :price) >))

(defn low
  [coll]
  (find-best-value (pairs coll :date :price) <))

(defn biggest-diff
  "Finds the biggest difference in prices. Returns a vector with highest and lowest values, and their difference."
  [coll]
  (let [[_ h] (high coll) [_ l] (low coll)]
    [h l (- h l)]))

(defn middle-or-average
  "Takes a vector and one or two indices and returns the value at that index or the average of two indices."
  ([coll i] (coll i))
  ([coll i1 i2] (/ (+ (coll i1) (coll i2)) 2)))

(defn median-price
  [coll]
  (let [i (middle (count coll))
        sorted (vec (sort (values-to-seq coll :price)))]
    (apply (partial middle-or-average sorted) i)))

(defn sum
  [coll]
  (apply + (values-to-seq coll :price)))

(defn most
  [coll]
  (find-best-value (count-groupby coll) >))

(defn least
  [coll]
  (find-best-value (count-groupby coll) <))

(defn date-pred
  [[d1 m1 y1] [d2 m2 y2]]
  (cond (< y1 y2) true
        (and (= y1 y2) (< m1 m2)) true
        (and (= y1 y2) (= m1 m2) (< d1 d2)) true))

(defn latest
  [coll]
  (let [dates (values-to-seq coll :date)]
    (last (sort (comparator date-pred) dates))))

