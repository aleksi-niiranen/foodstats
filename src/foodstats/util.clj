(ns foodstats.util
  (:require [clojure.string :as cstr])
  (:import (java.util Calendar GregorianCalendar)))

(def month-names 
  ["January"
   "February"
   "March"
   "April"
   "May"
   "June"
   "July"
   "August"
   "September"
   "October"
   "November"
   "December"])

(defn name-of-month
  "Takes the sequence number of a month and returns the name of that month."
  [m]
  (month-names (dec m)))

(defn month-and-year
  "Takes a date vector and returns a string with the months name and year."
  [[d m y]]
  (str (name-of-month m) " " y))

(defn cal
  "Returns a mutable calendar instance."
  ([] (. GregorianCalendar getInstance))
  ([d m y]
   (let [gc (. GregorianCalendar getInstance)]
     (doto gc
       (.set Calendar/YEAR y)
       (.set Calendar/MONTH (dec m))
       (.set Calendar/DATE d))
     gc)))

(defn reader
  "Reads csv file."
  [f]
  (cstr/split (slurp f) #"\n,{3}\n"))

