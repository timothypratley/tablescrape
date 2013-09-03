(ns tablescrape.data
  (:require [clj-time.core]
            [clj-time.format :refer :all]
            [net.cgrand.enlive-html :refer :all]))


; This code is intended to be run interactively via a REPL
; or inline using Light Table

; Get the data so we can experiment with it

(def t
  (select (html-resource
           (java.net.URL. "http://www.multpl.com/table?f=m"))
          [:table#datatable]))
(clojure.pprint/pprint t)

(def rows
  (for [tr (select t [:tr])
       :let [row (select tr [:td])]
       :when (seq row)]
   row))
(clojure.pprint/pprint rows)

(defn contents [x]
  (map (comp first :content) x))

(map contents rows)

; Pretty close to what we want, but everything is a string
; we need some string parsing functions to extract the data

(defn date-parser
  "Create a date parsing function for a format"
  [format-string]
  #(parse (formatter format-string) %))

(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [^String s]
  (if (re-find #"^-?\d+\.?\d*([Ee]\+\d+|[Ee]-\d+|[Ee]\d+)?$"
               (.trim s))
    (read-string s)))

(defn parse-money
  "Reads a number from a string, ignoring $ and , to just get the value"
  [^String s]
  (-> s
      (clojure.string/replace "$" "")
      (clojure.string/replace "," "")
      parse-number))

; At this point we have everthing we need to extract the data,
; but can we make a more generic approach?

(defn- parse-fs [fs ds]
  (for [[f d] (map vector fs ds)
        :when f]
    (f d)))

(defn- parse-table
  [table fs]
  (for [tr (select table [:tr])
        :let [row (contents (select tr [:td]))]
        :when (seq row)]
    (parse-fs fs row)))

(defn scrape-table
  "Scrapes data from a HTML table at url with CSS selector.
  fs are the parsing functions to use per column, nil indicates skip."
  [url selector fs]
  (parse-table
   (select
    (html-resource (java.net.URL. url))
    selector)
   fs))

; Try it out!

(scrape-table "http://www.multpl.com/table?f=m"
              [:table#datatable]
              [(date-parser "MMM dd, yyyy") parse-money])


; There are lots of tables on multpl that I want to get

(defn multpl [series]
  (let [url (str "http://www.multpl.com/" series)]
    (scrape-table url [:table#datatable]
                  [(date-parser "MMM dd, yyyy") parse-money])))



