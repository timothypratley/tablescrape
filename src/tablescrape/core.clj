(ns tablescrape.core
  (:use [tablescrape.data]
        [incanter core charts stats]
        [clj-time.coerce]))


(def sap (multpl "s-p-500-price/table?f=m"))
(def pe (multpl "table?f=m"))
(def spe (multpl "shiller-pe/table?f=m"))

; are the series the same length?
(def counts (map count [sap pe spe]))
counts
(def m (apply min counts))

; do all the dates align?
(= (map first sap) (map first pe) (map first spe))

; do the dates that we have align?
(= (map #(take m (map first %)) [sap pe spe]))

; plot an overlay chart
(let [date (map (comp to-long first) sap)
      chart (time-series-plot date (series sap)
                              :title "Overlay"
                              :y-label "S&P 500 index"
                              :x-label "Date"
                              :series-label "S&P"
                              :legend true)
      axis2 (org.jfree.chart.axis.NumberAxis. "Ratio")
      plot (.getXYPlot chart)]
  (.setRangeAxis plot 1 axis2)
  (doto chart
     (add-lines date (series pe) :series-label "PE")
     (add-lines date (series spe) :series-label "Shiller PE"))
  (.mapDatasetToRangeAxis plot 1 1)
  (.mapDatasetToRangeAxis plot 2 1)
  (view chart))

(view (histogram (series pe) :nbins 50 :title "Distribution of PE" :x-label "PE"))
(view (histogram (series spe) :nbins 50 :title "Distribution of Shiller PE" :x-label "SPE"))


(defn series
  "Ignore the first value as it is the current estimate.
  Take only as many as will match the other series, length m.
  And select only the values (not the dates)."
  [s]
  (rest (take m (map second s))))
(series sap)

(def ss (series sap))
(def monthly-gains (map / ss (rest ss)))
(def yearly-gains (map / ss (drop 12 ss)))
(def five-year-gains (map / ss (drop 60 ss)))
(mean five-year-gains)

(view (histogram monthly-gains :nbins 50 :title "Monthly gains"))
(view (histogram yearly-gains :nbins 50 :title "Yearly gains"))
(view (histogram five-year-gains :nbins 50 :title "Five year gains"))

(quantile (series pe))
(quantile (series spe))

(defn quantize-gains
  "Given prices and PE ratios,
  calculate the return for a given period,
  and gather the returns by PE quantile."
  [prices ratios period]
  (let [quantiles (drop-last (quantile ratios))
        quantize (fn [ratio]
                   (count (take-while #(>= ratio %) quantiles)))
        quantized (map quantize ratios)
        gains (map / prices (drop period prices))
        table (map vector quantized gains)
        indexed (group-by first table)]
    (for [[k v] (sort indexed)]
      (map second v))))

; check that the quantiles have roughly the same amount of values in each
(map count (quantize-gains (series sap) (series pe) 12))

(defn apr
  "To compare long term gains with short term gains"
  [rate months]
  (Math/pow rate (/ 12.0 months)))


; visualize the relative performance per quantile

(defn plot-quantized-gains
  "Plot the gains per bin of PE ratios per periods of investment"
  [prices ratios label]
  (let [qpe #(quantize-gains prices ratios %)
        periods [1 3 6 12 24 36 60 120 240 360]
        all-pe (map qpe periods)
        all-pe-means (map #(map mean %) all-pe)
        quantiles (range 1 5)
        raw (mapcat #(map (fn [x y] [%1 x y]) quantiles %2) periods all-pe-means)
        ;; n normalizes to an APR - uncomment the below if you want to plot adjusted
        ; raw (mapcat #(map (fn [x y] [%1 x (apr y %1)]) quantiles %2) periods all-pe-means)
        ds (dataset [:period :quantile :mean] raw)
        chart (bar-chart :quantile :mean :group-by :period :data ds
                         :title (str "Performance by " label)
                         :y-label "Return"
                         :x-label "Quantile"
                         :legend true)]
    (view chart)))

(plot-quantized-gains (series sap) (series pe) "PE")
(plot-quantized-gains (series sap) (series spe) "Shiller PE")


; what are the possible outcomes for a 1 year trading horizon?

(defn plot-distributions
  [prices ratios label bins1 bins2]
  (let [qg (quantize-gains prices ratios 12)
        chart (histogram (last qg) :nbins bins1 :series-label 1
                         :title (str "Relative distributions " label)
                         :x-label "Return"
                         :legend true)
        plot (.getPlot chart)
        renderer (.getRenderer plot)]
    (add-histogram chart (first qg) :nbins bins2 :series-label 4)
    (.setPaint renderer (java.awt.Color. 0 0 0 128))
    (.setSeriesOutlinePaint renderer 0 java.awt.Color/green)
    (.setSeriesOutlinePaint renderer 1 java.awt.Color/red)
    (view chart)))

(plot-distributions (series sap) (series pe) "PE" 50 25)
(plot-distributions (series sap) (series spe) "Shiller PE" 25 50)

