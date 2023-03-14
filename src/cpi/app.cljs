(ns cpi.app
  (:require [goog.string :as gstring]
            [goog.string.format]
            [re-frame.core :as rf]
            [reagent.dom]))

;; -- Domino 1 - Event Dispatch -----------------------------------------------

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db ;; sets up initial application state
 :initialize ;; usage:  (dispatch [:initialize])
 (fn [_ _] ;; the two parameters are not important here, so use _
   {:year 2020
    :value "100"
    :cpi {:2000 95.4
          :2001 97.8
          :2002 100.0
          :2003 102.8
          :2004 104.7
          :2005 107.0
          :2006 109.1
          :2007 111.5
          :2008 114.1
          :2009 114.4
          :2010 116.5
          :2011 119.9
          :2012 121.7
          :2013 122.8
          :2014 125.2
          :2015 126.6
          :2016 128.4
          :2017 130.4
          :2018 133.4
          :2019 136.0
          :2020 137.0
          :2021 141.6
          :2022 151.2}})) ;; so the application state will initially be a map with two keys

(rf/reg-event-db
 :year-change
 (fn [db [_ new-year]]
   (assoc db :year new-year)))

(rf/reg-event-db
 :value-change
 (fn [db [_ new-val]]
   (let [strv (.trim new-val)
         v (js/Number.parseFloat strv)
         isNum (complement js/Number.isNaN)]
     (cond
       (empty? strv) (assoc db :value strv)
       (and (isNum v) (>= v 0.0)) (assoc db :value strv)
       :else db))))

;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub ;; a part of the re-frame API
 :cpi ;; usage: (subscribe [:cpi])
 (fn [db query-v] ;; `db` is the map out of `app-db`
   (:cpi db))) ;; trivial extraction - no computation

;;here compute the ratios in cpi

;;reducer for cpi to compute the ratios
;;needs to be curried with cpi hash first
;;which will yield the reducer function
(defn ratio-reducer
  [cpi acc [key-year val-cpi]] (let [prev-cpi-key (-> key-year
                                                      name
                                                      js/Number.parseInt
                                                      dec
                                                      str
                                                      keyword)
                                     prev-cpi-val (prev-cpi-key cpi)]
                                 (if prev-cpi-val
                                   (assoc acc key-year (/ val-cpi prev-cpi-val))
                                   acc)))

(rf/reg-sub
 :ratios

  ;; signals function
 (fn [_]
   [(rf/subscribe [:cpi])]) ;; <-- these inputs are provided to the computation function 

  ;; computation function
 (fn [[cpi] _] ;; input values supplied in a vector
   (println "executing reduce" (str cpi))
   (reduce (partial ratio-reducer cpi) {} cpi)))

(rf/reg-sub
 :year
 (fn [db query-v]
   (:year db)))

(rf/reg-sub
 :value
 (fn [db query-v]
   (:value db)))

(rf/reg-sub
 :select-years

  ;; input signals 
 :<- [:ratios] ;; means (subscribe [:ratios] is an input)

  ;; computation function
 (fn [ratios _] ;;apparently if there is only one it does not send a vector but the value itself when using the syntactic sugar
   (println "doing ratios" (str ratios))
   (sort (map (comp int name) (keys ratios)))))

(rf/reg-sub
 :computed-value
 :<- [:ratios]
 :<- [:select-years]
 :<- [:year]
 :<- [:value]
 (fn [[ratios years cur-year value] _]
   (let [year (js/Number.parseInt cur-year)
         years-to-use (sort (filter #(>= % year) years))
         val (js/Number.parseFloat value)]
     (println "cputing value from year " (str years-to-use))
  ;;this works since years are ordered ascending order
     (reduce #(* %1 (-> %2
                        str
                        keyword
                        ratios)) val years-to-use))))

;; -- Domino 5 - View Functions ----------------------------------------------

(defn input-f
  []
  (let [value (rf/subscribe [:value])]
    [:input {:name "val"
             :id "val "
             :on-change #(rf/dispatch [:value-change (-> % .-target .-value)])
             :value @value}]))

(defn computed-val
  []

  (let [value (rf/subscribe [:computed-value])]
    [:div {:class "w-auto"} (gstring/format "%.2f" @value)]))

(defn ui
  []
  (let [years (rf/subscribe [:select-years])
        year (rf/subscribe [:year])]
    [:div {:class "flex w-full flex-col items-center"}
     [:h3 "Values adjusted to today by yearly average CPI"]
     [:label {:for "year"} "Year:"]
     [:select {:name "year"
               :id "year"
               :value @year
               :on-change #(rf/dispatch [:year-change (-> % .-target .-value)])}
      (for [x @years] ^{:key x} [:option {:value x} (str x)])]

     [:label {:for "val"} "Value:"]
     (input-f)
     [:span "Today's value: " (computed-val)]]))

;; -- Entry Point -------------------------------------------------------------

(defn render
  []
  (reagent.dom/render [ui]
                      (js/document.getElementById "root")))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (rf/clear-subscription-cache!)
  (render))

(defn main
  []
  (rf/dispatch-sync [:initialize])
  (render))