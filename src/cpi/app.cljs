(ns cpi.app
  (:require [re-frame.core :as rf]
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
          :2022 151.2}})) ;; initial application state

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
                                   (assoc acc key-year
                                          (->
                                           (/ val-cpi prev-cpi-val)
                                           (.toFixed 3)))
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
     (if (empty? value)
       0.0
       (reduce
        #(.toFixed
          (*
           (js/Number.parseFloat %1)
           (-> %2
               str
               keyword
               ratios
               js/Number.parseFloat))
          2)
        val
        years-to-use)))))

(defn table-reducer
  [initial cpi ratios acc year]
  (let [lastf (comp last last)
        startv (-> acc
                   (lastf)
                   (or initial)
                   (js/Number.parseFloat))
        kw (-> year str keyword)
        rat (-> kw
                ratios
                js/Number.parseFloat)
        cpiv (kw cpi)]
    (conj
     acc
     [(str year)
      (.toFixed startv 2)
      (.toFixed cpiv 1)
      (-> rat
          js/Number.parseFloat
          (- 1)
          (* 100)
          (.toFixed 1)
          (str "%"))
      (-> startv
          (* rat)
          (.toFixed 2))])))

(rf/reg-sub
 :computed-table
 :<- [:cpi]
 :<- [:ratios]
 :<- [:select-years]
 :<- [:year]
 :<- [:value]
 (fn [[cpi ratios years cur-year value] _]
   (let [year (js/Number.parseInt cur-year)
         years-to-use (sort (filter #(>= % year) years))]
     (println "computing table from years " (str years-to-use))
  ;;this works since years are ordered ascending order
     (if (empty? value)
       []
       (reduce (partial table-reducer value cpi ratios) [] years-to-use)))))

;; -- Domino 5 - View Functions ----------------------------------------------

(defn input-f
  []
  (let [value (rf/subscribe [:value])]
    [:input {:name "val"
             :id "val "
             :class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
             :on-change #(rf/dispatch [:value-change (-> % .-target .-value)])
             :value @value}]))

(defn computed-val
  []

  (let [value (rf/subscribe [:computed-value])]
    @value))

(defn table
  []

  (let [table (rf/subscribe [:computed-table])]
    [:div
     {:class "relative overflow-x-auto shadow-md sm:rounded-lg"}
     [:table {:class "w-full text-sm text-left text-gray-500 dark:text-gray-400"}
      [:thead {:class "text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400"}
       [:tr
        [:th {:scope "col"
              :class "px-6 py-3"} "Year"]
        [:th {:scope "col"
              :class "px-6 py-3"} "Start Value"]
        [:th {:scope "col"
              :class "px-6 py-3"} "CPI"]
        [:th {:scope "col"
              :class "px-6 py-3"} "Change"]
        [:th {:scope "col"
              :class "px-6 py-3"} "End Value"]]]
      (into [:tbody]
            (for [row @table]
              (into [:tr {:class "bg-white border-b dark:bg-gray-900 dark:border-gray-700"}]
                    (for [col row] [:td {:class "px-6 py-4"} (str col)]))))]]))

(defn ui
  []
  (let [years (rf/subscribe [:select-years])
        year (rf/subscribe [:year])]
    [:div {:class "flex w-full flex-col items-center gap-8 mt-8 mb-8"}

     [:h2
      {:class "text-4xl font-extrabold dark:text-white"}
      "Inflation Calculator"]
     [:div {:class "grid gap-1 md:grid-cols-2"}
      [:div
       [:label {:for "year"
                :class "block mb-2 text-sm font-medium text-gray-900 dark:text-white"} "Year:"]
       [:select {:name "year"
                 :class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-auto p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                 :id "year"
                 :value @year
                 :on-change #(rf/dispatch [:year-change (-> % .-target .-value)])}
        (for [x @years] ^{:key x} [:option {:value x} (str x)])]]
      [:div
       [:label {:for "val"
                :class "block mb-2 text-sm font-medium text-gray-900 dark:text-white"} "Value:"]
       (input-f)]]

     [:div {:class "block max-w-sm p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700"}

      [:p {:class "font-normal text-gray-700 dark:text-gray-400"} "Today's value adjusted for inflation"]
      [:div {:class "flex justify-center mt-4"}
       [:h5 {:class "mb-1 text-2xl font-bold tracking-tight text-gray-900 dark:text-white "} (computed-val)]]]

     (table)]))

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