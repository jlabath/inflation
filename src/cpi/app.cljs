(ns cpi.app
  (:require [re-frame.core :as rf]
            [reagent.dom]))

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db ;; sets up initial application state
 :initialize ;; usage:  (dispatch [:initialize])
 (fn [_ _] ;; the two parameters are not important here, so use _
   {:year 2020
    :value "100"
    :max-year 2023
    :cpi {:1979 40.0
          :1980 44.0
          :1981 49.5
          :1982 54.9
          :1983 58.1
          :1984 60.6
          :1985 63.0
          :1986 65.6
          :1987 68.5
          :1988 71.2
          :1989 74.8
          :1990 78.4
          :1991 82.8
          :1992 84.0
          :1993 85.6
          :1994 85.7
          :1995 87.6
          :1996 88.9
          :1997 90.4
          :1998 91.3
          :1999 92.9
          :2000 95.4
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
          :2022 151.2
          :2023 157.1}})) ;; initial application state

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

(rf/reg-sub
 :year
 (fn [db query-v]
   (:year db)))

(rf/reg-sub
 :max-year
 (fn [db query-v]
   (:max-year db)))

(rf/reg-sub
 :value
 (fn [db query-v]
   (:value db)))

(rf/reg-sub
 :select-years

  ;; input signals 
 :<- [:cpi] ;; means (subscribe [:cpi] is an input)

  ;; computation function
 (fn [cpi _] ;;apparently if there is only one it does not send a vector but the value itself when using the syntactic sugar
   (rest (sort (map (comp int name) (keys cpi))))))

(defn inflation-from-year
  "calculates an inflation from a certain year
   cpi is the cpi hashmap
   start-year is most likely whatever the user selected in dropdown 
   end-year is most likely max-year or diff year if desired
   val is the amount 
   returns a js number
  "
  [cpi start-year end-year val]
  (let [year-before-start (dec start-year)
        start-cpi (get cpi (-> year-before-start str keyword))
        end-cpi (get cpi (-> end-year str keyword))]
    (->
     (* end-cpi val)
     (/ start-cpi))))

(rf/reg-sub
 :computed-value
 :<- [:cpi]
 :<- [:max-year]
 :<- [:year]
 :<- [:value]
 (fn [[cpi max-year cur-year value] _]
   (let [val (js/Number.parseFloat value)]
     (->
      (inflation-from-year cpi cur-year max-year val)
      (.toFixed 2)))))

(defn table-reducer
  "reducer function for computed-table
   returns a vector of computed values as strings
  "
  [{:keys [cpi value result start-year]
    :as acc}
   year]
  (let [start-value (if (= start-year year)
                      value
                      (inflation-from-year cpi start-year (dec year) value))
        end-value (inflation-from-year cpi start-year year value)
        inflation (-> (/ end-value start-value) (- 1) (* 100))]
    (assoc acc :result (conj result [(str year) (str (.toFixed inflation 1) "%") (.toFixed start-value 2) (.toFixed end-value 2)]))))

(rf/reg-sub
 :computed-table
 :<- [:cpi]
 :<- [:select-years]
 :<- [:year]
 :<- [:value]
 (fn [[cpi years cur-year value] _]
   (let [year (js/Number.parseInt cur-year)
         years-to-use (sort (filter #(>= % year) years))]
     ;;this works since years are ordered ascending order
     (if (empty? value)
       []
       (-> (reduce table-reducer {:cpi cpi
                                  :start-year year
                                  :value (js/Number.parseFloat value)
                                  :result []} years-to-use)
           (get :result))))))

;; -- Domino 1 - Event Dispatch -----------------------------------------------
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
              :class "px-6 py-3"} "Inflation"]
        [:th {:scope "col"
              :class "px-6 py-3"} "Start Value"]
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
    [:div {:class "flex flex-col items-center gap-8 mt-8 mb-8"}

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

     (table)
    ;;footer
     (let [cls "font-medium text-blue-600 dark:text-blue-500 hover:underline"]
       [:div {:class "flex flex-col items-center gap-3"}
        [:a {:class cls
             :href "https://www.statcan.gc.ca/en/subjects-start/prices_and_price_indexes/consumer_price_indexes"} "Source Data"]
        [:a {:class cls
             :href "https://github.com/jlabath/inflation"} "Source Code"]])]))

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
