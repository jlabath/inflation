(ns cpi.app
  (:require [re-frame.core :as rf]
            [reagent.dom]))

;; -- Domino 1 - Event Dispatch -----------------------------------------------

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db ;; sets up initial application state
 :initialize ;; usage:  (dispatch [:initialize])
 (fn [_ _] ;; the two parameters are not important here, so use _
   {:cpi {:2000 95.4
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

;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub ;; a part of the re-frame API
 :cpi ;; usage: (subscribe [:cpi])
 (fn [db query-v] ;; `db` is the map out of `app-db`
   (:cpi db))) ;; trivial extraction - no computation

;; -- Domino 5 - View Functions ----------------------------------------------

(defn ui
  []
  (let [cpi (rf/subscribe [:cpi])
        years (sort (map (comp int name) (keys @cpi)))]
    [:div
     [:label {:for "year"} "Year:"]
     [:select {:name "year"
               :id "year"}
      (for [x years] ^{:key x} [:option {:value x} (str x)])]

     [:label {:for "val"} "Value:"]
     [:input {:name "val"
              :id "val "
              :value "boo"}]]))

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