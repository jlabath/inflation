(ns cpi.app
  (:require [re-frame.core :as rf]
            [reagent.dom]))

;; -- Domino 1 - Event Dispatch -----------------------------------------------

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db ;; sets up initial application state
 :initialize ;; usage:  (dispatch [:initialize])
 (fn [_ _] ;; the two parameters are not important here, so use _
   {:time (js/Date.) ;; What it returns becomes the new application state
    :cpi {}})) ;; so the application state will initially be a map with two keys

;; -- Domino 4 - Query  -------------------------------------------------------

;; -- Domino 5 - View Functions ----------------------------------------------

(defn ui
  []
  [:div
   [:h1 "Hello world"]])

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