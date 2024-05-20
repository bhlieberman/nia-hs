(ns com.bhlie.nia.v2.core
  (:require [clojure.string :as str]
            ["react" :refer [useEffect]]
            [reagent.core :as r]
            [reagent.dom.client :as rdc]))

(defonce root (rdc/create-root (js/document.getElementById "root")))

(defn timer [reset]
  (let [now (. (js/Date. (js/Date.now)) toLocaleTimeString)
        timer (js/setInterval #(reset now) 1000)]
    (fn [] (js/clearInterval timer))))

(defn get-script-tag []
  (into [:div]
        (for [tag (.-scripts js/document)
              :let [text (.-innerText tag)]
              :when (not (str/blank? text))]
          (for [line (str/split-lines text)]
            [:p line]))))

(defn clock []
  (r/with-let [current-time (r/atom nil)]
    (useEffect
     (fn []
       (let [now (-> (js/Date.now) js/Date. .toLocaleTimeString)
             timer (js/setInterval #(reset! current-time now) 1000)]
         (fn [] (js/clearInterval timer)))))
    [:div
     [:p "Current time: " @current-time]]))

(defn root-comp []
  [:div
   [:f> clock]
   [get-script-tag]])

(defn ^:dev/after-load render! []
  (rdc/render root [root-comp]))

(defn init []
  (render!))