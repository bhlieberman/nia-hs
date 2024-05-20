(ns com.bhlie.nia.v2.core
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdc]))

(defonce root (rdc/create-root (js/document.getElementById "root")))

(defn get-script-tag [idx]
  (let [tag (.item (.-scripts js/document) idx)
        text (.-innerText tag)
        [canto title & lines] (str/split-lines text)]
    (into [:div
           [:h1 canto]
           [:h2 title]]
          (for [line lines]
            [:p line]))))

(defonce app-state (r/atom {:footnotes nil}))

(defn get-footnotes [canto]
  (r/with-let [cursor (r/cursor app-state [:footnotes])]
    (-> (js/fetch (str "http://localhost:3000/footnotes/" canto))
        (.then (fn [resp] (.text resp)))
        (.then (fn [data] 
                 (js/console.log data)
                 (reset! cursor (str/split #"\n\n" data)))))))

(defn router [canto]
  [:div
   [:button
    {:on-click
     (fn []
       (when (not= @canto 1)
         (swap! canto dec)))} "Previous"]
   [:button
    {:on-click
     (fn []
       (when (not= @canto 3)
         (swap! canto inc)))} "Next"]])

(defn root-comp []
  (r/with-let [canto (r/atom 1)]
    [:div
     [router canto]
     [get-script-tag @canto]]))

(defn ^:dev/after-load render! []
  (rdc/render root [root-comp]))

(defn init []
  (render!))

(comment
  @app-state
  (get-footnotes 1))