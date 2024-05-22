(ns com.bhlie.nia.v2.core
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdc]))

(defonce app-state (r/atom {:footnotes {1 [1 2 3]}
                            :parens {1 []}}))

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

(defn show-parens [canto idx]
  (r/with-let [cursor (r/cursor app-state [:parens canto])]
    (into [:div]
          (for [line (str/split-lines (nth @cursor idx ""))]
            [:p line]))))

(defn parent [canto parens]
  (cond
    (not (zero? @parens)) [show-parens @canto @parens]
    (<= 1 @canto 3) [get-script-tag @canto]))

(defn get-footnotes [canto]
  (r/with-let [cursor (r/cursor app-state [:footnotes])]
    (let [ks (get @cursor 1)
          footnotes (if (every? int? ks) (str/join "," ks) "")]
      (when-not (str/blank? footnotes)
        (-> (js/fetch (str "http://localhost:3000/footnotes/" canto "/" footnotes))
            (.then (fn [resp] (.text resp)))
            (.then (fn [data]
                     (js/console.log data)
                     (reset! cursor (str/split data #"\n\n")))))))))

(defn get-parens [canto]
  (r/with-let [cursor (r/cursor app-state [:parens])]
    (let [ks (get @cursor canto)
          parens (if (every? int? ks) (str "?parens=" (str/join "," ks)) "")]
      (-> (js/fetch (str "http://localhost:3000/parens/" canto parens))
          (.then (fn [resp] (.text resp)))
          (.then (fn [data]
                   (js/console.log data)
                   (swap! cursor assoc canto (str/split data #"\n\n"))))))))

(defn router [canto parens]
  [:div
   [:button
    {:on-click
     (fn []
       (when (not= @canto 1)
         (swap! canto dec)))} "Previous"]
   [:button
    {:on-click
     (fn []
       (when (not= @parens 0)
         (swap! parens dec)))} "Up"]
   [:button
    {:on-click
     (fn []
       (when (not= @canto 3)
         (swap! canto inc)))} "Next"]
   [:button
    {:on-click
     (fn []
       (when (not= @parens 4)
         (swap! parens inc)))} "Down"]])

(defn root-comp []
  (r/with-let [canto (r/atom 1)
               parens (r/atom 0)]
    [:div
     [router canto parens]
     [parent canto parens]]))

(defn ^:dev/after-load render! []
  (rdc/render root [root-comp]))

(defn init []
  (render!))

(comment
  (reset! app-state {:footnotes {1 [1 2 3]}
                     :parens {1 [1 2 3 4 5]}})
  (get-in @app-state [:parens])
  (get-parens 1)
  (get-footnotes 1))