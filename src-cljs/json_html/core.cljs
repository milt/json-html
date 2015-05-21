(ns json-html.core
  (:require [clojure.string :as st]
            [hiccups.runtime :as hiccupsrt])
  (:require-macros [hiccups.core :as hiccups]))

(defn escape-html [s]
  (st/escape s
          {"&"  "&amp;"
           ">"  "&gt;"
           "<"  "&lt;"
           "\"" "&quot;"}))

(defn render-keyword [k]
  (->> k ((juxt namespace name)) (remove nil?) (clojure.string/join "/")))

(declare render)

(defn render-collection [col & [overrides]]
  (if (empty? col)
    [:div.jh-type-object [:span.jh-empty-collection]]
    [:table.jh-type-object
     [:tbody
      (for [[i v] (map-indexed vector col)]
        ^{:key (str i v)}
        [:tr [:th.jh-key.jh-array-key i]
         [:td.jh-value.jh-array-value (render v overrides)]])]]))

(defn render-set [s & [overrides]]
  (if (empty? s)
    [:div.jh-type-set [:span.jh-empty-set]]
    [:ul (for [item (sort s)]
           ^{:key (str item)}
           [:li.jh-value (render item overrides)])]))

(defn render-map [m & [overrides]]
  (if (empty? m)
    [:div.jh-type-object [:span.jh-empty-map]]
    [:table.jh-type-object
     [:tbody
      (for [[k v] (into (sorted-map) m)]
        ^{:key (str k v)}
        [:tr [:th.jh-key.jh-object-key (render k overrides)]
             [:td.jh-value.jh-object-value (render v overrides)]])]]))

(defn render-string [s]
  [:span.jh-type-string
   (if (st/blank? s)
     [:span.jh-empty-string]
     (escape-html s))])

(defn render [v & [overrides]]
  (let [t (type v)
        override (when overrides
                     (get overrides t))]
    (cond
      override (override v)
      (= t Keyword) [:span.jh-type-string (render-keyword v)]
      (= t js/String) [:span.jh-type-string (escape-html v)]
      (= t js/Date) [:span.jh-type-date (.toString v)]
      (= t js/Boolean) [:span.jh-type-bool (str v)]
      (= t js/Number) [:span.jh-type-number v]
      (satisfies? IMap v) (render-map v overrides)
      (satisfies? ISet v) (render-set v overrides)
      (satisfies? ICollection v) (render-collection v overrides)
      nil [:span.jh-empty nil])))

(defn edn->hiccup [edn & [overrides]]
  (render edn overrides))

(defn edn->html [edn & [overrides]]
  (hiccups/html (edn->hiccup edn overrides)))

(defn json->hiccup [json & [overrides]]
  (render (js->clj json) overrides))

(defn json->html [json & [overrides]]
  (hiccups/html (json->hiccup json overrides)))
