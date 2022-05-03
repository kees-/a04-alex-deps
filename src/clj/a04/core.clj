(ns a04.core
  (:require
   [malli.core :as malli]
   [clojure.pprint :refer [pprint]]))

;; ========== SCHEMA ===========================================================
(def schema
  [:schema
   {:registry
    {::node
     [:map-of
      :string
      [:map
       [:display {:optional true} string?]
       [:contents
        [:map
         [:type [:enum :nil :hiccup :text :link :img :video :audio :iframe]]
         [:data any?]]]
       [:children [:ref ::node]]]]}}
   ::node])

;; ========== CONSTRUCTION =====================================================
(defn node
  "Shape a tuple representing a single node"
  [{:keys [address display contents] :or {contents []}}]
  [address {:display (if display display address)
            :contents contents
            :children {}}])

(defn add-node
  "Given a vector of addresses, add a node to a tree."
  [tree path node]
  (assoc-in
   tree
   (-> path (interleave (repeat :children)) vec (conj (first node)))
   (last node)))

(defn remove-node
  "Given a vector of addresses, remove the node at the final address."
  [tree path]
  (update-in
   tree
   (interleave (butlast path) (repeat :children))
   dissoc (last path)))

;; ========== SELECTION ========================================================
(defn get-node
  [tree path]
  (->> (repeat :children) (interleave path) butlast (get-in tree)))

(defn get-children
  "Get a map of children at a particular address."
  [tree path]
  (->> (repeat :children) (interleave path) (get-in tree)))

;; ========== TESTS ============================================================
(def sample
  {"x" {:display "x"
        :contents
        {:type :hiccup
         :data [:div "x content"]}
        :children
        {"a" {:contents
              {:type :hiccup
               :data [:a "href"]}
              :children
              {"m" {:contents
                    {:type :hiccup
                     :data [:div "m content"]}
                    :children {}}
               "n" {:contents
                    {:type :hiccup
                     :data [:div "n content"]}
                    :children {}}}}
         "b" {:display "c"
              :contents
              {:type :nil
               :data nil}
              :children {}}}}
   "y" {:contents
        {:type :hiccup
         :data [:div "y content"]}
        :children
        {"c" {:contents {:type :nil
                         :data nil}
              :children {}}}}})

(comment
 (malli/schema schema)
 (malli/properties schema)
 (malli/validate schema sample)
 (pprint sample)
 (pprint (get-children sample ["x" "a"]))
 (pprint (get-node sample ["x" "a"]))
 (pprint (remove-node sample ["x" "a" "n"]))
 (pprint
  (add-node sample
            ["x"]
            (node {:address "a"
                   :contents [:span "hello"]}))))

(comment
 (let [a (node {:address "a" :contents [:div "goodmorning"]})
       b (node {:address "b" :contents [:div "goodnight"]})
       z (node {:address "z" :display "ℝ" :contents [:div "goodmorning"]})]
   (-> (add-node {} [] a)
       (add-node [] b)
       (add-node ["a"] z)
       pprint)))

(comment
 (malli/validate
  schema
  {"x" {:display "x"
        :contents {:type :hiccup
                   :data [:div "ee"]}
        :children {}}}))

;; ========== EXEC =============================================================
(defn -main
  "Do nothing"
  []
  (println ":)"))
