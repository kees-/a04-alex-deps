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
       [:contents vector?]
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
(defn get-children
  "Get a map of children at a particular address."
  [tree path]
  (->> (repeat :children) (interleave path) (get-in tree)))

;; ========== TESTS ============================================================
(def sample
  {"x" {:display "x"
        :contents [:div "x content"]
        :children {"a" {:display "a"
                        :contents [:a "href"]
                        :children {"m" {:display "m"
                                        :contents [:div "m content"]
                                        :children {}}
                                   "n" {:display "n"
                                        :contents [:div "n content"]
                                        :children {}}}}
                   "b" {:display "c"
                        :contents []
                        :children {}}}}
   "y" {:display "y"
        :contents [:div "y content"]
        :children {"c" {:display "c"
                        :contents []
                        :children {}}}}})

(comment
 (malli/schema schema)
 (malli/properties schema)
 (malli/validate schema sample)
 (pprint sample)
 (pprint
  (get-children sample ["x" "a"]))
 (pprint
  (add-node sample
            ["x"]
            (node {:address "a"
                   :contents [:span "hello"]})))
 (pprint
  (remove-node sample ["x" "a" "n"])))
;; ========== EXEC =============================================================
(defn -main
  "Do nothing"
  []
  (println ":)"))
