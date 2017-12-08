(ns gbtmago.mcts.mcts
  (:require [clojure.tools.trace :as trace])
  (:use [rhizome.viz]))

(defn mcts [{:keys [tree root state actions-fn transition-fn terminal? terminal-r-fn expansion-policy selection-policy selection-policy-args simulation-fn simulation-policy value-update]}]
  (println "state: " state)
  (let [children (map (partial transition-fn state)
                      (actions-fn state))
        expanded-children (-> (tree state)
                              :children)
        all-expanded? (= (into #{} children)
                         (into #{} expanded-children))]
    (if (or (not children)
            (empty? children))
      tree
      (if (not all-expanded?)
        (let [expanded-child (expansion-policy (filter (comp not (into #{} expanded-children))
                                                       children))
              sim-value (simulation-fn {:state expanded-child
                                        :actions-fn actions-fn
                                        :transition-fn transition-fn
                                        :simulation-policy simulation-policy
                                        :terminal? terminal?
                                        :terminal-r-fn terminal-r-fn})
              newtree (if (not (tree expanded-child))
                        (-> tree
                            (assoc expanded-child {:value 0
                                                   :visits 1
                                                   :parent state})
                            (update expanded-child assoc :value sim-value))
                        (-> tree
                            (update expanded-child update :value + sim-value)))]
          (println "expansion")
          (println "expanded-child " expanded-child)
          (println "sim-value " sim-value)
          (println "newtree " newtree)
          (println "updated newtree: " (-> newtree
                                           (update-in [state :children] conj expanded-child)
                                           ((fn [t n]
                                              (loop [t t
                                                     n n]
                                                (println "t: " t)
                                                (println "n: " n)
                                                (if (= n root)
                                                  t
                                                  (recur (-> t
                                                             (update n update :value value-update sim-value n root)
                                                             (update n update :visits inc))
                                                         (:parent (t n))))))
                                            state)))
          (-> newtree
              (update-in [state :children] conj expanded-child)
              ((fn [t n]
                 (loop [t t
                        n n]
                   (if (= n root)
                     t
                     (recur (-> t
                                (update n update :value value-update sim-value n root)
                                (update n update :visits inc))
                            (:parent (t n))))))
               state)))
        (do (println "selection")
          (merge tree
                 (mcts {:tree tree
                        :root root
                        :state (selection-policy tree children selection-policy-args)
                        :actions-fn actions-fn
                        :transition-fn transition-fn
                        :terminal? terminal?
                        :terminal-r-fn terminal-r-fn
                        :expansion-policy expansion-policy
                        :selection-policy selection-policy
                        :selection-policy-args selection-policy-args
                        :simulation-fn simulation-fn
                        :simulation-policy simulation-policy
                        :value-update value-update})))))))


(def game {:board [1 2 3 4 5 6 7 8 9 10 9 8 7 6 5 4 3 2 1]
           :position 5
           :energy 20})


(defn actions-fn [state]
  (transduce (filter #(and (>  % 0)
                           (<  % (dec (count (:board state))))
                           (>= (- (:energy state) (- ((:board state) %)) ((:board state) (:position state)))) 0))
             conj
             []
             (vec #{(max 0 (dec (:position state)))
                    (min (inc (:position state)) (dec (count (:board state))))
                    (:position state)})))

(defn transition-fn [state action]
  (-> state
      (assoc :position action)
      (update :energy - (- ((:board state) action) ((:board state) (:position state)) 1))
      (update-in [:board (:position state)] dec)))

(defn terminal? [state]
  (= ((:board state) (:position state)) 0))

(defn terminal-r-fn [state]
  (- 0 (:energy state)))

(defn selection-policy [tree nodes {:keys [c]}]
  (:node (first (sort-by :score > (map #(let [node-info (tree %)
                                              parent-info (tree (:parent node-info))]
                                          (hash-map :node %
                                                    :score (+ (/ (:value node-info) (:visits node-info))
                                                              (* c (Math/sqrt (/ (Math/log (reduce + 0 (map (fn [n] (:visits (tree n)))
                                                                                                            nodes)))
                                                                                 (:visits node-info)))))))
                                       nodes)))))

(defn simulation-fn [{:keys [state actions-fn transition-fn simulation-policy terminal? terminal-r-fn]}]
  (loop [s state]
    (if (terminal? s)
      (terminal-r-fn s)
      (recur (transition-fn s (simulation-policy (actions-fn s)))))))

(defn expansion-policy [nodes]
  (rand-nth (into [] nodes)))


(comment
  (def tree {})
  (def tree
    (loop [t tree
           n 0]
      (let [nt
            (mcts {:tree t
                   :root game
                   :state game
                   :actions-fn actions-fn
                   :transition-fn transition-fn
                   :terminal? terminal?
                   :terminal-r-fn terminal-r-fn
                   :expansion-policy expansion-policy
                   :selection-policy selection-policy
                   :selection-policy-args {:c 10}
                   :simulation-fn simulation-fn
                   :simulation-policy (fn [nodes] (rand-nth (into [] nodes)))})]
        (if (< n 1000)
          (recur nt (inc n))
          nt))))
  (def graphed-tree (reduce (fn [acc [k v]]
                              (assoc acc k
                                         (:children v)))
                            {}
                            tree))
  (view-graph (keys graphed-tree) graphed-tree
              :node->descriptor (fn [n] {:label (str (:value (tree n)) "\n")})))
                                                     ;(:visits (tree n)) "\n"
                                                     ;(:position n) "\n"
                                                     ;(:energy n))})))
