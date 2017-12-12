(ns gbtmago.mcts.scratch
  (:require [gbtmago.mcts.mcts :as mcts]
            [gbtmago.mcts.tictactoe :as ttt])
  (:use [rhizome.viz]
        [clojure.tools.trace]))

(comment

  (def game ttt/game)
  (def tree {})
  (def tree
    (loop [t tree
           n 0]
      (println "tree: " t)
      (let [nt
            (mcts/mcts {:tree t
                        :root game
                        :state game
                        :actions-fn ttt/actions-fn
                        :transition-fn ttt/transition-fn
                        :terminal? ttt/terminal?
                        :terminal-r-fn ttt/terminal-r
                        :expansion-policy mcts/expansion-policy
                        :selection-policy ttt/selection-policy
                        :selection-policy-args {:c 1.5
                                                :perspective-fn
                                                (fn [nodes]
                                                  (let [root-player (:player game)
                                                        cur-player (:player (first nodes))]
                                                    (if (= root-player cur-player)
                                                       -1
                                                      1)))}
                        :simulation-fn ttt/simulation-fn
                        :simulation-policy (fn [nodes] (rand-nth (into [] nodes)))
                        :value-update ttt/value-update})]
        (if (< n 500)
          (recur nt (inc n))
          nt))))

  (def graphed-tree (reduce (fn [acc [k v]]
                              (assoc acc k
                                         (:children v)))
                            {}
                            tree))

  (view-graph (keys graphed-tree) graphed-tree
              :node->descriptor (fn [n]
                                  (let [board (:board n)
                                        sq-disp (fn [board-val]
                                                  (case board-val
                                                    :x "x"
                                                    :o "o"
                                                    :n " "))]
                                    {:label (str (sq-disp (board 0)) " | " (sq-disp (board 1)) " | " (sq-disp (board 2)) "\n"
                                                 " - "     " - " " - "     " - " " - " "\n"
                                                 (sq-disp (board 3)) " | " (sq-disp (board 4)) " | " (sq-disp (board 5)) "\n"
                                                 " - "     " - " " - "     " - " " - " "\n"
                                                 (sq-disp (board 6)) " | " (sq-disp (board 7)) " | " (sq-disp (board 8))
                                                 "\n" "\n"
                                                 "value: " (:value (tree n)) "\n"
                                                 "visits: " (:visits (tree n)))}))
              :options {:fontsize 10.0
                        :dpi 65}
              :directed? false)

  (ttt/actions-fn game)

  (mcts/mcts {:tree tree
              :root game
              :state game
              :actions-fn ttt/actions-fn
              :transition-fn ttt/transition-fn
              :terminal? ttt/terminal?
              :terminal-r-fn ttt/terminal-r
              :expansion-policy mcts/expansion-policy
              :selection-policy ttt/selection-policy
              :selection-policy-args {:c 10
                                      :perspective-fn
                                         (fn [nodes]
                                           (let [root-player (:player game)
                                                 cur-player (:player (first nodes))]
                                             (if (= root-player cur-player)
                                               1
                                               -1)))}
              :simulation-fn ttt/simulation-fn
              :simulation-policy (fn [nodes] (rand-nth (into [] nodes)))
              :value-update ttt/value-update})

  (ttt/selection-policy tree (map (partial ttt/transition-fn game)
                                  (ttt/actions-fn game))
                        {:c 2
                         :perspective-fn
                         (fn [nodes]
                           (let [root-player (:player game)
                                 cur-player (:player (first nodes))]
                             (if (= root-player cur-player)
                               -1
                               1)))}))
