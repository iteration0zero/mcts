(ns gbtmago.mcts.tictactoe
  (:use [clojure.tools.trace]))

(def win-combinations
     #{[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]})

(def empty-board
     [:n :n :n
      :n :n :n
      :n :n :n])

(defn check-for-win [board player]
  (or
    (some true? (map (fn [wc] (every? (fn [wci]
                                        (= (board wci)
                                           player))
                                      wc))
                     win-combinations))
    false))

(defn get-moves [board]
  (transduce (filter #(= :n (board %)))
             conj
             (range (count board))))

(defn make-move [board player pos]
  (assoc board pos player))

(def game {:board empty-board
           :player :x})

(defn actions-fn [state]
  (get-moves (:board state)))

(defn transition-fn [state action]
  (-> state
      (update :board make-move (:player state) action)
      (update :player (fn [o] (if (= o :x)
                                :o
                                :x)))))

(defn terminal? [state]
  (let [prev-player (if (= (:player state) :x)
                      :o
                      :x)]
    (or (check-for-win (:board state) prev-player)
        (not-any? #(= :n %) (:board state)))))

(defn terminal-r [state {:keys [player]}]
  (let [other-player (if (= player :x)
                       :o
                       :x)]
    (if (check-for-win (:board state) player)
       -1
      (if (check-for-win (:board state) other-player)
        1
        0))))

(defn simulation-fn [{:keys [state actions-fn transition-fn simulation-policy terminal? terminal-r-fn]}]
  (let [cur-player (:player state)]
    (loop [s state]
      (if (terminal? s)
        (terminal-r-fn s {:player cur-player})
        (recur (transition-fn s (simulation-policy (actions-fn s))))))))

(defn selection-policy [tree nodes {:keys [c perspective-fn]}]
  (let [perspective (perspective-fn nodes)]
    (:node (first (sort-by :score > (map #(let [node-info (tree %)
                                                parent-info (tree (:parent node-info))]
                                            (hash-map :node %
                                                      :score (+ (/ (* (:value node-info) perspective) (:visits node-info))
                                                                (* c (Math/sqrt (/ (Math/log (reduce + 0 (map (fn [n] (:visits (tree n)))
                                                                                                              nodes)))
                                                                                   (:visits node-info)))))))
                                         nodes))))))

(defn value-update [old-value sim-value node start-node]
  (let [perspective (if (= (:player node)
                           (:player start-node))
                       1
                      -1)]
    (+ old-value
       (* sim-value perspective))))

(comment (check-for-win [:x :n :n
                         :x :y :y
                         :y :y :y]
                        :y)
         (get-moves empty-board)
         (get-moves (make-move empty-board :x 0)))
