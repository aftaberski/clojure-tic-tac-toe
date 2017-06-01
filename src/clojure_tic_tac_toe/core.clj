(ns clojure-tic-tac-toe.core
  (:require
   [clojure.tools.namespace.repl :as repl]))

;; TODO: split out into different namespaces
(def blank-space
  " ")

(defn rows
  [board]
  (->> board
       (partition 3)
       (mapv vec)))

(defn columns
  [board]
  (apply mapv (comp vec list) (rows board)))

;; TODO: make multimethod?
(defn diagonal
  "Takes a vector of rows. Returns a vector of
  the left-to-right diagonal tiles"
  [board]
  (mapv get board (range)))

(defn diagonals
  [board]
  (let [left-to-right (diagonal (rows board))
        right-to-left (diagonal (map (comp vec reverse) (rows board)))]
    [left-to-right right-to-left]))

(defn get-winner
  "Takes a coll of vectors. Returns the winner or nil"
  [v]
  (let [winner (->> v
                    (map set)
                    (filter #(or (= % #{"X"})
                                 (= % #{"O"}))))]
    (if (seq winner)
      (ffirst winner)
      nil)))

(defn winner
  [board]
  (let [rows      (rows board)
        columns   (columns board)
        diagonals (diagonals board)
        winner    (get-winner (concat rows columns diagonals))]
    (prn (concat rows columns diagonals))
    winner))

(defn filled-space?
  [x]
  (not= blank-space x))

(defn game-status
  [board]
  (let [winner (winner board)
        full?  (every? filled-space? board)]
    (prn {:winner winner
          :full?  full?})
    {:winner winner
     :full?  full?}))

(defn print-board
  [v]
  (doseq
      [rows (rows v)]
    (prn rows)))

(defn prompt
  [text]
  (println (apply str text))
  (read-line))

(defn take-turn
  [board player]
  (print-board board)
  (let [next-move       (prompt ["Player " player ": Pick your next move!"])
        options         (map str (range 0 9))]

    (if (some #(= % next-move) options)

      (let [conformed-input (read-string next-move)]
        (if (= blank-space (get board conformed-input))

          (assoc board conformed-input player)

          (do (println "That space is already taken! Pick again")
              (take-turn board player))))

      (do (println "You must select a number between 0 - 8")
          (take-turn board player)))))

(defn play
  [board players]
  (print-board board)
  (let [{:keys [winner full?]} (game-status board)]

    (cond winner
          (println (str winner " won the game!"))

          full?
          (println "Looks like its a tie!")

          :else
          (let [next-board (take-turn board (first players))]
            (play next-board (reverse players))))))

(defn welcome
  [board]
  (println "*******************************************************")
  (println "              LET'S PLAY TIC TAC TOE!")
  (println "*******************************************************")
  (println "How to reference tiles:")
  (print-board (range 0 9))
  (println "*******************************************************"))

(defn -main
  [& args]
  (let [board (vec (repeat 9 blank-space))]
    (welcome board)
    (play board ["X" "O"])))
