(ns clojure-tic-tac-toe.core
  (:gen-class))

;; Welcome message
;; Prompt for user X to select space
;; Place X on board
;; Prompt for user O to select space
;; Place O on board
;; Continue while there is no winner || board is not full

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
  [board]
  (mapv get board (range)))

(defn diagonals
  [board]
  (let [left-to-right (diagonal (rows board))
        right-to-left (diagonal (map (comp vec reverse) (rows board)))]
    [left-to-right right-to-left]))

(defn winner
  [board]
  (let [rows      (rows board)
        columns   (columns board)
        diagonals (diagonals board)]
    ;; TODO: check if set of all x or o but not blank
    ;; Get winner!!!
    (prn "rows" rows)
    (prn "columns" columns)
    (prn "diags" diagonals)))

(defn filled-space?
  [x]
  (not= blank-space x))

(defn game-status
  [board]
  (let [winner (winner board)
        full?  (every? filled-space? board)]
    (prn winner full?)))

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
        conformed-input (read-string next-move)]
    (assoc board conformed-input player)))

(defn play
  [board players]
  ;; while no winner && board not full
  (let [next-board (take-turn board (first players))]
    (print-board next-board)
    (game-status next-board)
    #_(recur next-board (reverse players))))

(defn welcome
  [board]
  (println "*******************************************************")
  (println "              LET'S PLAY TIC TAC TOE!")
  (println "*******************************************************"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [board (vec (repeat 9 blank-space))]
    (welcome board)
    (play board ["X" "O"])))
