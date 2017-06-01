(ns clojure-tic-tac-toe.core
  (:require
   [clojure.tools.namespace.repl :as repl]))

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
    ;; TODO: check if set of all x or o but not blank
    ;; Get winner!!! 
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
        ;; TODO: validate input - only 1 thru 9, no one else there
        ;; If not valid, take turn again
        conformed-input (read-string next-move)]
    (assoc board conformed-input player)))

(defn play
  [board players]
  ;; while no winner && board not full
  (print-board board)
  (let [{:keys [winner full?]} (game-status board)
        _ (prn "winner in play" winner)]

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
