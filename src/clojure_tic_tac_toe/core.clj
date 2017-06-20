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
    winner))

(defn filled-space?
  [x]
  (not= blank-space x))

(defn game-status
  [board]
  (let [winner (winner board)
        full?  (every? filled-space? board)]
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

(defn marker-with-next-turn
  [board]
  (let [board-frequencies (frequencies board)
        x-count           (get board-frequencies "X")
        o-count           (get board-frequencies "O")]

    (cond
      (nil? x-count)
      "X"

      (nil? o-count)
      "O"

      (> x-count o-count)
      "O"

      (= x-count o-count)
      "X")))

(defn player-with-next-turn
  [board players]
  (let [next-marker (marker-with-next-turn board)]
    (first (filter (fn [player] (= next-marker (:marker player))) players))))

(defn take-turn
  [board players]
  (print-board board)
  (println)
  (let [next-player (player-with-next-turn board players)
        next-move-fn (:next-move-fn next-player)
        next-move    (next-move-fn board next-player)
        options      (map str (range 0 9))]

    (if (some #(= % next-move) options)

      (let [conformed-input (read-string next-move)]
        (if (= blank-space (get board conformed-input))

          (assoc board conformed-input (:marker next-player))

          (do (println "That space is already taken! Pick again")
              (take-turn board players))))

      (do (println "You must select a number between 0 - 8")
          (take-turn board players)))))

(defn play
  [board players]
  (let [{:keys [winner full?]} (game-status board)]

    (cond winner
          (println (str winner " won the game!"))

          full?
          (println "Looks like its a tie!")

          :else
          (let [next-board (take-turn board players)]
            (play next-board (reverse players))))))

(defn welcome
  [board]
  (println "*******************************************************")
  (println "              LET'S PLAY TIC TAC TOE!")
  (println "*******************************************************")
  (println "How to reference tiles:")
  (print-board (range 0 9))
  (println "*******************************************************"))

(defn human-next-move
  [_ player]
  (prompt ["Player " (:marker player) ": Pick your next move!"]))

(defn free-spaces
  [board]
  (->> board
       (map-indexed vector)
       (filter #(= blank-space (second %)))
       (map (comp str first))))

(defn computer-next-move
  [board _]
  (rand-nth (free-spaces board)))



(defn smart-computer-next-move
  "Takes board as input and computes next move based on
  minimax algorithm"
  [])

(def human-x
  {:marker "X"
   :next-move-fn human-next-move})

(def human-o
  {:marker "O"
   :next-move-fn human-next-move})

(def computer
  {:marker       "O"
   :next-move-fn computer-next-move})

(def game-types
  {"1" [human-x human-o]
   "2" [human-x computer]})

(def game-type-prompt
  "Which type of game would you like to play?\n
  1 - Human v Human\n
  2 - Human v Computer")

(defn -main
  [& args]
  (let [board (vec (repeat 9 blank-space))]
    (welcome board)
    (let [game-type (prompt [game-type-prompt])
          players   (get game-types game-type)]
      (play board players))))
