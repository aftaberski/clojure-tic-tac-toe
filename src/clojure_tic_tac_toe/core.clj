(ns clojure-tic-tac-toe.core
  (:gen-class))

;; Welcome message
;; Prompt for user X to select space
;; Place X on board
;; Prompt for user O to select space
;; Place O on board
;; Continue while there is no winner || board is not full

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do (println "*******************************************************")
      (println "              LET'S PLAY TIC TAC TOE!")
      (println "*******************************************************") 
      (let [input (read-line)]
        (prn (str "this was the input: " input)))))
