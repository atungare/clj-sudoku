(ns clj-sudoku.sudoku)

(defn xy->i
  [x y]
    (+ x (* 9 y)))

(defn rows
  [board]
  (for [y (range 0 9)]
    (for [x (range 0 9)]
      (get board (xy->i x y)))))

(defn cols
  [board]
  (for [x (range 0 9)]
    (for [y (range 0 9)]
      (get board (xy->i x y)))))

(defn grids
  [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (for [a (range x (+ x 3))
          b (range y (+ y 3))]
      (get board (xy->i a b)))))

(defn board->sections
  [board]
  (mapcat #(% board) [rows cols grids]))

(defn isComplete?
  [section_or_board]
  (not-any? zero? section_or_board))

(defn isSectionValid?
  [section]
  (apply distinct? (remove zero? section)))

(defn isSectionSolved?
  [section]
  (and (isComplete? section) (isSectionValid? section)))

(defn isBoardSolved?
  [board]
  (every? isSectionSolved? (board->sections board)))

(defn updateFirst
  [x y lst]
  (:lst
    (reduce
      (fn [acc item]
        (if (and (not (:seen acc)) (= item x))
          {:lst (conj (:lst acc) y) :seen true}
          {:lst (conj (:lst acc) item) :seen (:seen acc)}))
      {:lst [] :seen false}
      lst)))

(defn updateBoard
  [board trial]
  (updateFirst 0 trial board))

(defn solveBoard
  [board]
  (if (isComplete? board)
    (if (isBoardSolved? board)
      board
      nil)
    (loop [trial 1]
      (if (> trial 9)
        nil
        (let [solution (solveBoard (updateBoard board trial))]
          (if solution
            solution
            (recur (inc trial))))))))

(defn -main [& args]
 (println "Hello"))
