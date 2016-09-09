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
  ())

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
          {:lst (cons y acc) :seen true}
          {:lst (cons item acc) :seen (:seen acc)}))
      {:lst [] :seen false}
      lst)))

(defn updateBoard
  [board candidate]
  (updateFirst 0 candidate board))

(defn solveBoard
  [board]
  (if (isComplete? board)
    (if (isBoardSolved? board)
      board
      nil)
    (loop [candidate 1]
      (if (> candidate 9)
        nil
        (let [solution (solveBoard (updateBoard board candidate))]
          (if solution
            solution
            (recur (inc candidate))))))))

(defn -main [& args]
 (println "Hello"))
