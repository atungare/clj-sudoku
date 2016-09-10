(ns clj-sudoku.sudoku)

(defn xy->i
  "Int -> Int -> Int"
  [x y]
    (+ x (* 9 y)))

(defn rows
  "[a] -> [[a]]"
  [board]
  (for [y (range 0 9)]
    (for [x (range 0 9)]
      (get board (xy->i x y)))))

(defn cols
  "[a] -> [[a]]"
  [board]
  (for [x (range 0 9)]
    (for [y (range 0 9)]
      (get board (xy->i x y)))))

(defn grids
  "[a] -> [[a]]"
  [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (for [a (range x (+ x 3))
          b (range y (+ y 3))]
      (get board (xy->i a b)))))

(defn board->sections
  "[a] -> [[a]]"
  [board]
  (mapcat #(% board) [rows cols grids]))

(defn isComplete?
  "[Int] -> Boolean"
  [section_or_board]
  (not-any? zero? section_or_board))

(defn isSectionValid?
  "[Int] -> Boolean"
  [section]
  (let [values (remove zero? section)]
    (= (count values) (count (set values)))))

(defn isBoardValid?
  "[Int] -> Boolean"
  [board]
  (every? isSectionValid? (board->sections board)))

(defn updateFirst
  "a -> a -> [a]"
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
  "[Int] -> Int -> [Int]"
  [board trial]
  (updateFirst 0 trial board))

(defn solveBoard
  "[Int] -> [Int] || nil"
  [board]
  (if (isBoardValid? board)
    (if (isComplete? board)
      board
      (loop [trial 1]
        (if (> trial 9)
          nil
          (let [solution (solveBoard (updateBoard board trial))]
            (if solution
              solution
              (recur (inc trial)))))))
    nil))

(defn -main [& args]
 (println "Hello"))
