(ns clj-sudoku.sudoku)

(defn xy->i
  [x y]
    (+ x (* 9 y)))

(defn i->xy
  [i]
  {:x (mod i 9) :y (quot i 9)})

(defn noconflict
  [i]
  (let [{x :x y :y} (i->xy i)
        row (for [a (range 0 9) :when (not= a x)]
              (xy->i a y))
        col (for [b (range 0 9) :when (not= b y)]
              (xy->i x b))
        c (quot x 3)
        d (quot y 3)
        grid (for [a (range c (+ c 3))
                   b (range d (+ d 3))
                   :when (not (and (= a x) (= b y)))]
               (xy->i a b))]
    (set (concat row col grid))))

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
  (every? number? section_or_board))

(defn isSectionValid?
  [section]
  (let [values (filter number? section)]
    (= (count values) (count (set values)))))

(defn isBoardValid?
  [board]
  (every? isSectionValid? (board->sections board)))

(defn updateFirst
  [pred transform lst]
  (:lst
    (reduce
      (fn [acc item]
        (if (and (not (:seen acc)) (pred item))
          {:lst (conj (:lst acc) (transform item)) :seen true}
          {:lst (conj (:lst acc) item) :seen (:seen acc)}))
      {:lst [] :seen false}
      lst)))

(defn updateBoard
  [board]
  (if (empty? (first (remove number? board)))
    nil
    (updateFirst #(not (number? %)) first board)))

(defn rejectTrial
  [board]
  (updateFirst #(not (number? %)) rest board))

(defn strikeConflicts
  [board]
  (map-indexed
    (fn [idx item]
      (if (number? item)
        item
        (let [neigbor_indices (noconflict idx)]
          (reduce
            (fn [options neighbor_index]
              (let [neighbor_val (get board neighbor_index)]
                (if (number? neighbor_val)
                  (remove #(= neighbor_val %) options)
                  options)))
            item
            neigbor_indices))))
    board))

(defn doStrikeConflicts
  [board]
  (loop [prior board
         new_board (strikeConflicts board)]
    (if (= prior new_board)
      new_board
      (recur new_board (strikeConflicts new_board)))))

(defn solveBoard
  [board]
  (println "new")
  (println board)
  (let [reduced_board (doStrikeConflicts board)]
    (if (isBoardValid? reduced_board)
      (if (isComplete? reduced_board)
        reduced_board
        (loop [prior reduced_board]
          (let [trial_board (updateBoard prior)]
            (if (nil? trial_board)
              nil
              (let [solution (solveBoard trial_board)]
                (if (not (nil? solution))
                  solution
                  (recur (rejectTrial prior))))))))
      nil)))

(defn initializeBoard
  [arr]
  (map
    (fn [i]
      (if (= i 0)
        [1 2 3 4 5 6 7 8 9]
        i))
    arr))

(defn -main [& args]
  (println "Hello"))
