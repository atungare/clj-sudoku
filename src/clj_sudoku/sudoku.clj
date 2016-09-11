(ns clj-sudoku.sudoku)

(defn xy->i
  [x y]
    (+ x (* 9 y)))

(defn i->xy
  [i]
  {:x (mod i 9) :y (quot i 9)})

(defn noconflictSet
  [i]
  (let [{x :x y :y} (i->xy i)
        row (for [a (range 0 9) :when (not= a x)]
              (xy->i a y))
        col (for [b (range 0 9) :when (not= b y)]
              (xy->i x b))
        c (* 3 (quot x 3))
        d (* 3 (quot y 3))
        grid (for [a (range c (+ c 3))
                   b (range d (+ d 3))
                   :when (not (and (= a x) (= b y)))]
               (xy->i a b))]
    (set (concat row col grid))))

(defn isComplete?
  [board]
  (every? number? board))

(defn hasConflict?
  [board i]
  (let [item (get board i)
        others (->> (noconflictSet i)
                    (map #(get board %))
                    (filter number?)
                    (remove zero?))]
    (if (number? item)
      (some #(= item %) others)
      false)))

(defn isBoardValid?
  [board]
  (let [all_conflicts (map #(hasConflict? board %) (range (count board)))]
    (not-any? true? all_conflicts)))

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
        (let [neigbor_indices (noconflictSet idx)]
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
      (recur new_board (strikeConflicts new_board))))
  board)

(defn solveBoard
  [board]
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
  (vec
    (map
      (fn [i]
        (if (= i 0)
          [1 2 3 4 5 6 7 8 9]
          i))
    arr)))

(defn -main [& args]
  (println "Hello"))
