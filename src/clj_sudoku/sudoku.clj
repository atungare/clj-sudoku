(ns clj-sudoku.sudoku)
(use '[clojure.string :as s])

(defn xy->i
  [x y]
    (+ x (* 9 y)))

(defn i->xy
  [i]
  {:x (mod i 9) :y (quot i 9)})

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

(defn sections
  [board]
  (apply concat ((juxt rows cols grids) board)))

(defn myRow
  [i]
  (let [{x :x y :y} (i->xy i)]
    (for [a (range 0 9) :when (not= a x)]
      (xy->i a y))))

(defn myCol
  [i]
  (let [{x :x y :y} (i->xy i)]
    (for [b (range 0 9) :when (not= b y)]
      (xy->i x b))))

(defn myGrid
  [i]
  (let [{x :x y :y} (i->xy i)
         c (* 3 (quot x 3))
         d (* 3 (quot y 3))]
    (for [a (range c (+ c 3))
          b (range d (+ d 3))
          :when (not (and (= a x) (= b y)))]
      (xy->i a b))))

(defn noconflictSet
  [i]
  (set (apply concat ((juxt myRow myCol myGrid) i))))

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
        (let [neighbor_indices (noconflictSet idx)]
          (reduce
            (fn [options neighbor_index]
              (let [neighbor_val (get board neighbor_index)]
                (if (number? neighbor_val)
                  (remove #(= neighbor_val %) options)
                  options)))
            item
            neighbor_indices))))
    board))

(defn placeUniques
  [board]
  (map-indexed
    (fn [idx item]
      (if (number? item)
        item
        (let [neighbor_indices (noconflictSet idx)
              neighbors (map #(get board %) neighbor_indices)
              neighbor_values (set (flatten neighbors))
              only_possible (first (filter #(not (contains? neighbor_values %)) item))]
          (if (not (nil? only_possible))
            only_possible
            item))))
    board))

(defn iterateUntilNoChange
  [board transform]
  (loop [prior board
         new_board (transform board)]
    (if (= prior new_board)
      new_board
      (recur new_board (transform new_board))))
  board)

(defn solveBoard
  [board]
  (let [reduced_board (iterateUntilNoChange board (comp placeUniques strikeConflicts))]
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
  (println (->> (slurp "http://projecteuler.net/project/resources/p096_sudoku.txt")
             s/split-lines
             (partition 10)
             (map rest)
             (map #(s/join "" %))
             (map #(s/split % #""))
             (map (fn [arr] (map read-string arr)))
             (map initializeBoard)
             (map solveBoard)
             (map #(take 3 %))
             (map #(+ (* 100 (nth % 0)) (* 10 (nth % 1)) (* 1 (nth % 2))))
             (apply +))))
