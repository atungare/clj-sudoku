(ns clj-sudoku.sudoku-test
  (:require [clojure.test :refer :all]
            [clj-sudoku.sudoku :refer :all]))

(def board [0 0 3  0 2 0  6 0 0
            9 0 0  3 0 5  0 0 1
            0 0 1  8 0 6  4 0 0

            0 0 8  1 0 2  9 0 0
            7 0 0  0 0 0  0 0 8
            0 0 6  7 0 8  2 0 0

            0 0 2  6 0 9  5 0 0
            8 0 0  2 0 3  0 0 9
            0 0 5  0 1 0  3 0 0])

(def board_solved [4 8 3  9 2 1  6 5 7
                   9 6 7  3 4 5  8 2 1
                   2 5 1  8 7 6  4 9 3

                   5 4 8  1 3 2  9 7 6
                   7 2 9  5 6 4  1 3 8
                   1 3 6  7 9 8  2 4 5

                   3 7 2  6 8 9  5 1 4
                   8 1 4  2 5 3  7 6 9
                   6 9 5  4 1 7  3 8 2])

(def row-soln '((0 0 3 0 2 0 6 0 0) (9 0 0 3 0 5 0 0 1) (0 0 1 8 0 6 4 0 0) (0 0 8 1 0 2 9 0 0) (7 0 0 0 0 0 0 0 8) (0 0 6 7 0 8 2 0 0) (0 0 2 6 0 9 5 0 0) (8 0 0 2 0 3 0 0 9) (0 0 5 0 1 0 3 0 0)))

(def col-soln '((0 9 0 0 7 0 0 8 0) (0 0 0 0 0 0 0 0 0) (3 0 1 8 0 6 2 0 5) (0 3 8 1 0 7 6 2 0) (2 0 0 0 0 0 0 0 1) (0 5 6 2 0 8 9 3 0) (6 0 4 9 0 2 5 0 3) (0 0 0 0 0 0 0 0 0) (0 1 0 0 8 0 0 9 0)))

(def grids-soln '((0 9 0 0 0 0 3 0 1) (0 7 0 0 0 0 8 0 6) (0 8 0 0 0 0 2 0 5) (0 3 8 2 0 0 0 5 6) (1 0 7 0 0 0 2 0 8) (6 2 0 0 0 1 9 3 0) (6 0 4 0 0 0 0 1 0) (9 0 2 0 0 0 0 8 0) (5 0 3 0 0 0 0 9 0)))

(def boardToSections-soln '((0 0 3 0 2 0 6 0 0) (9 0 0 3 0 5 0 0 1) (0 0 1 8 0 6 4 0 0) (0 0 8 1 0 2 9 0 0) (7 0 0 0 0 0 0 0 8) (0 0 6 7 0 8 2 0 0) (0 0 2 6 0 9 5 0 0) (8 0 0 2 0 3 0 0 9) (0 0 5 0 1 0 3 0 0) (0 9 0 0 7 0 0 8 0) (0 0 0 0 0 0 0 0 0) (3 0 1 8 0 6 2 0 5) (0 3 8 1 0 7 6 2 0) (2 0 0 0 0 0 0 0 1) (0 5 6 2 0 8 9 3 0) (6 0 4 9 0 2 5 0 3) (0 0 0 0 0 0 0 0 0) (0 1 0 0 8 0 0 9 0) (0 9 0 0 0 0 3 0 1) (0 7 0 0 0 0 8 0 6) (0 8 0 0 0 0 2 0 5) (0 3 8 2 0 0 0 5 6) (1 0 7 0 0 0 2 0 8) (6 2 0 0 0 1 9 3 0) (6 0 4 0 0 0 0 1 0) (9 0 2 0 0 0 0 8 0) (5 0 3 0 0 0 0 9 0)))

(def pattern_board [1 2 3  4 5 6  7 8 9
                   4 5 6  7 8 9  1 2 3
                   7 8 9  1 2 3  4 5 6

                   2 3 4  5 6 7  8 9 1
                   5 6 7  8 9 1  2 3 4
                   8 9 1  2 3 4  5 6 7

                   3 4 5  6 7 8  9 1 2
                   6 7 8  9 1 2  3 4 5
                   9 1 2  3 4 5  6 7 8])

(def board_invalid [1 2 3  4 5 6  7 8 9
                    1 2 3  4 5 6  7 8 9
                    1 2 3  4 5 6  7 8 9

                    1 2 3  4 5 6  7 8 9
                    1 2 3  4 5 6  7 8 9
                    1 2 3  4 5 6  7 8 9

                    1 2 3  4 5 6  7 8 9
                    1 2 3  4 5 6  7 8 9
                    1 2 3  4 5 6  7 8 9])

(deftest ixy-test
  (testing "i->xy"
    (is (= (i->xy 1) {:x 1 :y 0}))
    (is (= (i->xy 16) {:x 7 :y 1}))
    (is (= (i->xy 54) {:x 0 :y 6}))
    (is (= (i->xy 80) {:x 8 :y 8}))))

(deftest noconflictSet-test
  (testing "noconflictSet"
    (is (= (noconflictSet 1) #{0 2 3 4 5 6 7 8 10 19 28 37 46 55 64 73 9 18 11 20}))
    (is (= (noconflictSet 45) #{0 72 27 46 54 48 50 36 29 28 51 47 9 53 38 18 52 37 63 49}))))

(deftest isComplete-test
  (testing "isComplete?"
    (is (= (isComplete? [1 2 3]) true))
    (is (= (isComplete? [1 1 3]) true))
    (is (= (isComplete? [1 [] 3]) false))))

(deftest isBoardValid-test
  (testing "isBoardValid?"
    (is (= (isBoardValid? (initializeBoard board)) true))
    (is (= (isBoardValid? (initializeBoard board_solved)) true))
    (is (= (isBoardValid? (initializeBoard board_invalid)) false))
    (is (= (isBoardValid? (initializeBoard pattern_board)) true))))

(deftest updateFirst-test
  (testing "updateFirst"
    (is (= (updateFirst #(= 0 %) (fn [_] 1) [1 2 3 0]) [1 2 3 1]))
    (is (= (updateFirst #(= 0 %) (fn [_] 1) [1 2 3 0 0]) [1 2 3 1 0]))))

(deftest solveBoard-test
  (testing "solveBoard"
    (is (= (solveBoard (initializeBoard board)) [board_solved]))))

