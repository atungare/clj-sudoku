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


(def row-soln '((0 0 3 0 2 0 6 0 0) (9 0 0 3 0 5 0 0 1) (0 0 1 8 0 6 4 0 0) (0 0 8 1 0 2 9 0 0) (7 0 0 0 0 0 0 0 8) (0 0 6 7 0 8 2 0 0) (0 0 2 6 0 9 5 0 0) (8 0 0 2 0 3 0 0 9) (0 0 5 0 1 0 3 0 0)))
(deftest rows-test
  (testing "rows"
    (is (= (rows board) row-soln))))

(def col-soln '((0 9 0 0 7 0 0 8 0) (0 0 0 0 0 0 0 0 0) (3 0 1 8 0 6 2 0 5) (0 3 8 1 0 7 6 2 0) (2 0 0 0 0 0 0 0 1) (0 5 6 2 0 8 9 3 0) (6 0 4 9 0 2 5 0 3) (0 0 0 0 0 0 0 0 0) (0 1 0 0 8 0 0 9 0)))
(deftest cols-test
  (testing "cols"
    (is (= (cols board) col-soln))))
