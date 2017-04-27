(ert-deftest test-drdv-2048-move1d-plus ()
  "Test `drdv-2048-move1d' with direction = 1"
  (let ((test-pairs '(([2 0 2 0] . ([4 0 0 0] t 4))
		      ([2 4 4 2] . ([2 8 2 0] t 8)))))
    (dolist (pair test-pairs)
      (should (equal (drdv-2048-move1d (car pair) 1) (cdr pair))))))

(ert-deftest test-drdv-2048-move1d-minus ()
  "Test `drdv-2048-move1d' with direction = -1"
  (let ((test-pairs '(([2 0 2 0] . ([0 0 0 4] t 4))
		      ([2 4 4 2] . ([0 2 8 2] t 8)))))
    (dolist (pair test-pairs)
      (should (equal (drdv-2048-move1d (car pair) -1) (cdr pair))))))

;;; EOF
