;;; drdv-2048.el --- The 2048 game

;; Copyright (C) 2017 Dimitar Dimitrov

;; Author: Dimitar Dimitrov <mail.mitko@gmail.com>
;; URL: https://github.com/drdv/drdv-2048
;; Package-Version: 20170503.1
;; Package-Requires: ()
;; Keywords: games

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Package tested on:
;; GNU Emacs 25.2.1 (x86_64-apple-darwin16.5.0)

;; Given that there is 2048-game.el, I wrote this because I simply wanted
;; to have some practice using Lisp, to have undo (requested by my daughter)
;; and to be able to replay saved games (for no good reason).
;;
;; I have borrowed a few things from 2048-game.el
;;   - the colors (they are simply reasonable)
;;   - the trick (intern (concat "drdv-2048-face-" (int-to-string number)))

;; Quick start:
;;
;; add (require 'drdv-2048) to your .emacs

;;; Code:

(require 'json)

(defvar drdv-2048-dimension 4
  "Dimension of the (square) board.")

(defvar drdv-2048-board (make-vector (* drdv-2048-dimension drdv-2048-dimension)
				     0)
  "The board (zero-based column major storage).")

(defvar drdv-2048-number-of-moves 0
  "Number of valid moves performed.")

(defvar drdv-2048-moves-history nil
  "Valid moves history.")

(defvar drdv-2048-last-move nil
  "Most recent valid move.
When playing, this is the car or `drdv-2048-moves-history',
but it is convenient to have a dedicated variable when
replaying with `drdv-2048-replay'.")

(defvar drdv-2048-score 0
  "The score.")

(defvar drdv-2048-score-history nil
  "The score history.")

(defvar drdv-2048-undo-stack nil
  "Stack to store previous state of `drdv-2048-board'.")

(defvar drdv-2048-initial-board nil
  "Initial state of `drdv-2048-board'.")

(defvar drdv-2048-replay-wait-period 0.01
  "Waiting period between redisplays in `drdv-2048-replay'.")

(defvar drdv-2048-replay-save-images "macos"
  "Set to nil to not not output images in `drdv-2048-replay'.")

(defvar drdv-2048-buffer-name "*drdv-2048*"
  "Name of buffer where to display the board.")

(defvar drdv-2048-possible-values-to-insert-randomly (make-vector 10 2)
  "Randomly choose a number from this sequence upon insertion of new element.")
;; 90% of the time insert 2 (the remaining 10% insert 4)
(aset drdv-2048-possible-values-to-insert-randomly 9 4)

(defvar drdv-2048-inserted-random-elements-history nil
  "History of inserted random elements.")

(defvar drdv-2048-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>"   ) 'drdv-2048-up)
    (define-key map (kbd "<down>" ) 'drdv-2048-down)
    (define-key map (kbd "<left>" ) 'drdv-2048-left)
    (define-key map (kbd "<right>") 'drdv-2048-right)
    (define-key map (kbd "u"      ) 'drdv-2048-undo)
    (define-key map (kbd "w"      ) 'drdv-2048-record-history)
    map)
  "Keymap for drdv-2048 major mode.")



(defgroup drdv-2048 nil
  "The 2048 game."
  :group 'games)

(defgroup drdv-2048-faces nil
  "The 2048 game related phases."
  :group 'drdv-2048
  :group 'faces)

;; taken directly from 2048-game.el
(defface drdv-2048-face-2
  '((t . (:inherit highlight
		   :background "khaki"
		   :foreground "black")))
  "Face for 2"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-4
  '((t . (:inherit highlight
		   :background "burlywood"
		   :foreground "black")))
  "Face for 4"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-8
  '((t . (:inherit highlight
		   :background "orange3"
		   :foreground "black")))
  "Face for 8"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-16
  '((t . (:inherit highlight
		   :background "orange"
		   :foreground "black")))
  "Face for 16"
  :group 'drdv-2048-faces)
(defface drdv-2048-face-32
  '((t . (:inherit highlight
		   :background "orange red"
		   :foreground "black")))
  "Face for 32"
  :group 'drdv-2048-faces)
(defface drdv-2048-face-64
  '((t . (:inherit highlight
		   :background "firebrick"
		   :foreground "white")))
  "Face for 64"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-128
  '((t . (:inherit highlight
		   :background "dark red"
		   :foreground "white")))
  "Face for 128"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-256
  '((t . (:inherit highlight
		   :background "dark magenta"
		   :foreground "white")))
  "Face for 256"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-512
  '((t . (:inherit highlight
		   :background "magenta"
		   :foreground "black")))
  "Face for 512"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-1024
  '((t . (:inherit highlight
		   :background "gold"
		   :foreground "black")))
  "Face for 1024"
  :group 'drdv-2048-faces)

(defface drdv-2048-face-2048
  '((t . (:inherit highlight
		   :background "yellow"
		   :foreground "black")))
  "Face for 2048"
  :group 'drdv-2048-faces)



(defun drdv-2048-play ()
  "Start/restart the game."
  (interactive)
  (switch-to-buffer drdv-2048-buffer-name)
  (buffer-disable-undo drdv-2048-buffer-name)
  (drdv-2048-mode)
  (drdv-2048-reset-game)
  (drdv-2048-display-board))

(defun drdv-2048-move (row_or_column direction)
  "Perform a move.
ROW_OR_COLUMN is a string specifying whether to move along columns or rows
possible values: {\"row\", \"col\"}. If argument DIRECTION is 1, move from
index 0 to index drdv-2048-dimension-1, otherwise move from index
drdv-2048-dimension-1 to index 0."
  (let ((scored-in-this-move 0)
	vector-modified-score
	modified)
    (dotimes (k drdv-2048-dimension)
      ;; vector-modified-score is a list
      ;; ("the new vector" "modified or not" "score")
      (setq vector-modified-score
	    (drdv-2048-move1d (drdv-2048-get-board-vector k row_or_column)
			      direction))
      ;; if necessary record back in drdv-2048-board and update score
      (when (nth 1 vector-modified-score)
	(drdv-2048-set-board-vector k (car vector-modified-score) row_or_column)
	;; note: the modification of a vector could result in zero score
	(setq scored-in-this-move (+ scored-in-this-move
				     (nth 2 vector-modified-score))))
      ;; update output
      (setq modified (or modified
			 (nth 1 vector-modified-score))))
    ;; add scored-in-this-move to the total score
    (setq drdv-2048-score (+ drdv-2048-score
			     scored-in-this-move))
    ;; push scored-in-this-move to the score history
    (push scored-in-this-move drdv-2048-score-history)
    ;; return whether the board has been modified
    modified))



(defun drdv-2048-up ()
  "User interface function (move up)."
  (interactive)
  (let ((backup-board (copy-sequence drdv-2048-board))
	(modified (drdv-2048-move "col" 1)))
    (when modified
      (push "U" drdv-2048-moves-history)
      (setq drdv-2048-last-move "U")
      (push backup-board drdv-2048-undo-stack)
      (setq drdv-2048-number-of-moves (1+ drdv-2048-number-of-moves))
      (drdv-2048-insert-random-element)
      (drdv-2048-display-board))))

(defun drdv-2048-down ()
  "User interface function (move down)."
  (interactive)
  (let ((backup-board (copy-sequence drdv-2048-board))
	(modified (drdv-2048-move "col" -1)))
    (when modified
      (setq drdv-2048-last-move "D")
      (push "D" drdv-2048-moves-history)
      (push backup-board drdv-2048-undo-stack)
      (setq drdv-2048-number-of-moves (1+ drdv-2048-number-of-moves))
      (drdv-2048-insert-random-element)
      (drdv-2048-display-board))))

(defun drdv-2048-left ()
  "User interface function (move left)."
  (interactive)
  (let ((backup-board (copy-sequence drdv-2048-board))
	(modified (drdv-2048-move "row" 1)))
    (when modified
      (setq drdv-2048-last-move "L")
      (push "L" drdv-2048-moves-history)
      (push backup-board drdv-2048-undo-stack)
      (setq drdv-2048-number-of-moves (1+ drdv-2048-number-of-moves))
      (drdv-2048-insert-random-element)
      (drdv-2048-display-board))))

(defun drdv-2048-right ()
  "User interface function (move right)."
  (interactive)
  (let ((backup-board (copy-sequence drdv-2048-board))
	(modified (drdv-2048-move "row" -1)))
    (when modified
      (setq drdv-2048-last-move "R")
      (push "R" drdv-2048-moves-history)
      (push backup-board drdv-2048-undo-stack)
      (setq drdv-2048-number-of-moves (1+ drdv-2048-number-of-moves))
      (drdv-2048-insert-random-element)
      (drdv-2048-display-board))))

(define-derived-mode drdv-2048-mode special-mode "drdv-2048")



(defun drdv-2048-row-col-from-index (index)
  "Return the [row, col] corresponding to [INDEX].
Assuming zero-based column major storage."
  (when (>= index (* drdv-2048-dimension drdv-2048-dimension))
    (error "Index is out of range"))
  `(,(mod index drdv-2048-dimension) . ,(/ index drdv-2048-dimension)))

(defun drdv-2048-get-index-of-zero-elements ()
  "Return indexes of nonzero elements of `drdv-2048-board'."
  (let (index)
    (dotimes (k (* drdv-2048-dimension drdv-2048-dimension))
      (when (= (elt drdv-2048-board k) 0)
	(push k index)))
    (reverse index)))

(defun drdv-2048-get-element (row col)
  "Get `drdv-2048-board'[ROW, COL]."
  (if (or (>= row drdv-2048-dimension)
	  (>= col drdv-2048-dimension))
      (error "Inconsistent dimensions")
    (elt drdv-2048-board (+ row (* drdv-2048-dimension col)))))

(defun drdv-2048-set-element (row col value)
  "Set `drdv-2048-board'[ROW, COL] = VALUE."
  (if (or (>= row drdv-2048-dimension)
	  (>= col drdv-2048-dimension))
      (error "Inconsistent dimensions")
    (aset drdv-2048-board (+ row (* drdv-2048-dimension col)) value)))

(defun drdv-2048-get-board-vector (index row_or_column)
  "Return a row or column with a given INDEX.
If ROW_OR_COLUMN is equal to \"col\" return the INDEX-th column
If ROW_OR_COLUMN is equal to \"row\" return the INDEX-th row."
  (if (>= index drdv-2048-dimension)
      (error "Inconsistent dimensions")
    (let ((vector (make-vector drdv-2048-dimension 0)))
      (dotimes (k drdv-2048-dimension)
	(if (equal row_or_column "col")
	    (aset vector k (drdv-2048-get-element k index))
	  (aset vector k (drdv-2048-get-element index k))))
      vector)))

(defun drdv-2048-set-board-vector (index vector row_or_column)
  "Set a row or column with a given INDEX equal to VECTOR.
If ROW_OR_COLUMN is equal to \"col\" set the INDEX-th column
If ROW_OR_COLUMN is equal to \"row\" set the INDEX-th row."
  (if (>= index drdv-2048-dimension)
      (error "Inconsistent dimensions")
    (dotimes (k drdv-2048-dimension)
      (if (equal row_or_column "col")
	  (drdv-2048-set-element k index (elt vector k))
	(drdv-2048-set-element index k (elt vector k))))))

(defun drdv-2048-move1d (vector direction)
  "Move elements of VECTOR according to the rules of the game.
If argument DIRECTION is 1, move from index 0 to index drdv-2048-dimension-1,
otherwise move from index drdv-2048-dimension-1 to index 0."
  (let ((previous 0)
	(updated-vector (make-vector drdv-2048-dimension 0))
	(sign (if (= direction 1)
		  1
		-1))
	(j (if (= direction 1)
	       0
	     (1- drdv-2048-dimension)))
	(span (if (= direction 1)
		  (number-sequence 0 (1- drdv-2048-dimension) 1)
		(number-sequence (1- drdv-2048-dimension) 0 -1)))
	(modified nil)
	(scored 0))
    (dolist (i span)
      ;; jump over zero entries
      (when (not (= (elt vector i) 0))
	(if (= previous 0)
	    ;; THEN-1
	    (setq previous (elt vector i))
	  ;; ELSE-1
	  (if (= previous (elt vector i))
	      ;; THEN-2
	      (progn
		(aset updated-vector j (* 2 previous))
		(setq scored (+ scored
				(* 2 previous)))
		(setq previous 0))
	    ;; ELSE-2
	    (progn
	      (aset updated-vector j previous)
	      (setq previous (elt vector i))))
	  ;; belongs to ELSE-1 but not inside the second IF
	  (setq j (+ sign j))))
      (when (not (= previous 0))
	(aset updated-vector j previous)))
    ;; return the updated vector
    (when (not (equal updated-vector vector))
      (setq modified t))
    `(,updated-vector ,modified ,scored)))

(defun drdv-2048-get-face (row col)
  "Return nil if number at ROW COL is zero and appropriate face otherwise."
  (let ((number (drdv-2048-get-element row col)))
    (when (not (= number 0))
      (intern (concat "drdv-2048-face-"
		      (int-to-string number))))))

(defun drdv-2048-undo ()
  "Undo when playing (but not when replaying)."
  (interactive)
  ;; limit the undo
  (when (> drdv-2048-number-of-moves 0)
    (setq drdv-2048-board
	  (copy-sequence (pop drdv-2048-undo-stack)))
    (pop drdv-2048-moves-history)
    (setq drdv-2048-last-move (car drdv-2048-moves-history))
    (setq drdv-2048-number-of-moves (1- drdv-2048-number-of-moves))
    (setq drdv-2048-score (- drdv-2048-score (pop drdv-2048-score-history)))
    (drdv-2048-display-board)))

(defun drdv-2048-insert-random-element (&optional dont-record)
  "Insert random element at a randomly chosen cell with a zero value.
The random element can be 2 (90%) or 4 (10% of the time).
Set DONT-RECORD non-nil to not record history (useful when initializing
`drdv-2048-board')."
  (let* ((zero-elements (drdv-2048-get-index-of-zero-elements))
	 (index-1
	  (random (length zero-elements)))
	 (index-2
	  (random (length drdv-2048-possible-values-to-insert-randomly))))
    (aset drdv-2048-board
	  (elt zero-elements index-1)
	  (elt drdv-2048-possible-values-to-insert-randomly index-2))
    (unless dont-record
      (push (vector (elt zero-elements index-1)
		    (elt drdv-2048-possible-values-to-insert-randomly index-2))
	    drdv-2048-inserted-random-elements-history))))

(defun drdv-2048-reset-game ()
  "Reset the game."
  (setq drdv-2048-board (make-vector (* drdv-2048-dimension drdv-2048-dimension)
				     0))
  (setq drdv-2048-moves-history nil)
  (setq drdv-2048-score-history nil)
  (setq drdv-2048-inserted-random-elements-history nil)
  (setq drdv-2048-undo-stack nil)
  (setq drdv-2048-number-of-moves 0)
  (setq drdv-2048-last-move nil)
  (setq drdv-2048-score 0)
  (drdv-2048-insert-random-element t)
  (drdv-2048-insert-random-element t)
  (setq drdv-2048-initial-board (copy-sequence drdv-2048-board)))

(defun drdv-2048-display-board ()
  "Display the board."
  ;; make sure that this function is executed only in the correct buffer
  (when (not (equal (buffer-name) drdv-2048-buffer-name))
    (error (format "We are not in buffer %s" drdv-2048-buffer-name)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row drdv-2048-dimension)
      ;; draw top line
      (dotimes (col drdv-2048-dimension)
	(insert "+--------"))
      (insert "+\n")

      ;; insert empty line above the number
      (dotimes (col drdv-2048-dimension)
	(let ((point (point)))
	  (insert "|        ")
	  (put-text-property (+ point 1) (+ point 9)
			     'font-lock-face (drdv-2048-get-face row col))))
      (insert "|\n")

      ;; insert the number
      (dotimes (col drdv-2048-dimension)
	(let ((point (point)))
	  (insert (format "| %4d   " (drdv-2048-get-element row col)))
	  (put-text-property (+ point 1) (+ point 9)
			     'font-lock-face (drdv-2048-get-face row col))))
      (insert "|\n")

      ;; insert empty line below the number
      (dotimes (col drdv-2048-dimension)
	(let ((point (point)))
	  (insert "|        ")
	  (put-text-property (+ point 1) (+ point 9)
			     'font-lock-face (drdv-2048-get-face row col))))
      (insert "|\n")
      )

    ;; insert the closing line
    (dotimes (col drdv-2048-dimension)
      (insert "+--------"))
    (insert "+\n\n")

    (insert (format "moves: %d (%s)\n"
		    drdv-2048-number-of-moves
		    drdv-2048-last-move))
    (insert (format "score: %d\n"
		    drdv-2048-score))

    ;; notify that the game has ended (and propose to restart)
    (when (drdv-2048-game-has-ended-p)
      (insert "\n\n")
      (let ((point (point)))
	(insert "      =====> GAME OVER <=====")
	(put-text-property (+ point 6) (+ point 29)
			   'font-lock-face 'drdv-2048-face-16)
	)
      (when (y-or-n-p "Press y to start again.  Start again? ")
	(drdv-2048-play)))))



(defun drdv-2048-replay (filename)
  "Replay a previously stored game in FILENAME."
  (interactive "fFilename: ")
  (let* ((a (json-read-file filename))
	 (initial-board (cdr (assoc 'initial-board a)))
	 (moves-history (cdr (assoc 'moves a)))
	 (random-elements-history (cdr (assoc 'random-elements a)))
	 index-and-value)

    ;; vector -> list (and reverse it)
    (setq moves-history
	  (reverse (mapcar (lambda (x) x) moves-history)))
    (setq random-elements-history
	  (reverse (mapcar (lambda (x) x) random-elements-history)))
    (setq drdv-2048-board (copy-sequence initial-board))

    ;; reset stuff
    (setq drdv-2048-moves-history nil)
    (setq drdv-2048-inserted-random-elements-history nil)
    (setq drdv-2048-score-history nil)
    (setq drdv-2048-undo-stack nil)
    (setq drdv-2048-number-of-moves 0)
    (setq drdv-2048-last-move nil)
    (setq drdv-2048-score 0)

    (switch-to-buffer drdv-2048-buffer-name)
    (buffer-disable-undo drdv-2048-buffer-name)
    (drdv-2048-mode)
    (drdv-2048-display-board)
    (sit-for drdv-2048-replay-wait-period)
    (dotimes (k (length moves-history))
      (setq drdv-2048-last-move (nth k moves-history))
      (setq index-and-value (nth k random-elements-history))
      (cond
       ((equal drdv-2048-last-move "U") (drdv-2048-move "col"  1))
       ((equal drdv-2048-last-move "D") (drdv-2048-move "col" -1))
       ((equal drdv-2048-last-move "L") (drdv-2048-move "row"  1))
       ((equal drdv-2048-last-move "R") (drdv-2048-move "row" -1)))
      (aset drdv-2048-board
	    (elt index-and-value 0)
	    (elt index-and-value 1))
      (setq drdv-2048-number-of-moves (1+ drdv-2048-number-of-moves))
      (drdv-2048-display-board)
      (sit-for drdv-2048-replay-wait-period)
      (when (equal drdv-2048-replay-save-images "macos")
	;;   0, 50  are the x and -y from the top left corner
	;;          (50 is appropriate when the terminal has tabs)
	;; 340, 340 are width and height (appropriate for the size of the board)
	(shell-command
	 (concat "screencapture -R 0,50,340,340 -x -t jpg ./output/image-"
		 (format "%06d" (1+ k)) ".jpg"))))))

(defun drdv-2048-record-history (filename)
  "Store the game history in FILENAME."
  (interactive "fFilename: ")
  (let ((json-encoding-pretty-print t))
    (write-region (json-encode `(("initial-board" . ,drdv-2048-initial-board)
				 ("final-board" . ,drdv-2048-board)
				 ("score" . ,drdv-2048-score)
				 ("number-moves" . ,drdv-2048-number-of-moves)
				 ("moves" . ,drdv-2048-moves-history)
				 ("random-elements" .
				  ,drdv-2048-inserted-random-elements-history)
				 ;; ("scores" . ,drdv-2048-score-history)
				 ))
		  nil filename)))



(defun drdv-2048-game-has-ended-p ()
  "Return true if the game has ended, return nil otherwise."
  (let (modified)
    (dotimes (k drdv-2048-dimension)
      (setq modified
	    (or modified
		(nth 1 (drdv-2048-move1d (drdv-2048-get-board-vector k "row")
					 1))
		(nth 1 (drdv-2048-move1d (drdv-2048-get-board-vector k "row")
					 -1))
		(nth 1 (drdv-2048-move1d (drdv-2048-get-board-vector k "col")
					 1))
		(nth 1 (drdv-2048-move1d (drdv-2048-get-board-vector k "col")
					 -1)))))
    (not modified)))

(provide 'drdv-2048)

;;; drdv-2048.el ends here
