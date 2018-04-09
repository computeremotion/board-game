;;; board-game.el --- Flashcard game for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2018

;; Author: Florian Tauber
;; URL: https://github.com/computeremotion/board-game

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program provides a mode for playing a flashcard board game.

;;; Code:

(require 'seq)

;; Decreased value for fast feedback.
;; Increase value for doing real load test.
(defconst LOAD-TEST-INPUT-SIZE 10
  "The default input size for doing load tests.")
(defconst ARBITRARY-INTEGER-FOR-TESTING 42
  "This arbitrary Integer is used for testing.")

;;;; Model:

;; a Game is a Sequence of Move elements
(defconst GAME-0 []
  "The initial game.  ( number of moves played: 0 ).")

;; a Move is one of:
(defconst :FLIP ':FLIP
  "Flip the card on top.")
(defconst :YEA ':YEA
  "When answer correct, remove card from deck.")
(defconst :NAY ':NAY
  "When answer wrong, put card into back of deck.")
(defconst :QUIT ':QUIT
  "When user wants to quit playing the game.")
(defconst :RESET ':RESET
  "Reset game to initial state.")
(defconst :SHUFFLE ':SHUFFLE
  "Shuffle the cards.")
;; String ( representing an answer :GUESS move )

(ert-deftest move-legal-p-test ()
  (should (move-legal-p GAME-0 ":GUESS move"))
  (should (not (move-legal-p GAME-0 'any-old-symbol))))
;; Game Move -> Boolean
(defun move-legal-p (game move)
  "Check if in GAME it is legal to play MOVE."
  (pcase move
    (:FLIP (flip-legal-p game))
    (:YEA (yea-legal-p game))
    (:NAY (nay-legal-p game))
    (:QUIT (quit-legal-p game))
    (:RESET (reset-legal-p game))
    (:SHUFFLE (shuffle-legal-p game))
    ((pred stringp) (guess-legal-p game))
    (_ nil)))

(ert-deftest flip-legal-p-test ()
  (should (not (flip-legal-p (seq-reduce #'play
                                         [:FLIP :YEA :FLIP :NAY :QUIT]
                                         GAME-0))))
  (should (flip-legal-p GAME-0))
  (should (flip-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP :NAY] GAME-0))))
;; Game -> Boolean
(defun flip-legal-p (game)
  "Check if it is legal to play a flip move in current GAME."
  (not (last-move-equal game :QUIT)))

(ert-deftest yea-legal-p-test ()
  (should
   (not (yea-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT] GAME-0))))
  (should (not (yea-legal-p GAME-0)))
  (should
   (not (yea-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP :NAY] GAME-0))))
  (should (yea-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP] GAME-0))))
;; Game -> Boolean
(defun yea-legal-p (game)
  "Check if it is legal to play a yea move in current GAME."
  (last-move-equal game :FLIP))

(ert-deftest nay-legal-p-test ()
  (should
   (not (nay-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT] GAME-0))))
  (should (not (nay-legal-p GAME-0)))
  (should
   (not (nay-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP :NAY] GAME-0))))
  (should (nay-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP] GAME-0))))
;; Game -> Boolean
(defun nay-legal-p (game)
  "Check if it is legal to play a nay move in current GAME."
  (last-move-equal game :FLIP))

(ert-deftest quit-legal-p-test ()
  (should (quit-legal-p GAME-0))
  (should (quit-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP :NAY] GAME-0)))
  (should
   (not
    (quit-legal-p (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT] GAME-0)))))
;; Game -> Boolean
(defun quit-legal-p (game)
  "Check if it is legal to play a quit move in current GAME."
  (not (last-move-equal game :QUIT)))

(ert-deftest reset-legal-p-test ()
  (should (not (reset-legal-p GAME-0)))
  (should
   (reset-legal-p
    (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT] GAME-0)))
  (should
   (not
    (reset-legal-p
     (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT :RESET] GAME-0)))))
;; Game -> Boolean
(defun reset-legal-p (game)
  "Check if it is legal to play a reset move in current GAME."
  (and (not (seq-empty-p game))
       (not (last-move-equal game :RESET))))

(ert-deftest shuffle-legal-p-test ()
  (should (shuffle-legal-p GAME-0))
  (should
   (shuffle-legal-p
    (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT :RESET] GAME-0)))
  (should
   (not
    (shuffle-legal-p
     (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT] GAME-0)))))
;; Game -> Boolean
(defun shuffle-legal-p (game)
  "Check if it is legal to play a shuffle move in current GAME."
  (not (last-move-equal game :QUIT)))

(ert-deftest guess-legal-p-test ()
  (should (guess-legal-p GAME-0))
  (should
   (guess-legal-p
    (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT :RESET] GAME-0)))
  (should
   (guess-legal-p
    (seq-reduce #'play [:FLIP :FLIP] GAME-0)))
  (should
   (guess-legal-p
    (seq-reduce #'play [:FLIP :FLIP :FLIP :FLIP] GAME-0)))
  (should
   (not
    (guess-legal-p
     (seq-reduce #'play [:FLIP] GAME-0))))
  (should
   (not
    (guess-legal-p
     (seq-reduce #'play [:FLIP :FLIP :FLIP] GAME-0))))
  (should
   (not
    (guess-legal-p
     (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :QUIT] GAME-0)))))
;; Game -> Boolean
(defun guess-legal-p (game)
  "Check if it is legal to play a guess move in current GAME."
  (and (not (board-is-answer-up (setup-board game)))
       (not (last-move-equal game :QUIT))))

(ert-deftest last-move-equal-test ()
  (should (not (last-move-equal GAME-0 :FLIP)))
  (should (not (last-move-equal GAME-0 :NAY)))
  (should (not (last-move-equal GAME-0 :YEA)))
  ;; load test
  (should
   (last-move-equal
    (seq-reduce #'play (make-vector LOAD-TEST-INPUT-SIZE :FLIP) GAME-0) :FLIP))
  (should
   (last-move-equal
    (seq-reduce #'play [:FLIP :FLIP :FLIP] GAME-0) :FLIP))
  (should
   (last-move-equal
    (seq-reduce #'play [:FLIP :FLIP :YEA] GAME-0) :YEA)))
;; Game Move -> Boolean
(defun last-move-equal (game move)
  "Check if in given GAME last move was MOVE."
  (if (seq-empty-p game)
      nil
    (let* ((len (seq-length game))
           (index-of-last-ele (1- len))
           (last-elt (seq-elt game index-of-last-ele)))
      (equal last-elt move))))

(ert-deftest play-test ()
  (should (equal (play GAME-0 :FLIP)
                 [:FLIP]))
  (should (equal (play (play GAME-0 :FLIP) :YEA)
                 [:FLIP :YEA]))
  (should (equal (seq-reduce #'play [:FLIP :YEA :FLIP :NAY] GAME-0)
                 [:FLIP :YEA :FLIP :NAY]))
  (should (equal (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :SHUFFLE] GAME-0)
                 [:FLIP :YEA :FLIP :NAY :SHUFFLE]))
  (should
   (equal
    (seq-reduce
     #'play [:FLIP :YEA :FLIP :NAY :QUIT :QUIT :QUIT :FLIP :NAY] GAME-0)
    [:FLIP :YEA :FLIP :NAY :QUIT]))
  (should
   (equal
    (seq-reduce
     #'play [:FLIP :YEA :FLIP :NAY :QUIT :QUIT :QUIT :FLIP :NAY :RESET] GAME-0)
    [:FLIP :YEA :FLIP :NAY :QUIT :RESET]))
  (should (equal (seq-reduce #'play [:FLIP :YEA :FLIP :NAY :NAY :YEA] GAME-0)
                 [:FLIP :YEA :FLIP :NAY])))
;; Game Move -> Game
(defun play (game move)
  "Play the current GAME by doing MOVE."
  (if (move-legal-p game move)
      (seq-concatenate 'vector game (list move))
    game))

;; a Card is a structure
(cl-defstruct
    (card
     ;; String String -> Card
     (:constructor new-card (question answer)))
  question answer)

(defconst HIRAGANA-TABLE
  '((あ い う え お)
    (か き く け こ きゃ きゅ きょ)
    (さ し す せ そ しゃ しゅ しょ)
    (た ち つ て と ちゃ ちゅ ちょ)
    (な に ぬ ね の にゃ にゅ にょ)
    (は ひ ふ へ ほ ひゃ ひゅ ひょ)
    (ま み む め も みゃ みゅ みょ)
    (や    ゆ   よ)
    (ら り る れ ろ りゃ りゅ りょ)
    (わ       を)
    (ん)
    (が ぎ ぐ げ ご ぎゃ ぎゅ ぎょ)
    (ざ じ ず ぜ ぞ じゃ じゅ じょ)
    (だ      で ど)
    (ば び ぶ べ ぼ びゃ びゅ びょ)
    (ぱ ぴ ぷ ぺ ぽ ぴゃ ぴゅ ぴょ))
  "A table with hiragana characters.")

(defconst KATAKANA-TABLE
  '((ア イ ウ エ オ)
    (カ キ ク ケ コ キャ キュ キョ)
    (サ シ ス セ ソ シャ シュ ショ)
    (タ チ ツ テ ト チャ チュ チョ)
    (ナ ニ ヌ ネ ノ ニャ ニュ ニョ)
    (ハ ヒ フ ヘ ホ ヒャ ヒュ ヒョ)
    (マ ミ ム メ モ ミャ ミュ ミョ)
    (ヤ   ユ    ヨ)
    (ラ リ ル レ ロ リャ リュ リョ)
    (ワ         ヲ)
    (ン)
    (ガ ギ グ ゲ ゴ ギャ ギュ ギョ)
    (ザ ジ ズ ゼ ゾ ジャ ジュ ジョ)
    (ダ      デ ド)
    (バ ビ ブ ベ ボ ビャ ビュ ビョ)
    (パ ピ プ ペ ポ ピャ ピュ ピョ))
  "Table with katakana characters.")

(defconst ROMANJI-TABLE
  '((a i u e o)
    (ka ki ku ke ko kya kyu kyo)
    (sa shi su se so sha shu sho)
    (ta chi tsu te to cha chu cho)
    (na ni nu ne no nya nyu nyo)
    (ha hi fu he ho hya hyu hyo)
    (ma mi mu me mo mya myu myo)
    (ya    yu    yo)
    (ra ri ru re ro rya ryu ryo)
    (wa          wo)
    (n)
    (ga gi gu ge go gya gyu gyo)
    (za ji zu ze zo ja ju jo)
    (da       de do)
    (ba bi bu be bo bya byu byo)
    (pa pi pu pe po pya pyu pyo))
  "A table with roman characters.")

(ert-deftest flatten-test ()
  (should (equal (flatten '((1 2 3)
                            (4 5 6)
                            (7 8 9)))
                 '(1 2 3 4 5 6 7 8 9)))
  (should (equal (flatten '(()
                            ()
                            ()
                            ()))
                 '()))
  (should (equal (flatten '())
                 '())))
;; List<List<X>> -> List<X>
(defun flatten (alol)
  "Return a single list from a list of lists ALOL."
  (seq-reduce (apply-partially #'seq-concatenate 'list)
              alol
              '()))

(ert-deftest zip-test ()
  (should (equal (zip '() '())
                 '()))
  (should (equal (zip '(1) '(2))
                 '((1 . 2))))
  (should (equal (zip '(1 2 3 "hello")
                      '(4 5 6 "world"))
                 '((1 . 4)
                   (2 . 5)
                   (3 . 6)
                   ("hello" . "world")))))
;; List<X> List<Y> -> List<Pair<X,Y>>
(defun zip (lis1 lis2)
  "Produce a List of Pairs from LIS1 and LIS2.
Note that the two input lists should be of equal length."
  (if (seq-empty-p lis1)
      lis1
    (cons (cons (car lis1)
                (car lis2))
          (zip (cdr lis1)
               (cdr lis2)))))

(ert-deftest map-pair-to-card-test ()
  (should (equal (map-pair-to-card '((a . b)
                                     (c . d)))
                 (list (new-card "a" "b")
                       (new-card "c" "d"))))
  (should (equal (map-pair-to-card '())
                 '())))
;; List<Pair<Symbol,Symbol>> -> List<Card>
(defun map-pair-to-card (alop)
  "Map a List of Pair elements ( ALOP ) onto a List of Card elements."
  (seq-map (lambda (pair)
             (new-card (symbol-name (car pair))
                       (symbol-name (cdr pair))))
           alop))

(defconst CARDS-0
  (append
   (map-pair-to-card (zip (flatten HIRAGANA-TABLE)
                          (flatten ROMANJI-TABLE)))
   (map-pair-to-card (zip (flatten KATAKANA-TABLE)
                          (flatten ROMANJI-TABLE))))
  "The initial flashcards.  ( number of moves played: 0 ).")
(defconst FIRST-CARD-0 (car CARDS-0)
  "The first card in the initial deck of cards.")
(defconst FIRST-CARD-0-ANSWER (card-answer FIRST-CARD-0)
  "The answer of the first card in the deck.")
(defconst FIRST-CARD-0-WRONG-ANSWER (format "not-%s" FIRST-CARD-0-ANSWER)
  "Wrong answer to question of first card in deck of cards.")

;; a Board is a structure
(cl-defstruct
    (board
     ;; List<Card> Boolean Boolean -> Board
     (:constructor new-board (cards &optional is-answer-up is-quit)))
  cards is-answer-up is-quit)

(defconst BOARD-0 (new-board CARDS-0)
  "The initial board.  ( number of moves played: 0 ).")

(ert-deftest setup-board-test ()
  (should (equal (setup-board GAME-0)
                 BOARD-0))
  (should (equal (setup-board (list))
                 (new-board CARDS-0)))
  (should (equal (setup-board (seq-reduce #'play [:FLIP] GAME-0))
                 (new-board CARDS-0
                            t)))
  (should (equal (setup-board (seq-reduce #'play [:FLIP :FLIP] GAME-0))
                 BOARD-0))
  (should
   (equal
    (setup-board
     (seq-reduce #'play (make-vector LOAD-TEST-INPUT-SIZE :FLIP) GAME-0))
    BOARD-0))
  (should (equal (setup-board (seq-reduce #'play [:FLIP :FLIP :FLIP] GAME-0))
                 (new-board CARDS-0
                            t)))
  (should
   (equal (setup-board (seq-reduce #'play [:FLIP :FLIP :FLIP :QUIT] GAME-0))
          (new-board CARDS-0
                     t
                     t)))
  (should
   (equal (setup-board (seq-reduce #'play [:FLIP :FLIP :FLIP :YEA] GAME-0))
          (new-board (cdr CARDS-0))))
  (should
   (equal (setup-board (seq-reduce #'play (vector FIRST-CARD-0-ANSWER) GAME-0))
          (new-board (cdr CARDS-0))))
  (should
   (equal (seq-length
           (board-cards (setup-board (seq-reduce #'play [:SHUFFLE] GAME-0))))
          (seq-length CARDS-0)))
  (should
   (equal (setup-board (seq-reduce #'play [:FLIP :FLIP :FLIP :NAY] GAME-0))
          (new-board (append (cdr CARDS-0)
                             (list (car CARDS-0))))))
  (should
   (equal
    (setup-board (seq-reduce #'play (vector FIRST-CARD-0-WRONG-ANSWER) GAME-0))
    (new-board (append (cdr CARDS-0)
                       (list (car CARDS-0))))))
  (should
   (equal
    (setup-board (seq-reduce #'play [:FLIP :FLIP :FLIP :NAY :FLIP :NAY] GAME-0))
    (new-board (append (cdr (cdr CARDS-0))
                       (list (car CARDS-0)
                             (car (cdr CARDS-0)))))))
  (should
   (equal
    (setup-board
     (seq-reduce #'play [:FLIP :FLIP :FLIP :NAY :FLIP :NAY :RESET] GAME-0))
    BOARD-0))
  ;; when giving wrong answer for every card,
  ;; then one ends up with the original deck again.
  (should (equal (setup-board (make-list (length CARDS-0) :NAY))
                 BOARD-0))
  (should
   (equal
    (setup-board (seq-reduce #'play [:FLIP :FLIP :FLIP :NAY :FLIP :NAY] GAME-0))
    (new-board (append (cddr CARDS-0)
                       (list (car CARDS-0)
                             (cadr CARDS-0)))))))

;; Game -> Board
(defun setup-board (game-full)
  "Setup the board using given GAME-FULL."
  (let ((game [])
        (acc BOARD-0)
        (ctr 0)
        (game-full-len (seq-length game-full)))
    (while (< ctr game-full-len)
      (setq game (seq-subseq game-full 0 (1+ ctr)))
      (setq acc (finish-setup game
                              acc
                              (seq-elt game ctr)))
      (setq ctr (1+ ctr)))
    acc))

;; Game Board Move -> Board
(defun finish-setup (game bad fm)
  "Using GAME and Board Almost Done ( BAD )
perform final move ( FM )."
  (let* ((cards (board-cards bad))
         (rest-of-cards (cdr cards))
         (first-card (car cards))
         (first-card-ans (card-answer first-card))
         (first-card-as-one-ele-list (list first-card))
         (cards-with-first-put-in-back
          (append rest-of-cards first-card-as-one-ele-list))
         (is-ans-up (board-is-answer-up bad))
         (is-ans-up-not (not is-ans-up))
         (nay-board (new-board cards-with-first-put-in-back))
         (yea-board (new-board rest-of-cards)))
    (pcase fm
      (:FLIP (new-board cards is-ans-up-not))
      (:NAY nay-board)
      (:YEA yea-board)
      (:QUIT (new-board cards is-ans-up t))
      (:RESET BOARD-0)
      (:SHUFFLE (new-board (shuffle-game-memoized (cons cards game))))
      ((pred stringp)
       (if (equal first-card-ans fm)
           yea-board
         nay-board))
      (_ (error "Illegal state")))))

;; (X -> Y) -> (X -> Y)
(defun memoize (fn)
  "Return memoized version of function ( FN )."
  (let ((memo (make-hash-table :test #'equal)))
    (lambda (arg)
      (let ((prev-result (gethash arg memo)))
        (or prev-result
            (let ((result (funcall fn arg)))
              (puthash arg
                       result
                       memo)
              result))))))

;; Pair<List<Card>,Game> -> List<Card>
(defalias 'shuffle-game-memoized (memoize #'shuffle-game))

;;;; View:

(defconst VIEW-WIN-MESSAGE "You Win. Congratulations."
  "Display this message in case the user wins the game.")
(defconst VIEW-QUIT-MESSAGE "Thank you for playing. Goodbye."
  "Display this message in case the user quits the game.")
(defconst VIEW-REMAINING "Remaining"
  "Print number of remaining cards after this.")
(defconst VIEW-MASTERED "Mastered"
  "Print number of mastered cards after this.")

;; Board -> String
(defun render-board (board)
  "Render given BOARD."
  (let* ((cards (board-cards board))
         (is-quit-board (board-is-quit board))
         (cards-len (seq-length cards))
         (cards-0-len (seq-length CARDS-0))
         (mastered-len (- cards-0-len cards-len)))
    (format "\n\n\n          %s\n\n\n"
            (if is-quit-board
                VIEW-QUIT-MESSAGE
              (if cards
                  (format "%s\n\n\n%s: %d\n%s: %d"
                          (visible-text-from-top-card board)
                          VIEW-REMAINING
                          cards-len
                          VIEW-MASTERED
                          mastered-len)
                VIEW-WIN-MESSAGE)))))

;; Board -> String
(defun visible-text-from-top-card (board)
  "Return visible text for the top most card in current BOARD."
  (let* ((cards (board-cards board))
         (first-card (car cards))
         (first-card-answer (card-answer first-card))
         (first-card-question (card-question first-card))
         (is-ans-up (board-is-answer-up board)))
    (if is-ans-up
        first-card-answer
      first-card-question)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PURITY BORDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Random:

;; Pair<List<Card>,Game> -> List<Card>
(defun shuffle-game (cards-game-pair)
  "Shuffle the cards of CARDS-GAME-PAIR."
  (shuffle (car cards-game-pair)))

(ert-deftest shuffle-test ()
  (should (equal (shuffle '())
                 '()))
  (should (equal (shuffle [])
                 '()))
  (should (equal (shuffle "")
                 '()))
  (should (equal (shuffle '(ARBITRARY-INTEGER-FOR-TESTING))
                 '(ARBITRARY-INTEGER-FOR-TESTING)))
  (should (equal (shuffle [ARBITRARY-INTEGER-FOR-TESTING])
                 '(ARBITRARY-INTEGER-FOR-TESTING)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO factor out "1"
  (should (equal (shuffle "1")
                 '(?1)))
  ;; load test
  (should
   (equal
    (shuffle (make-list LOAD-TEST-INPUT-SIZE
                        ARBITRARY-INTEGER-FOR-TESTING))
    (make-list LOAD-TEST-INPUT-SIZE
               ARBITRARY-INTEGER-FOR-TESTING)))
  (should
   (equal
    (shuffle (make-vector LOAD-TEST-INPUT-SIZE ARBITRARY-INTEGER-FOR-TESTING))
    (make-list LOAD-TEST-INPUT-SIZE ARBITRARY-INTEGER-FOR-TESTING))))
;; Sequence<X> -> List<X>
(defun shuffle (seq0)
  "Randomly shuffle Sequence ( SEQ0 )."
  (let ((seq seq0)
        (acc '()))
    (while (not (seq-empty-p seq))
      (let* ((len (seq-length seq))
             (random-index (random len))
             (random-ele (seq-elt seq random-index))
             (seq-before-random-index (seq-subseq seq 0 random-index))
             (seq-after-random-index (seq-subseq seq (1+ random-index) len))
             (remaining-seq (seq-concatenate 'list
                                             seq-before-random-index
                                             seq-after-random-index)))
        (setq seq remaining-seq)
        (setq acc (cons random-ele acc))))
    acc))

;;;; Interaction:

(defconst BOARD-GAME-MODE-BUFFER-NAME "*board game*"
  "The name of the board game mode buffer.")
(defconst TEXT-SCALE-MODE-INCREASE-SETTING 3
  "Increase the default text size by this much, when starting to play.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO put interaction text constants up here

(defvar *current-game*
  "The current state of the board game.")

(defmacro define-interactive-command (command-symbol doc move)
  "Return `defun' for an interactive command using:
COMMAND-SYMBOL
DOC
MOVE"
  `(defun ,command-symbol ()
     ,doc
     (interactive)
     (setq *current-game*
           (play *current-game* ,move))))

(defmacro key-for-map (key command-symbol)
  "Return `define-key' for `board-game-mode-map' using:
KEY
COMMAND-SYMBOL"
  `(define-key map ,key ',command-symbol))

(defmacro menu-item (menu-name command-symbol help-string predicate-symbol)
  "Return vector menu item for `easy-menu-define' using:
MENU-NAME
COMMAND-SYMBOL
HELP-STRING
PREDICATE-SYMBOL"
  `[,menu-name
    ,command-symbol
    :help ,help-string
    :active (,predicate-symbol *current-game*)])

(defmacro interaction-boilerplate (&rest ms)
  "Return interaction boilerplate code from move elements ( MS )."
  (let* ((trio-struct-list
          (seq-map
           (lambda (m) ; move
             (let* ((move-name (symbol-name m))
                    (move-nice-name (downcase (seq-subseq move-name 1)))
                    (doc (format "Interactive command for playing a %s move."
                                 move-nice-name))
                    (menu-name (capitalize move-nice-name))
                    (menu-help-string
                     (pcase m
                       (:FLIP "Flip top most card.")
                       (:YEA "Remove mastered card from deck.")
                       (:NAY "Put card in back of deck for further study.")
                       (:QUIT "Quit playing the game.")
                       (:RESET "Reset game to initial state.")
                       (:SHUFFLE "Shuffle cards.")
                       (_ (error "Illegal state"))))
                    (key (seq-subseq move-nice-name 0 1))
                    (predicate-name (format "%s-legal-p" move-nice-name))
                    (predicate-symbol (intern predicate-name))
                    (command-name (format "%s-command" move-nice-name))
                    (command-symbol (intern command-name)))
               `((define-interactive-command ,command-symbol ,doc ,m)
                 (key-for-map ,key ,command-symbol)
                 (menu-item ,menu-name
                            ,command-symbol
                            ,menu-help-string
                            ,predicate-symbol))))
           ms))
         (define-interactive-command-list (seq-map #'car trio-struct-list))
         (key-for-map-list (seq-map #'cadr trio-struct-list))
         (menu-item-list (seq-map (lambda (trio-struct)
                                    (car (cddr trio-struct)))
                                  trio-struct-list)))
    `(progn
       ,@define-interactive-command-list
       (defconst board-game-mode-map
         (let ((map (make-sparse-keymap)))
           ,@key-for-map-list
           map)
         "Keymap for board game mode.")
       (easy-menu-define board-game-mode-menu board-game-mode-map
         "Menu for board-game-mode-map."
         (list
          "Board Game" ; menu title
          ,@menu-item-list)))))
;; GUESS MOVE ( String case ) IS DONE SEPARATELY AND MANUALLY BELOW !!!
(interaction-boilerplate :FLIP :YEA :NAY :QUIT :RESET :SHUFFLE)

(defun guess-command (answer-guess)
  "Play guess move using ANSWER-GUESS."
  (interactive "MAnswer guess: ")
  (let* ((board (setup-board *current-game*))
         (cards (board-cards board))
         (first-card (car cards))
         (first-card-ans (card-answer first-card))
         (first-card-que (card-question first-card)))
    (when (not (equal first-card-ans answer-guess))
      (message (concat "Wrong answer to question: %s\n"
                       "Correct answer would have been: %s\n"
                       "Wrong answer was: %s")
               first-card-que
               first-card-ans
               answer-guess)))
  (setq *current-game*
        (play *current-game* answer-guess)))
(define-key board-game-mode-map "g" 'guess-command)
(define-key-after board-game-mode-menu [guess-command]
  '("Guess" "Guess the correct answer." . guess-command) 'guess-command)

(define-key board-game-mode-map [down-mouse-3] board-game-mode-menu)

(defmacro yes-or-no-p-checked-command (command-symbol yes-or-no-p-prompt-string)
  "Secure COMMAND-SYMBOL with YES-OR-NO-P-PROMPT-STRING."
  `(advice-add ',command-symbol :around
               (lambda (command)
                 (when (yes-or-no-p ,yes-or-no-p-prompt-string)
                   (funcall command)))))
(yes-or-no-p-checked-command quit-command "Really wanna try to quit? ")
(yes-or-no-p-checked-command reset-command "Really wanna try to reset? ")

(define-derived-mode board-game-mode nil "board-game"
  "A mode for playing board games in emacs.")

(defun print-board ()
  "Print current game state to board game buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (princ (render-board (setup-board *current-game*))
         (get-buffer BOARD-GAME-MODE-BUFFER-NAME))
  (setq buffer-read-only t))

(defun start-board-game ()
  "Start board game."
  (interactive)
  (setq *current-game* GAME-0)
  (switch-to-buffer BOARD-GAME-MODE-BUFFER-NAME)
  (board-game-mode)
  (text-scale-increase TEXT-SCALE-MODE-INCREASE-SETTING)
  (add-hook 'post-command-hook
            #'print-board
            t t))

(provide 'board-game)
;;; board-game.el ends here
