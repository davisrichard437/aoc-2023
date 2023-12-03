(use-modules (ice-9 textual-ports)
             (ice-9 string-fun)
             (srfi srfi-9))

;;; Part 1

(define-record-type game
  (make-game id draws)
  game?
  (id game-id)
  (draws game-draws))

(define-record-type draw
  (make-draw red green blue)
  draw?
  (red draw-red)
  (green draw-green)
  (blue draw-blue))

(define (string->draw str)
  "Construct a draw record according to the data in str."
  (let* ((number-capture "([0-9]+)")
         (make-color-capture
          (lambda (color)
            (format #f "~a ~a" number-capture color)))
         (colors '("red" "green" "blue"))
         (captures (map make-color-capture colors))
         (matches (map (lambda (rx)
                         (string-match rx str))
                       captures))
         (numbers (map (lambda (m)
                         (if m
                             (string->number
                              (regexp-substitute #f m 1))
                             0))
                       matches)))
    (apply make-draw numbers)))

(define (string->game str)
  "Construct a game record according to the data in str."
  (let* ((rx "^Game ([0-9]+): ")
         (mat (string-match rx str))
         (id (string->number
              (regexp-substitute #f mat 1)))
         (draw-str (match:suffix mat))
         (draw-strs (string-split draw-str #\;)))
    (make-game id (map string->draw draw-strs))))

(define* (draw-possible? d #:optional
                         (mr 12)
                         (mg 13)
                         (mb 14))
  "Check that draw does not exceed maximum blocks."
  (and (<= (draw-red d) mr)
       (<= (draw-green d) mg)
       (<= (draw-blue d) mb)))

(define* (game-possible? g #:optional
                         (mr 12)
                         (mg 13)
                         (mb 14))
  "Check that game does not exceed maximum blocks."
  (let* ((draws (game-draws g))
         (draws-possible (map draw-possible? draws)))
    (eval (cons 'and draws-possible)
          (interaction-environment))))

(define (sum-possible-games games)
  "Take a list of strings representing games and return a list of game objects
corresponding to those that are possible."
  (let* ((possible (filter game-possible? games))
         (possible-ids (map game-id possible)))
    (apply + possible-ids)))

;;; Part 2

(define-record-type cubes
  (make-cubes red green blue)
  cubes?
  (red cubes-red)
  (green cubes-green)
  (blue cubes-blue))

(define (min-cubes g)
  "Get the minimum number of cubes of each color required by a given game."
  (let* ((draws (game-draws g))
         (red (map draw-red draws))
         (green (map draw-green draws))
         (blue (map draw-blue draws)))
    (make-cubes (apply max red)
                (apply max green)
                (apply max blue))))

(define (power c)
  "Return the power of a cubes object."
  (* (cubes-red c)
     (cubes-green c)
     (cubes-blue c)))

(define (sum-games-power games)
  (let* ((mins (map min-cubes games))
         (powers (map power mins)))
    (apply + powers)))

;;; BODY
;; for some reason the repl starts at the root of the repository
(define input-file "./day02/input2.txt")

(define input
  (reverse-list->string
   (cdr ; remove trailing newline
    (reverse
     (string->list
      (call-with-input-file input-file
        get-string-all))))))

(define game-strs
  (string-split input #\newline))

(define games
  (map string->game game-strs))

(sum-games-power games)
