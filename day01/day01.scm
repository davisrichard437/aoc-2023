(use-modules (ice-9 textual-ports)
             (ice-9 string-fun))

;; substring replacement
(define digits '(("one" "o1e")
                 ("two" "t2o")
                 ("three" "t3e")
                 ("four" "f4r")
                 ("five" "f5e")
                 ("six" "s6x")
                 ("seven" "s7n")
                 ("eight" "e8t")
                 ("nine" "n9e")))

(define (char->number char)
  (string->number (string char)))

(define (char-is-number? char)
  (number? (char->number char)))

;; replace instances of spelled-out digits
(define* (get-sub str #:optional (idx 0))
  (let* ((matches
          (map (lambda (pair)
                 (cons (string-match
                        (string-append "^" (car pair))
                        str idx) (cadr pair)))
               digits))
         (subs
          (filter (lambda (x)
                    (regexp-match? (car x)))
                  matches))
         (sub (if (null? subs)
                  (cons #f #f)
                  (car subs))))
    sub))

(define (apply-sub sub str)
  (let ((mat (car sub)))
    (string-replace str
                    (cdr sub)
                    (match:start mat)
                    (match:end mat))))

(define (preprocess-str str)
  (let process ((idx 0)
                (str str))
    (let* ((sub (get-sub str idx))
           (mat (car sub)))
      (if (>= idx (string-length str))
          str ; base case
          (if mat
              ;; one after the digit
              (process (+ 1 (match:start mat))
                       (apply-sub sub str))
              (process (+ 1 idx) str))))))

(define (get-digits str)
  (filter char-is-number?
          (string->list (preprocess-str str))))

(define (num-first-and-last digits)
  (if (null? digits)
      0
      (string->number
       (list->string
        (list (car digits)
              (car (reverse digits)))))))

(define (single-calibration str)
  (display str)
  (display "\n")
  (display (num-first-and-last (get-digits str)))
  (display "\n")
  (num-first-and-last (get-digits str))
  )

(define (calibration str)
  (apply +
         (map single-calibration
              (string-split str #\newline))))

(define contents
  (call-with-input-file "input.txt" get-string-all))

(calibration contents)
