;; Lokalizacja Markov'a
;; SKN Noesis 21.05.2012

(defparameter *world-map* '(green red green green))
(defparameter *measurement-prob* 0.7)
(defparameter *movement-success-prob* 0.8)
(defparameter *movement-fail-prob* 0.1)

(defun prior-prob ()
  "Funkcja zwraca początkowy rozkład prawdopodobieństwa dla podanego świata."
;;
)

(defun sense (prob measurement)
  "Funkcja zwraca rozkład prawdopodobieństwa po wykonaniu pomiaru `measurement'."
;;
)

(defun move (prob movement)
  "Funkcja zwraca rozkład prawdopodobieństwa po wykonaniu ruch `movement'."
;;
)

(defparameter *measurements* '(red green green))
(defparameter *movements* '(1 1 1))

(defun simulate (prior-prob measurements movements)
  "Funkcja zwraca rozkład prawdopodobieństwa po wykonaniu całej symulacji."
;;
)

(print (simulate (prior-prob) *measurements* *movements*))


; (setq *world-map* '(green green green red green
;                     red green green green green
;                     green red green green green))
; (setq *measurements* '(red red green))
; (setq *movements* '(2 1 1))

; (print (simulate (prior-prob) *measurements* *movements*))