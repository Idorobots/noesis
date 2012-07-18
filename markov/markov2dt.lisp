;; Lokalizacja Markov'a
;; SKN Noesis 21.05.2012

(defparameter *world-map* '((red   green green)
                            (green green green)
                            (green green green)))
(defparameter *measurement-prob* 0.7)
(defparameter *movement-success-prob* 0.8)
(defparameter *movement-fail-prob* 0.1)

(defun prior-prob ()
  "Funkcja zwraca początkowy rozkłąd prawdopodobieństwa dla świata dwuwymiarowego."
;;
)

(defun sense (curr-prob measurement)
  "Funkcja wykonuje pomiar i odpowiednio modyfikuje rozkład prawdopodobieństwa."
;;
)

(defun nmth (n m lst)
  "Funkcja pomocnicza zwracająca element 'dwuwymiarowej listy'."
  (nth n (nth m lst)))

(defun move (curr-prob movement)
  "Funkcja wykonująca ruch zgodnie z modelem ruchu."
;;
)

(defparameter *measurements* '(red green green))
(defparameter *movements* '((1 . 0) (0 . -1) (0 . -1))) ; (x . y)

(defun simulate (prior-prob measurements movements)
  (let ((prob prior-prob))
    (loop for measurement in measurements
          for movement in movements
          do (setf prob (move (sense prob measurement)
                              movement)))
    prob))

(print (simulate (prior-prob) *measurements* *movements*))

;; (setq *world-map* '((green red red red green green green green red red)
;;                     (green red green green green red green green red red)
;;                     (green green green red green green green green green green)
;;                     (green green green green green red green green green green)
;;                     (red red green green green green green green red green)
;;                     (green red green green green red green green red green)
;;                     (green green green red green red green green green green)
;;                     (green green green green green green green green red green)
;;                     (red red green green green green green red red green)
;;                     (green green green red green green green green green green)))
;; (setq *measurements* '(red red green))
;; (setq *movements* '((2 . 0) (1 . 0) (1 . 0)))

;; (print (simulate (prior-prob) *measurements* *movements*))
