;; SKN Noesis
;; 04.06.2012

;; Parametry symulacji

; Rozmiar swiata
(defconstant +world-size+ 100.0)

; Punkty orientacyjne i poczatkowa pozycja robota
(defparameter *landmarks* '((20.0 . 20.0) (80.0 . 80.0) (20.0 . 80.0) (80.0 . 20.0)))
(defparameter *robot* '(50.0 50.0 0.0))

; Zakresy losowych zaklocen
(defparameter *movement-noise* 0.5)
(defparameter *turn-noise* 0.05)
(defparameter *sense-noise* 5.0)

;; Funkcje pomocnicze

(defun normal ()
  "Funkcja zwraca losowa wartosc o rozkladzie normalnym."
  (* (sqrt (* -2 (log (random 1.0))))
     (cos (* 2 pi (random 1.0)))))

(defun random-gaussian (&key (sigma 1.0) (mu 0.0))
  "Funkcja zwraca losowa wartosc o rozkladzie Gaussa o sredniej mu i wariancji sigma."
  (+ mu (* (sqrt sigma) (normal))))

(defun gaussian (x &key (sigma 1.0) (mu 0.0))
  "Funkcja zwraca gestosc prawdopodobienstwa dla sredniej mu i wariancji sigma zmiennej x."
  (let ((sigmasq (* sigma sigma)))
    (/ (exp (- (/ (expt (- x mu) 2)
                  (* 2 sigmasq))))
       (sqrt (* 2.0 PI sigmasq)))))

;; Funkcje filtru czasteczkowego

(defun make-particle (x y theta)
  "Funkcja tworzy nowa czasteczke."
)

(defun random-particle ()
  "Funkcja tworzy nowa, losowa czasteczke."
)

(defun move (particles turn-angle distance)
  "Funkcja przemieszcza wszystkie czasteczki delegujac obliczenia do `move-particle'."
)

(defun move-particle (particle turn-angle distance)
  "Funkcja przemieszcza pojedyncza czasteczke o zadany dystans i kat."
)

(defun distance (particle landmark)
  "Funkcja zwraca odleglosc od czasteczki do punktu orientacyjnego."
)

(defun sense (particle landmarks)
  "Funkcja zwraca liste odleglosci czasteczki od poszczegolnych punktow orientacyjnych."
)

(defun weight (particle robot-measurement landmarks)
  "Funkcja oblicza wage danej czasteczki."
)

(defun resample (particles measurement landmarks)
  "Funkcja resampluje czasteczki w zgodzie z pomiarem robota."
)

(defun simulate (robo landmarks movements &key (N 100))
  "Funkcja przeprowazda symulacje dla danej poczatkowej pozycji robota,
   punktow orientacyjnych oraz listy ruchow do wykonania."
  (loop with robot = (apply #'make-particle robo)
        with particles = (make-array N :initial-contents
                                       (loop repeat N
                                             collecting (random-particle)))

       for (angle . distance) in movements do
           (setf robot (move-particle robot angle distance))
           (setf particles (move (resample particles
                                           (sense robot landmarks)
                                           landmarks)
                                 angle
                                 distance))

           finally (return (list particles robot))))

;; Przyklady dzialania:

; (Przewaznie) bledne przyblizenie:
;(simulate *robot* *landmarks* '((0.0 . 5.0) (0.0 . 5.0) (0.0 . 5.0)) :N 10)

; (Przewaznie) skuteczne przyblizenie:
; (simulate *robot*
;           *landmarks*
;           '((0.0 . 5.0)
;             (0.0 . 5.0)
;             (0.0 . 5.0)
;             (1.57 . 10.0)
;             (0.0 . 10.0))
;           :N 1000)