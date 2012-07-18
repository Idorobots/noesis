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
  (make-array 3 :initial-contents (list x y theta)))

(defun random-particle ()
  "Funkcja tworzy nowa, losowa czasteczke."
  (make-particle (* (random 1.0) +world-size+)
                 (* (random 1.0) +world-size+)
                 (* (random 1.0) 2 PI)))

(defun move (particles turn-angle distance)
  "Funkcja przemieszcza wszystkie czasteczki delegujac obliczenia do `move-particle'."
  (loop with len = (length particles)
        with new-particles = (make-array len)

        for particle across particles
        for i from 0 below len do
            (setf (aref new-particles i)
                  (move-particle particle turn-angle distance))

            finally (return new-particles)))

(defun move-particle (particle turn-angle distance)
  "Funkcja przemieszcza pojedyncza czasteczko o zadany dystans i kat."
  (let* ((theta (mod (+ (aref particle 2)  ; theta' = theta + angle + noise
                         turn-angle
                         (random-gaussian :sigma *turn-noise*))
                     (* 2 PI)))
         (dist (+ distance
                  (random-gaussian :sigma *movement-noise*)))
         (x (+ (aref particle 0)           ; x' = x + cos(theta') * distance + noise
               (* (cos theta) dist)))
         (y (+ (aref particle 1)           ; y' = y + sin(theta') * distance + noise
               (* (sin theta) dist))))
    (make-particle (mod x +world-size+)
                   (mod y +world-size+)
                   theta)))

(defun distance (particle landmark)
  "Funkcja zwraca odleglosc od czasteczki do punktu orientacyjnego."
  (sqrt (+ (expt (- (aref particle 0) (car landmark)) 2)       ; #(x y theta) vs (x . y)
           (expt (- (aref particle 1) (cdr landmark)) 2))))

(defun sense (particle landmarks)
  "Funkcja zwraca liste odleglosci czasteczki od poszczegolnych punktow orientacyjnych."
  (mapcar (lambda (landmark)
            (+ (distance particle landmark)
               (random-gaussian :sigma *sense-noise*)))
          landmarks))

(defun weight (particle robot-measurement landmarks)
  "Funkcja oblicza wage danej czasteczki."
  (let ((particle-measurement (sense particle landmarks)))
    (apply #'*
           (mapcar (lambda (particle-m robot-m)
                     (gaussian particle-m
                               :mu robot-m
                               :sigma *sense-noise*))
                   particle-measurement
                   robot-measurement))))

(defun resample (particles measurement landmarks)
  "Funkcja resampluje czasteczki w zgodzie z pomiarem robota."
  (loop with N = (length particles)
        with new-particles = (make-array N)
        with weights = (map 'vector (lambda (p)
                                      (weight p measurement landmarks))
                                    particles)
        with wmax = (* 2.0 (reduce #'max weights))
        with beta = 0
        with index = (random N)

        for i from 0 below N do
            (setf beta (+ beta (random wmax)))

            (loop while (> beta (aref weights index)) do
                  (setf beta (- beta (aref weights index)))
                     (setf index (mod (1+ index) N)))

            (setf (aref new-particles i)
                  (aref particles index))

            finally (return new-particles)))

(defun simulate (robo landmarks movements &key (N 100))
  "Funkcja przeprowazda symulacje dla danej poczatkowej pozycji robota,
   punktow orientacyjnych oraz listy ruchow do wykonania."
  (loop with robot = (apply #'make-particle robo)
        with particles = (make-array N :initial-contents
                                       (loop repeat N collecting (random-particle)))

       for (angle . distance) in movements do
           (setf robot (move-particle robot angle distance))
           (setf particles (resample (move particles angle distance)
                                     (sense robot landmarks)
                                     landmarks))

           finally (return (list particles robot))))

;; Przyklady dzialania:

; (Przewaznie) bledne przyblizenie:
;; (simulate *robot* *landmarks* '((0.0 . 5.0) (0.0 . 5.0) (0.0 . 5.0)) :N 10)

;; ; (Przewaznie) skuteczne przyblizenie:
;; (simulate *robot*
;;           *landmarks*
;;           '((0.0 . 5.0)
;;             (0.0 . 5.0)
;;             (0.0 . 5.0)
;;             (1.57 . 10.0)
;;             (0.0 . 10.0))
;;           :N 1000)

;; ; Skuteczne przyblizenie:
;; (simulate *robot*
;;           *landmarks*
;;           '((0.0 . 15.0)
;;             (0.0 . 15.0)
;;             (0.0 . 15.0)
;;             (1.57 . 30.0)
;;             (0.0 . 15.0)
;;             (0.0 . 25.0)
;;             (1.57 . 20.0)
;;             (0.0 . 15.0)
;;             (3.14 . 25.0)
;;             (1.57 . 20.0))
;;           :N 2500)
