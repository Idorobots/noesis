;; Kajetan Rzepecki
;; SKN Noesis 21.05.2012

(defparameter *world-map* '(green red green green))
(defparameter *measurement-prob* 0.7)
(defparameter *movement-success-prob* 0.8)
(defparameter *movement-fail-prob* 0.1)

(defun prior-prob ()
  (let ((len (length *world-map*)))
    (make-list len :initial-element (/ 1.0 len))))

(defun sense (curr-prob measurement)
  (let* ((new-prob (loop for world-val in *world-map*
                         for prob-val in curr-prob
                         collecting (if (eql measurement world-val)
                                        (* prob-val *measurement-prob*)
                                        (* prob-val (- 1 *measurement-prob*)))))
         (prob-sum (apply #'+ new-prob)))
    (mapcar (lambda (x) (/ x prob-sum)) new-prob)))

(defun move (curr-prob movement)
  (let ((len (length curr-prob)))
    (loop for index from 0 below len
          collecting (+ (* (- 1 *movement-success-prob*       ; Prawdopodobienstwo zostania w miejscu.
                                *movement-fail-prob*)
                           (nth index curr-prob))
                        (* *movement-success-prob*            ; Prawdopodobienstwo udanego ruchu.
                           (nth (mod (- index movement) len) curr-prob))
                        (* *movement-fail-prob*               ; Prawdopodobienstwo cofniecia sie.
                           (nth (mod (+ index movement) len) curr-prob))))))

(defparameter *measurements* '(red green green))
(defparameter *movements* '(1 1 1))

(defun simulate (prior-prob measurements movements)
  (let ((prob prior-prob))
    (loop for measurement in measurements
          for movement in movements
          do (setf prob (move (sense prob measurement)
                              movement)))
    prob))

(print (simulate (prior-prob) *measurements* *movements*))

;; (setq *world-map* '(green green green red green
;;                     red green green green green
;;                     green red green green green))
;; (setq *measurements* '(red red green))
;; (setq *movements* '(2 1 1))

;; (print (simulate (prior-prob) *measurements* *movements*))