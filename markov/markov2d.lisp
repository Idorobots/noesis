;; Kajetan Rzepecki
;; SKN Noesis 21.05.2012

(defparameter *world-map* '((red   green green)
                            (green green green)
                            (green green green)))
(defparameter *measurement-prob* 0.7)
(defparameter *movement-success-prob* 0.8)
(defparameter *movement-fail-prob* 0.1)

(defun prior-prob ()
  (let* ((lenx (length (car *world-map*)))
         (leny (length *world-map*))
         (size (* lenx leny)))
    (loop for line from 0 below leny
          collect (make-list lenx :initial-element (/ 1.0 size)))))

(defun sense (curr-prob measurement)
  (let* ((new-prob (loop for world-line in *world-map*
                         for prob-line in curr-prob
                         collecting (loop for world-val in world-line
                                          for prob-val in prob-line
                                          collecting (if (eql measurement world-val)
                                                         (* prob-val *measurement-prob*)
                                                         (* prob-val (- 1 *measurement-prob*))))))
         (prob-sum (apply #'+
                          (mapcar (lambda (lne)
                                    (apply #'+ lne))
                                  new-prob))))
    (mapcar (lambda (line)
              (mapcar (lambda (x)
                        (/ x prob-sum))
                      line))
            new-prob)))

(defun nmth (n m lst)
  (nth n (nth m lst)))

(defun move (curr-prob movement)
  (let ((lenx (length (car *world-map*)))
        (leny (length *world-map*))
        (movex (car movement)) ; (x . y)
        (movey (cdr movement)))
    (loop for y from 0 below leny
          collecting (loop for x from 0 below lenx
                           collecting (+ (* *movement-success-prob*       ; Udany ruch
                                            (nmth (mod (- x movex) lenx)
                                                  (mod (- y movey) leny)
                                                  curr-prob))
                                         (* (- 1 *movement-success-prob*
                                                 *movement-fail-prob*)    ; Robot zostal w miejscu
                                            (nmth x y curr-prob))
                                         (* *movement-fail-prob*          ; Robot sie cofnal
                                            (nmth (mod (+ x movex) lenx)
                                                  (mod (+ y movey) leny)
                                                  curr-prob)))))))

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