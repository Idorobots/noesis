;; Kajetan Rzepecki
;; SKN Noesis 21.05.2012

(load "markov2d.lisp")
(load "webserver.lisp")
(load "svgutils.lisp")

(defconstant +red+ '(204 65 37))
(defconstant +green+ '(147 196 125))
(defconstant +yellow+ '(255 217 102))
(defconstant +ghostly+ '(200 200 200))
(defconstant +block-size+ 50)
(defconstant +robot-radius+ 10)

(defun draw-block (x y size type &optional value)
  (let ((col (if (eql type 'green) +green+ +red+)))
    (tag g ()
      (tag rect (x (* x size)
                 y (* y size)
                 width size
                 height size
                 style (svg-style col))
        (tag set ("attributeName" "fill"
                   from (svg-rgb col)
                   to (svg-rgb (brightness col 25))
                   begin "mouseover"
                   end "mouseout")))
      (when value
        (text (cons (+ (* x size) 3)
                    (+ (* y size) (- size 2)))
              "Verdana"
              16
              (format nil "~,4f" value))))))

(defun distance (xa ya xb yb)
  (let ((dx (- xb xa))
        (dy (- yb ya)))
    (sqrt (+ (* dx dx)
             (* dy dy)))))

(defun real-pos (x y size)
  (cons (+ (* x size) (/ size 2.0))
        (+ (* y size) (/ size 2.0))))

(defun draw-robot (xs ys xd yd size &optional color)
  (let ((duration (+ (distance xs ys xd yd)
                     (if color 0.5 0))))
    (animated-circle (real-pos xs ys size)
                     +robot-radius+
                     (if color
                         color
                         +yellow+)
                     `((0 ,duration ,(real-pos xs ys size)
                                         ,(real-pos xd yd size))))))

(defun draw-button (color)
  (tag a (href (format nil "./markov?sense=~a" color))
    (format t "<b style=\"padding: 0px 25px 0px 25px; margin: 0px 25px 0px 25px\">~a</b>" color)))

(defun make-link (x y)
  (format nil "./markov?x=~a&y=~a" x y))

(defun nmth (n m lst)
  (nth n (nth m lst)))

(defun find-best (curr-prob)
  (let ((leny (length curr-prob))
        (lenx (length (car curr-prob)))
        (best (list (caar curr-prob) 0 0)))
    (loop for y from 0 below leny
          for probs in curr-prob
          do (loop for x from 0 below lenx
                   for prob in probs
                   do (when (> prob (car best))
                            (setf best (list prob x y)))))
    (cdr best)))

(defun draw-map (world-map curr-prob robot-pos movement)
  (let* ((leny (length world-map))
         (lenx (length (car world-map)))
         (total-sizex (* lenx +block-size+))
         (total-sizey (* leny +block-size+))
         (robotx (car robot-pos))
         (roboty (cdr robot-pos))
         (movex (car movement))
         (movey (cdr movement)))
    (svg total-sizex total-sizey
      (loop for y from 0 below leny
            for map in world-map
            for prob in curr-prob
            do (loop for x from 0 below lenx
                     for type in map
                     for val in prob
                     do (tag a ("xlink:href" (make-link x y))
                          (draw-block x y +block-size+ type val))))
      (let* ((best (find-best curr-prob))
             (bestx (car best))
             (besty (cadr best)))
        (draw-robot (- bestx movex) (- besty movey)
                    bestx besty
                    +block-size+
                    +ghostly+))
      (draw-robot (- robotx movex) (- roboty movey)
                  robotx roboty
                  +block-size+))))

(defun draw-menu ()
  (tag p ()
    (draw-button 'green)
    (draw-button 'red))
  (tag form (id "optionsForm"
             method "get"
             style "display: none;")
    (tag textarea (name "code"
                   cols "80"
                   rows "10"
                   style "white-space: pre;")
      (format t "(~%~{~(~A~)~%~})" *world-map*))
    (tag br ())
    (tag input (type "submit"
                value "reset")))
   (tag input (type "reset"
               value "options"
               onclick "javascript:toggleLayer('optionsForm');")))

(defun draw (world-map curr-prob robot-pos movement)
  (tag html (xmlns "http://www.w3.org/1999/xhtml"
             "xml:lang" "pl"
             lang "pl")
    (tag head ()
      (tag meta (http-equiv "Content-Type"
                 content "text/html; charset=utf-8"))
      (tag title ()
        (princ "Markov localization")))
    (tag body (style "text-align: center;")
      (tag script (language "javascript")
      (princ "function toggleLayer( whichLayer ) {
                  var elem, vis;
                  if( document.getElementById ) // this is the way the standards work
                      elem = document.getElementById( whichLayer );
                  else if( document.all ) // this is the way old msie versions work
                      elem = document.all[whichLayer];
                  else if( document.layers ) // this is the way nn4 works
                      elem = document.layers[whichLayer];
                  vis = elem.style;
                  // if the style.display value is blank we try to figure it out here
                  if(vis.display==''&&elem.offsetWidth!=undefined&&elem.offsetHeight!=undefined)
                      vis.display = (elem.offsetWidth!=0&&elem.offsetHeight!=0)?'block':'none';
                  vis.display = (vis.display==''||vis.display=='block')?'none':'block';
              }"))
      (draw-map world-map curr-prob robot-pos movement)
      (draw-menu))))

; Initial parametres:
(defparameter *curr-prob* nil)
(defparameter *curr-robot-pos* '(0 . 0))

(defun initialize (&optional map)
  (setf *world-map* (if map
                        map
                        '((green red green green red)
                          (green green green red red)
                          (red red green red green)
                          (green green red green green))))
  (setf *measurement-prob* 0.7)
  (setf *movement-prob* 0.8)
  (setf *curr-prob* (prior-prob))
  (setf (car *curr-robot-pos*) 0)
  (setf (cdr *curr-robot-pos*) 0)
  (setf *movement-success-prob* 0.8)
  (setf *movement-fail-prob* 0.1)
  (setf *measurement-prob* 0.7))

(defun visualize (path header params)
  (if (equal path "markov")
      (let ((sensed (cdr (assoc 'sense params)))
            (x (cdr (assoc 'x params)))
            (y (cdr (assoc 'y params)))
            (code (cdr (assoc 'code params))))
        (cond ((and code (not (equal code "")))
                (initialize (read-from-string code))
                (draw *world-map* *curr-prob* *curr-robot-pos* '(0 . 0)))
              (sensed
                (setf *curr-prob* (sense *curr-prob* (intern sensed)))

                (draw *world-map* *curr-prob* *curr-robot-pos* '(0 . 0)))

              ((and x y)
                ;(print (format nil "x: ~a, y: ~a" x y) *error-output*)

                (let* ((robotx (car *curr-robot-pos*))
                       (roboty (cdr *curr-robot-pos*))
                       (dx (- (parse-integer x) robotx))
                       (dy (- (parse-integer y) roboty)))

                  (setf *curr-prob* (move *curr-prob* (cons dx dy)))
                  (setf (car *curr-robot-pos*) (+ (car *curr-robot-pos* ) dx))
                  (setf (cdr *curr-robot-pos*) (+ (cdr *curr-robot-pos*) dy))

                  (draw *world-map* *curr-prob* *curr-robot-pos* (cons dx dy))))

              (t
                (initialize)
                (draw *world-map* *curr-prob* *curr-robot-pos* '(0 . 0)))))
      (format t "No can do, buddy!")))

; Run the simulation:
(defun run-markov ()
  (serve #'visualize))