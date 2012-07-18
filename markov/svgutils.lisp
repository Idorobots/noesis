;; A bunch of macros to ease SVG outputting for the webapp.

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
        ,@body))

(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
                ,yes)
            ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
          (f lst nil)))

(defun print-tag (name alst closingp)
  (princ #\<)
  (if closingp
      (princ #\/))
  (princ (if (symbolp name)
             (string-downcase name)
             name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (if (symbolp (car att))
                                     (string-downcase (car att))
                                     (car att)) (cdr att)))
        alst)
  (princ #\>)
  (princ #\Newline))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro head (&body body)
  `(tag head ()
     ,@body))

(defmacro meta (attrs)
  `(tag meta ,attrs))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

(defmacro svg (w h &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
             "xmlns:xlink" "http://www.w3.org/1999/xlink"
             height ,h
             width ,w)
            ,@body))

(defun svg-motion (translation rotation)
  (format nil "~{translate(~a, ~a) rotate(~a, ~a, ~a)~}"
              (append translation rotation)))

(defmacro g (translation rotation &body body)
  `(tag g (transform ,(format nil (svg-motion translation rotation)))
     ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

(defun svg-rgb (col)
  (format nil "~{rgb(~a, ~a, ~a)~}" col))


(defun svg-style (color)
  (format nil "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a);~}"
              ;(append color (brightness color -100))
              (append color '(0 0 0))))

(defun circle (center radius color)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))))

(defun animate (type-name start-end from-to)
  (tag animate ("attributeName" (cdr type-name)
                "attributeType" (car type-name)
                begin (format nil "~as" (car start-end))
                dur (format nil "~as" (abs (- (cdr start-end)
                                              (car start-end))))
                fill "freeze"
                from (car from-to)
                to (cdr from-to))))

(defun animated-circle (center radius color path)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))
     (mapc (lambda (node)
             (let ((ts (car node))
                   (te (cadr node))
                   (xs (car (caddr node)))
                   (ys (cdr (caddr node)))
                   (xe (car (cadddr node)))
                   (ye (cdr (cadddr node))))
               (animate '(XML . "cx")
                        (cons ts te)
                        (cons xs xe))
               (animate '(XML . "cy")
                        (cons ts te)
                        (cons ys ye))))
           path)))

(defun polygon (points color)
  (tag polygon (points (format nil "~{~a,~a ~}"
                                   (mapcan (lambda (tp)
                                             (list (car tp) (cdr tp)))
                                           points))
                style (svg-style color))))

(defun square (top-left size color)
  (tag rect (x (car top-left)
             y (cdr top-left)
             width size
             height size
             style (svg-style color))))

(defun text (pos font size txt)
  (tag text (id "TextElement"
             x (car pos)
             y (cdr pos)
             font-family font
             font-size size
             visibility "visible")
    (princ txt)))

(defun random-walk (value length)
  (unless (zerop length)
          (cons value
                (random-walk (if (zerop (random 2))
                                 (1- value)
                                 (1+ value))
                             (1- length)))))

(defun svg-to-file (outputfile fun)
  (with-open-file (*standard-output* outputfile
                   :direction :output
                   :if-exists :supersede)
                   (funcall fun)))

(defun draw-walk (outputfile)
  (svg-to-file outputfile
               (lambda ()
                  (svg 400 200
                       (loop repeat 10
                             do (polygon (append '((0 . 200))
                                                 (loop for y in (random-walk 100 400)
                                                       for x from 1 to 400
                                                       collect (cons x y))
                                                 '((400 . 200)))
                                         (loop repeat 3
                                                collect (random 256))))))))