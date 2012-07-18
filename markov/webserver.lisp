;; Slightly modified version of the webserver from Land of Lisp, works with SBCL.

(require :sb-bsd-sockets)
(use-package :sb-bsd-sockets)

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer (coerce (list c1 c2) 'string)
                             :radix 16
                             :junk-allowed t)))
       (if code
           (code-char code)
           default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
                   (case (car lst)
                         (#\% (cons (http-char (cadr lst) (caddr lst))
                                    (f (cdddr lst))))
                         (#\+ (cons #\space (f (cdr lst))))
                         (otherwise (cons (car lst) (f (cdr lst))))))))
          (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
        (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                              (decode-param (subseq s (1+ i1) i2)))
                        (and i2 (parse-params (subseq s (1+ i2))))))
              ((equal s "") nil)
              (t s))))

(defun parse-url (s)
 (let* ((url (subseq s
                     (+ 2 (position #\space s))
                     (position #\space s :from-end t)))
        (x (position #\? url)))
       (if x
           (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
           (cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
                 (when i
                       (cons (intern (string-upcase (subseq s 0 i)))
                             (subseq s (+ i 2)))))))
        (when h
              (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
       (when length
             (let ((content (make-string (parse-integer length))))
                  (read-sequence content stream)
                  (parse-params content)))))

(defun socket-server (port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-bind socket #(127 0 0 1) port)
    (socket-listen socket 5)
    socket))

(defun socket-server-close (socket)
  (socket-close socket))

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
       (unwind-protect ;<3
         (loop (with-open-stream (stream (socket-make-stream (socket-accept socket)
                                                             :output t :input t))
                 (let* ((url (parse-url (read-line stream)))
                        (path (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                        ;(print path)
                        ;(print header)
                        ;(print params)
                       (funcall request-handler path header params)
                       (force-output stream))))
         (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'NAME params)))
        (if (not name)
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (progn (princ "Sorry... I don't know that page."))))