(defpackage :bet-tracker
  (:use :cl :hunchentoot)
  (:export :start-server
           :stop-server))

(in-package :bet-tracker)

(defvar *players* (make-hash-table :test 'equal))

(defun sanitize (input)
  (cl-ppcre:regex-replace-all "[<>]" input ""))

(defun add-player (name &optional (balance 100))
  (setf (gethash (sanitize name) *players*) balance))

(defun remove-player (name)
  (remhash (sanitize name) *players*))

(defun place-bet (name amount)
  (let ((balance (gethash (sanitize name) *players*)))
    (if (and balance (>= balance amount))
        (setf (gethash (sanitize name) *players*) (- balance amount)))))

(defun resolve-bet (name amount win)
  (let ((balance (gethash (sanitize name) *players*)))
    (if balance
        (setf (gethash (sanitize name) *players*) (if win (+ balance (* amount 2)) balance)))))

(defun show-balances ()
  (with-output-to-string (s)
    (maphash (lambda (key value)
               (format s "~A: ~A<br>" (hunchentoot:escape-for-html key) value))
             *players*)))

(defparameter *server* (make-instance 'easy-acceptor :port 4242))

(define-easy-handler (css-handler :uri "/style.css") ()
  (setf (content-type*) "text/css")
  "body { font-family: Arial, sans-serif; background-color: #f4f4f4; padding: 20px; }
   h1 { color: #333; }
   form { margin-bottom: 10px; }
   input, select { margin-right: 10px; }")

(define-easy-handler (add-handler :uri "/add") (name)
  (add-player name)
  (home-handler))

(define-easy-handler (remove-handler :uri "/remove") (name)
  (remove-player name)
  (home-handler))

(define-easy-handler (bet-handler :uri "/bet") (name amount)
  (place-bet name (parse-integer amount))
  (home-handler))

(define-easy-handler (resolve-handler :uri "/resolve") (name amount win)
  (resolve-bet name (parse-integer amount) (string= win "true"))
  (home-handler))

(define-easy-handler (home-handler :uri "/") ()
  (concatenate 'string
               "<html><head><link rel='stylesheet' type='text/css' href='/style.css'></head><body>"
               "<h1>Bet Tracker</h1>"
               "<form action='/add' method='get'>"
               "Add Player: <input name='name'><input type='submit' value='Add'>"
               "</form>"
               "<form action='/remove' method='get'>"
               "Remove Player: <input name='name'><input type='submit' value='Remove'>"
               "</form>"
               "<form action='/bet' method='get'>"
               "Place Bet: <input name='name'> Amount: <input name='amount'>"
               "<input type='submit' value='Bet'>"
               "</form>"
               "<form action='/resolve' method='get'>"
               "Resolve Bet: <input name='name'> Amount: <input name='amount'>"
               "Win? <select name='win'><option>true</option><option>false</option></select>"
               "<input type='submit' value='Resolve'>"
               "</form>"
               "<h2>Balances</h2>" (show-balances)
               "</body></html>"))

(defun start-server ()
  (start *server*))
