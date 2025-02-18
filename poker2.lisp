(defpackage :bet-tracker
  (:use :cl :hunchentoot)
  (:export :start-server :stop-server))

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
    (format s "<div class='balance-grid'>")
    (maphash (lambda (key value)
               (format s "<div class='balance-card' onclick=\"selectPlayer('~A')\"><strong>~A</strong><br>~A</div>" 
                       (hunchentoot:escape-for-html key) (hunchentoot:escape-for-html key) value))
             *players*)
    (format s "</div>")))

(defparameter *server* (make-instance 'easy-acceptor :port 4242))

(define-easy-handler (css-handler :uri "/style.css") ()
  (setf (content-type*) "text/css")
  "body { font-family: Arial, sans-serif; background-color: #f4f4f4; padding: 20px; text-align: center; }
   .button-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px; margin-bottom: 20px; }
   .button-grid button { padding: 15px; font-size: 18px; cursor: pointer; border: none; background-color: #007bff; color: white; border-radius: 5px; }
   .balance-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(120px, 1fr)); gap: 10px; }
   .balance-card { background: white; padding: 10px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0,0,0,0.1); cursor: pointer; }
   .selected-player { background-color: red !important; }")

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
               "<h2>Balances</h2>" (show-balances)
               "<div class='button-grid'>"
               "<button onclick=\"location.href='/add?name=' + prompt('Enter player name:')\">Add Player</button>"
               "<button onclick=\"location.href='/remove?name=' + selectedPlayer\">Remove Player</button>"
               "<button onclick=\"location.href='/bet?name=' + selectedPlayer + '&amount=10'\">Bet 10</button>"
               "<button onclick=\"location.href='/bet?name=' + selectedPlayer + '&amount=50'\">Bet 50</button>"
               "<button onclick=\"location.href='/bet?name=' + selectedPlayer + '&amount=100'\">Bet 100</button>"
               "<button onclick=\"location.href='/resolve?name=' + selectedPlayer + '&amount=10&win=true'\">Win</button>"
               "<button onclick=\"location.href='/resolve?name=' + selectedPlayer + '&amount=10&win=false'\">Lose</button>"
               "</div>"
               "<script>
               function selectPlayer(name) {
                   selectedPlayer = name;
                   // Remove the 'selected-player' class from all balance cards
                   document.querySelectorAll('.balance-card').forEach(card => {
                       card.classList.remove('selected-player');
                   });
                   // Add the 'selected-player' class to the selected player's card
                   let selectedCard = document.querySelector(`[onclick=\"selectPlayer('${name}')\"]`);
                   if (selectedCard) {
                       selectedCard.classList.add('selected-player');
                   }
    
               }
               </script>"
               "</body></html>"))

(defun start-server ()
  (start *server*))
