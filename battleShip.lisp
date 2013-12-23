
(setq myShips '(((x . 0) (y . 0)) ((x . 0) (y . 0)) ((x . 0) (y . 0)) ((x . 0) (y . 0)) ((x . 0) (y . 0))))
(setq enemyShips '(((x . 0) (y . 0)) ((x . 0) (y . 0)) ((x . 0) (y . 0)) ((x . 0) (y . 0)) ((x . 0) (y . 0))))
(setq shipNumber 
  (with-open-file (in "setting.txt" :direction :input)
     (read in nil)))

;;models
(defun initEnemyShips()
  (enemyShipGenerator nil shipNumber))

(defun enemyShipGenerator(ships num)
  (if (= (length ships) num) ships
    (enemyShipGenerator (append ships (list (list (cons 'x (* (random 10) 30) ) (cons 'y (* (random 10) 30))))) num)))

(defun initMyShips()
  (myShipGenerator nil shipNumber))

(defun myShipGenerator(ships num)
  (if (= (length ships) num) ships
    (myShipGenerator (append ships (list (list (cons 'x (* (length ships) 30)) (cons 'y 120)))) num)))

(defun setMyShip(x y num targets others)
  (if (= num 1) (append (append others (list (list (cons 'x x) (cons 'y y)))) (cdr targets))
    (setMyShip x y (- num 1) (cdr targets) (append others (list (car targets))))))

(defun initModels()
  (setq enemyShips (initEnemyShips))
  (setq myShips (initMyShips))
  )

(initModels)


;;view and controllers
(load "ltk/ltk")
(use-package :ltk)

(defun createButton(num button-frame)
  (make-instance 'button
		 :master button-frame
		 :text (concatenate 'string "ship" (write-to-string num))
		 :command(lambda()
			   (setq shipSelected num)
			   (format t (write-to-string shipSelected)))))

(defun initShipButtons(num button-frame)
  (cond ((= num 0) T)
	((> num 0) 
	 (pack (createButton num button-frame) :side :right)
	 (initShipButtons (- num 1) button-frame))))

(defun initFieldGrid(myField enemyField)
  (drawRectangles myField 0 0 30 30 0 "#0066ff")
  (drawRectangles enemyField 0 0 30 30 0 "#003399")
  )

(defun drawRectangles(field x y w h index color)
  (cond ((= index 100) T)
	((< index 100)
	 (itemconfigure field (create-rectangle field x y (+ x w) (+ y h)) "fill" color)
	 (if (= (mod index 10) 9)
	     (drawRectangles field 0 (+ y h) w h (+ index 1) color)
	   (drawRectangles field (+ x w) y w h (+ index 1) color)
	   ))))

(defun getCurrentCoords (ships num)
  (if (= num 1) (car ships)
    (getCurrentCoords (cdr ships) (- num 1))))

(defun fillRect (field x y w h color)
  (itemconfigure field (create-rectangle field x y (+ x w) (+ y h)) "fill" color)
  )

;;当たり判定
(defun judge (x y ships)
  (judge-tr x y ships 2))

(defun judge-tr (x y ships tmp)
  (setf cx (cdr (assoc 'x (car ships))))
  (setf cy (cdr (assoc 'y (car ships))))
  (if (null ships) tmp
    (cond 
     ((and (= x cx) (= y cy)) 0)
     ((and (> 60 (abs (- x cx))) (> 60 (abs (- y cy)))) 
      (judge-tr x y (cdr ships) 1)
      )
     (T 
      (judge-tr x y (cdr ships) tmp)))))

;;塗り色変化
(defun setRectColor (hit)
  (cond ((= hit 0) "#ff6600")
	((= hit 1) "#ffff00")
	((= hit 2) "#333333")))

(defun enemyAttack ()
  (list (cons 'x (* (random 10) 30)) (cons 'y (* (random 10) 30)))
  )

(defun initView ()
  (setq shipSelected nil)
  (setq myHP shipNumber)
  (setq enemyHP shipNumber)
  
  (with-ltk ()
	    (let* ((button-frame (make-instance 'frame))
		   (myFrame (make-instance 'frame))
		   (myField (make-canvas myFrame
					 :width 300
					 :height 300
					 ))
		   (enemyFrame (make-instance 'frame))
		   (enemyField (make-canvas enemyFrame
					    :width 300
					    :height 300
					    ))
		   (down nil))
	      ;;downにmousedownのパラメータを入れる。
	      
	      (initShipButtons shipNumber button-frame)
	      (pack button-frame)
	      
	      (pack myFrame :side :left)
	      (pack myField :side :left)
	      (bind myField "<ButtonPress-1>"
		    (lambda (evt)
		      (setf down t)))
	      (bind myField "<ButtonRelease-1>" (lambda (evt) 
						  (setf currentCoords (getCurrentCoords myShips shipSelected))
						  (fillRect myField (cdr (assoc 'x currentCoords)) (cdr (assoc 'y currentCoords)) 30 30 "#0066ff")
						  (fillRect myField (* (floor (event-x evt) 30) 30) (* (floor (event-y evt) 30) 30) 30 30 "#0033cc")
						  (setf myShips (setMyShip (* (floor (event-x evt) 30) 30) (* (floor (event-y evt) 30) 30) shipSelected myShips nil))
						  (setf down nil)))

	      (pack enemyFrame :side :left)
	      (pack enemyField :side :left)
	      (bind enemyField "<ButtonRelease-1>" (lambda (evt) 
						     (setf hit (judge (* (floor (event-x evt) 30) 30) (* (floor (event-y evt) 30) 30) enemyShips));;0 1 2 => 0は当たり、1が近い、2が外 
						     (if (= hit 0) (setq enemyHP (- enemyHP 1)) T)
						     (if (= enemyHP 0) (endDirection myField enemyField) T)

						     (fillRect enemyField (* (floor (event-x evt) 30) 30) (* (floor (event-y evt) 30) 30) 30 30  (setRectColor hit))
						     
						     (setf attack (enemyAttack))
						     (setf hitted (judge (cdr (assoc 'x attack)) (cdr (assoc 'y attack)) myShips))
						     (if (= hitted 0) (setq myHP (- myHP 1)) T)
						     (if (= myHP 0) (endDirection enemyField myField) T)

						     (fillRect myField (cdr (assoc 'x attack)) (cdr (assoc 'y attack)) 30 30  (setRectColor hitted))
						     ;;ランダム。AI無理。
						     ))
	      (initFieldGrid myField enemyField)
	      )))

(defun endDirection (winner loser)
  (create-text winner 30 120 "YOU WIN")
  (create-text loser 30 120 "YOU LOSE")
  )

(initView)
