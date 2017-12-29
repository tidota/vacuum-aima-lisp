;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Some simple agents for the vacuum world

(defstructure (random-vacuum-agent
   (:include agent
    (program
     #'(lambda (percept)
	 (declare (ignore percept))
	 (random-element
	  '(suck forward (turn right) (turn left) shut-off))))))
  "A very stupid agent: ignore percept and choose a random action.")

(defstructure (reactive-vacuum-agent
   (:include agent
    (program
     #'(lambda (percept)
	 (destructuring-bind (bump dirt home obs) percept
	   (cond (dirt 'suck)
		 (home (random-element '(shut-off forward (turn right))))
		 (bump (random-element '((turn right) (turn left))))
		 (t (random-element '(forward forward forward
					      (turn right) (turn left))))))))))
  "When you bump, turn randomly; otherwise mostly go forward, but
  occasionally turn.  Always suck when there is dirt.")

(defstructure (improved-vacuum-agent
     (:include agent
      (program
       #'(lambda (percept)
         (let ((retval nil))
        	 (destructuring-bind (bump dirt home obs completed) percept
             (cond
               ((> dirt 0) 'suck)
          		 ((and home completed) 'shut-off)
               (completed (random-element '(up right left left left down down down)))
               (bump (random-element '(up right left down)))
          		 (t (random-element (getdirs_free obs)))
             )
           )
         )
       ))))
       "randomly choose up, right, left, or down.  Always suck when there is dirt. If all of them are clean and at home, shut-off"
       (map nil))

(defun updatemap (map loc dirt obs)
  "update map by percepts"
  (if (= dirt 0) (setcell map loc '_))
  (for each dir in (getdirs_obs obs) do
    (setcell map (locdir loc dir) 'X)
  )
)

(defun createmap (size)
  "create w X h map, U: unknown, X: occupied, C: clean"
  (let ((w (nth 0 size)) (h (nth 1 size)) (grid nil) row )
    (dotimes (i h)
      (setq row nil)
      (dotimes (j w)
        (setf row (cons 'U row)))
        (setf grid (cons row grid)
      )
    )
    grid
  )
)

(defun mapcomplete (map)
  "checks if there is no unknown cell U contiguous to clean cells _"
  (let ((h (length map))(w (length (first map))) (flag T))
    (dotimes (y (- h 1))
      (dotimes (x (- w 1))
        (if (or (and (equal 'U (getcell map (list x y))) (equal '_ (getcell map (list (+ x 1) y))))
                (and (equal '_ (getcell map (list x y))) (equal 'U (getcell map (list (+ x 1) y)))))
          (setq flag nil)
        )
        (if (or (and (equal 'U (getcell map (list x y))) (equal '_ (getcell map (list x (+ y 1)))))
                (and (equal '_ (getcell map (list x y))) (equal 'U (getcell map (list x (+ y 1))))))
          (setq flag nil)
        )
      )
      (if (or (and (equal 'U (getcell map (list (- w 1) y))) (equal '_ (getcell map (list (- w 1) (+ y 1)))))
              (and (equal '_ (getcell map (list (- w 1) y))) (equal 'U (getcell map (list (- w 1) (+ y 1))))))
        (setq flag nil)
      )
    )
    flag
  )
)

(defun setcell (map loc infor)
  "set infor to the cell specified by x and y"
  (let ((x (nth 0 loc)) (y (nth 1 loc)))
    (setf (nth x (nth y map)) infor)
  )
)

(defun getcell (map loc)
  "returns a cell in map specified by x and y"
  (let ((x (nth 0 loc)) (y (nth 1 loc)))
    (nth x (nth y map))
  )
)

(defun dispmap (map)
  (let (h (length map) (w (length (first map))))
    (format t "~%===internal map===~%")
    (for each row in (reverse map) do
      (for each elem in row do
        (format t "~S " elem)
      )
      (format t "~%")
    )
    (format t "~%")
  )
)

(defun getdirs_free (obs)
  "get free directions specified obs"
  (remove-if #'not
     (let ((options '(up down left right)))
      (mapcar #'(lambda (pair) (if (equal (cdr pair) 'non-blocked) (car pair) nil))
       (pairlis options obs)
  )))
)

(defun getdirs_obs (obs)
  "get occupied directions specified obs"
  (remove-if #'not
     (let ((options '(up down left right)))
      (mapcar #'(lambda (pair) (if (equal (cdr pair) 'blocked) (car pair) nil))
       (pairlis options obs)
  )))
)

(defun locdir (loc dir)
  "get location specified by loc + dir (up, down, left, or right)"
  (let ((x (nth 0 loc)) (y (nth 1 loc)))
    (case dir
      ('up (list x (+ y 1)))
      ('down (list x (- y 1)))
      ('left (list (- x 1) y))
      ('right (list (+ x 1) y))
      (t loc)
    )
  )
)
