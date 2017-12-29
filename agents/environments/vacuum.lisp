;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

; load a setting File
(defun loadworld (&optional (steps nil))
  (setq in (open (concatenate 'string (namestring *aima-root*) "agents/setting.txt")))
  (setq size (@ (read in) (read in)))
  (setq d_level (list 'at 'all (list 'P (read in) 'dirt)))
  (setq f_list nil)
  (setq c_list nil)
  (loop while (setq line (read-line in nil)) do
    (setq buff (make-string-input-stream line))
    (setq lst nil)
    (loop while (setq token (read buff nil)) do
      (setq lst (append lst (list token)))
    )
    (if (= (length lst) 4)
      (setq f_list
        (cons
          (list 'at (list 'area (subseq lst 0 2) (subseq lst 2 4)) 'furn)
          f_list
    ) ) )
    (if (= (length lst) 2)
      (setq c_list
        (cons
          (list 'at lst 'cat)
          c_list
    ) ) )
  )
  (close in)
  (if (not steps)
    (make-vacuum-world :size size :cspec (append (list d_level) f_list c_list))
    (make-vacuum-world :max-steps steps :size size :cspec (append (list d_level) f_list c_list))
  )
)

;;;; The Vacuum World: cleaning up dirt in a grid

(defstructure (dirt (:include object (name "*") (size 0.01))))

; firniture element
(defstructure (furn (:include obstacle (name "F") (size 1.0))))
; cat
(defstructure (cat (:include obstacle (name "C") (size 0.5))))

(defstructure (vacuum-world (:include grid-environment
    (size (@ 10 9))
    (aspec '(improved-vacuum-agent))
    (cspec '((at all (P 0.25 dirt))(at (area (3 3) (5 5)) furn)(at (area (4 7) (5 7)) furn)(at (7 4) cat)))))
  "A grid with some dirt in it, and by default a reactive vacuum agent.")

;;;; Defining the generic functions

(defmethod performance-measure ((env vacuum-world) agent)
  "100 points for each piece of dirt vacuumed up, -1 point for each
  step taken, and -1000 points if the agent does not return home."
  (- (* 100 (count-if #'dirt-p (object-contents (agent-body agent))))
     (environment-step env)
     (if (equal (object-loc (agent-body agent))
		(grid-environment-start env))
	 0
       1000)))

(defmethod get-percept ((env vacuum-world) agent)
  "Percept is a three-element sequence: bump, dirt and home."
  (let ((loc (object-loc (agent-body agent))) (retval nil) dirt obs)
    (progn
      (if (not (improved-vacuum-agent-map agent))
        (setf (improved-vacuum-agent-map agent) (createmap (grid-environment-size env)))
      )
      (setq dirt (detect-dirt env loc))
      (setq obs (detect-obs env loc))
      (updatemap (improved-vacuum-agent-map agent) loc dirt obs)
      (dispmap (improved-vacuum-agent-map agent))
      (list
        (if (object-bump (agent-body agent)) 'bump)
        dirt
        ; replaced (if (find-object-if #'dirt-p loc env) 'dirt)
        (if (equal loc (grid-environment-start env)) 'home)
        obs
        (mapcomplete (improved-vacuum-agent-map agent))
      )
    )
  )
)

(defmethod legal-actions ((env vacuum-world))
  '(suck up right left down shut-off))
  ;; replaced '(suck forward turn shut-off))

;;;; Actions (other than the basic grid actions of forward and turn)

(defmethod suck ((env vacuum-world) agent-body)
  (let ((dirt (find-object-if #'dirt-p (object-loc agent-body) env)))
    (when dirt
      (place-in-container dirt agent-body env))))

(defmethod shut-off ((env environment) agent-body)
  (declare-ignore env)
  (setf (object-alive? agent-body) nil))

(defmethod up ((env environment) agent-body)
  ;"Move the object to the up-side cell."
  (setf (object-heading agent-body) '(0 1))
  (forward env agent-body))

(defmethod down ((env environment) agent-body)
  "Move the object to the up-side cell."
  (setf (object-heading agent-body) '(0 -1))
  (forward env agent-body))

(defmethod right ((env environment) agent-body)
  "Move the object to the up-side cell."
  (setf (object-heading agent-body) '(1 0))
  (forward env agent-body))

(defmethod left ((env environment) agent-body)
  "Move the object to the up-side cell."
  (setf (object-heading agent-body) '(-1 0))
  (forward env agent-body))

;;;;; enhanced sensor ;;;;;
(defmethod detect-dirt ((env environment) loc)
   ;"detects all dirt under the vacuum"
   ;"returns a total amount of dirt"
   (sum
    (remove-if #'not (mapcar #'(lambda (x) (if (dirt-p x) x)) (grid-contents env loc)))
    #'(lambda (n) (dirt-size n))
   ))

(defmethod detect-obs ((env environment) loc)
   ;"detects obstacles in the 4 neighboring cells
   ;"blocked/not-blocked for up down left right"
  (let ((headings '((0 1) (0 -1) (-1 0) (1 0))))
   (mapcar
    #'(lambda (heading)
     (let ((target (add-locs loc heading)))
      (if (find-object-if #'obstacle-p target env) 'blocked 'non-blocked)))
    headings
   )
  )
)
