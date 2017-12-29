# Mods on AIMA-LISP

This is basically from my course work where I modified some code of AIMA-LISP, which is now available from the [author's github page](https://github.com/aimacode/aima-lisp).
Since the lisp programming part of AIMA is no longer active, I decided to put my (little) work on the lisp code (and also my personal purpose to backup my code). I hope this will help somebody.

## SETUP
It is the same way as the original code is installed and I believe the instruction page is still [alive](http://aima.cs.berkeley.edu/lisp/doc/install.html).
But just in case, I list up items to do.
- modify aima.lisp (line 9 and 13) for your environment (root dir and binary file type)
- compile the code
```
(load 'aima.lisp)
(aima-load 'all)
(aima-compile)
```
If clips is used as a lisp environment, it generates an error saying something is locked, but just ignore and continue by typing 'continue'.

## Modifications
I modified the vacuum robot agent to add additional features.
At the moment, modifications were applied only to "agients" directory.

The code can run in the default way, and perform the newly added features.
```
(run-environment (make-vacuum-world :max-steps 10))
```

The setting file "setting.txt" in the agents folder is loaded to run a specific world setting.
```
(run-environment (loadworld 10))
(run-environment (loadworld))
```
The first line runs for 10 steps. The second one runs until the robot cleans all dirty cells
and goes back home or the number of steps exceeds 1000.

The modifications are described as follows.

### Resizable Environment
A default size of the environment can be set in the definition of vacuum-world. In environ-
ments/vacuum.lisp,
```
(defstructure (vacuum-world (:include grid-environment
    (size (@ 10 9))
...
```
For setting a maximum size, I set a limitation of the size so that it does not exceed
40x40. The size is adjusted when the environment is initialized.
In environments/grid-env.lisp,
method "initialize" assign 40 to the environment size if width or height is bigger than 40.

### Mobility
New motions (up, down, left, and right) are defined in environments/vacuum.lisp,
and they are listed in "legal-actions."
Each one is implemented by combining the two actions: turning and forwarding.
The following is the action "up". It first turns upward, and goes forward.
```
(defmethod up ((env environment) agent-body)
   ;"Move the object to the up-side cell."
   (setf (object-heading agent-body) ’(0 1))
   (forward env agent-body))
```

### Percepts
This section corresponds requirements 3, 4, and 5.
In environments/vacuum.lisp,
"get-percept" provides a list of the following percepts based on the given environment:
bump, dirt, home, obstacles, and task completion.
"bump" indicates if the robot previously hit some obstacle.
"dirt" contains the amount of dirt on the current cell.
"home" indicates if the robot is currently at the home cell.
"obstacles" is a list each of which contains "blocked" or "non-blocked"
for up, down, left, and right, respectively.
"task completion" indicates if the environment has no dirt based on the internal map.
For example, if the robot is at the home cell and the task is not completed yet, the
percepts would be as follows:
```
(NIL 0 HOME (NON-BLOCKED BLOCKED BLOCKED NON-BLOCKED) NIL)
```

### Obstacles
Furniture and cats were added as obstacles. They are defined in environments/vacuum.lisp.
```
; firniture element
(defstructure (furn (:include obstacle (name "F") (size 1.0))))
; cat
(defstructure (cat (:include obstacle (name "C") (size 0.5))))
```
To apply them to the environment, they are set to "cspec" in "vacuum-world."
Unlike dirt, specific locations are given to apply them.
```
(defstructure (vacuum-world (:include grid-environment
...
   (cspec ’(
      (at all (P 0.25 dirt))
      (at (area (3 3) (5 5)) furn)
      (at (area (4 7) (5 7)) furn)
      (at (7 4) cat)))))
...
```

### Internal Map
The modified agent "improved-vacuum-agent" has its internal map. Necessary functions to
create and update it are defined in agents/vacuum.lisp. The map is updated and evaluated
to determine if the task is completed when "get-percept" is executed. The following is an
example of internal map, which is displayed while the program is running.
```
===internal map===
U U U U X X X X X U
U U U X _ _ _ _ _ X
U X X X _ _ _ _ _ X
X _ _ _ _ _ _ _ _ X
X _ X _ _ _ X X X U
X _ _ _ _ _ X U U U
X _ _ X _ _ X U U U
X _ _ _ _ _ X U U U
U X X X X X U U U U
```
"U" represents an unknown cell. "X" represents an obstacle such as wall, furniture, and
cat. An underscore "\_" means a clean cell. When no "\_" is contiguous to any "U", it indicates
there is no more accessible dirty cell in the environment. Then the function "mapcomplete"
says T.

### Setting File
"setting.txt" is the setting file to specify the environment. The first line specifies the size of
the world. The second specifies the probability to place dirt in a cell. The following lines are
either furniture or cat. If the line contains two numbers, it represents the location of a cat.
If the line contains four numbers, it specifies the range of furniture: the first two numbers
are the bottom-left point, and the latter ones are the top-right point.

***
The following is the original README.md
***

# aima-lisp

Common Lisp implementation of algorithms from Russell And Norvig's book *Artificial Intelligence - A Modern Approach.*

This repository was the original code base, back in 1995.
Since then, the Java and Python versions have become more popular, and this Lisp version is no
longer up-to-date. But it is here for whatever use you want to make of it.
