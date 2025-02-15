;;;; Projeto Adji-boto*
;;;; UC Inteligencia Artificial 2024/2025
;;;; Realizado por:
;;;; Raul Rohjans - 202100518

;; ************************* Entry Related Functions *************************

(defun empty-entry (entry)
  "Checks if the entry is empty"
  (cond ((null entry) nil)
    ((and (= (apply #'+ (linha 0 entry)) 0)
          (= (apply #'+ (linha 1 entry)) 0))
    )
  )
)

;; ************************* Nodes & States *************************

(defun create-node (entry &optional (parent-node nil) (depth 0) (removed-pieces 0) (heuristic 0))
  "Creates a node object to represent a piece of the game."
  (list entry depth parent-node removed-pieces heuristic)
)

(defun node-parent (no) (nth 2 no))
(defun node-depth (no) (nth 1 no))
(defun node-entry (no) (nth 0 no))
(defun node-removed-pieces (no) (nth 3 no))
(defun node-heuristic (no) (nth 4 no))

(defun show-execution-path (no)
  "Shows the executed path until the give node."
  (cond 
    ((null no) (format t "There is no path to show."))
    (t
     (print (list (node-entry no) (node-depth no) (node-removed-pieces no) (node-heuristic no)))
     (show-execution-path (node-parent no)))))

(defun node-solution (no)
  "Checks if the entry of a node is empty"
  (empty-entry (node-entry no)))

(defun create-node-aux (entry pieces &optional (heuristic 0))
  (list entry pieces heuristic)
)

(defun node-aux-entry (no)
  (first no)
)

(defun node-aux-piece (no) 
  (second no)
)