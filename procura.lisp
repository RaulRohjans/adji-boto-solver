;;;; Projeto Adji-boto*
;;;; UC Inteligencia Artificial 2024/2025
;;;; Realizado por:
;;;; Raul Rohjans - 202100518

;; ************************* Heuristics *************************

(defun default-heuristic (node)
  (cond ((null node) 0)
    (t (- (entry-piece-count (node-entry node)) (cadr node)))
  )
)

(defun custom-heuristic (node)
  "Measures how spread out the pieces are, to estimate difficulty.
  This favors boards where pieces are concentrated in fewer columns because 
  spreading them out means more moves."
  (let* ((entry (node-entry node))
         (upper-row (remove 0 (car entry)))
         (lower-row (remove 0 (cadr entry)))
         (spread (+ (length upper-row) (length lower-row))))
    spread)
)


;; ************************* BFS *************************

(defun bfs (node &optional (open-nodes (list node)) (closed-nodes nil))
  (cond ((node-solution node) node)
        ((null open-nodes) nil)
        (t (let* ((sucessores (remove-if #'(lambda (suc) (existsp suc closed-nodes))
                  (expand-node (car open-nodes))))
                  (new-open-nodes (append (cdr open-nodes) sucessores)))
           (bfs (car new-open-nodes) new-open-nodes (cons (car open-nodes) closed-nodes))))
  )
)


;; ************************* DFS *************************

(defun dfs (node max-depth &optional (open-nodes (list node)) (closed-nodes nil))
  (cond ((node-solution node) node)
        ((>= (node-depth node) max-depth) nil)
        ((null open-nodes) nil)
        (t (let* ((current-node (car open-nodes))
              (sucessores (remove-if #'(lambda (suc) (existsp suc closed-nodes))
                                     (expand-node current-node)))
              (new-open-nodes (append sucessores (cdr open-nodes))))
          (dfs (car new-open-nodes) max-depth new-open-nodes (cons current-node closed-nodes))))
  )
)


;; ************************* A* *************************

(defun a-star (node heu &optional (open-nodes (list node)) (closed-nodes nil))
  (cond ((node-solution node) node)
        ((null open-nodes) nil)
        (t (a-star (car open-nodes) heu (open-nodes-a (cdr open-nodes) 
                    (apply #'append 
                          (mapcar  #'(lambda (suc) (if (or (null open-nodes) (existsp suc closed-nodes)) '() (list suc))) 
                                    (gen-sucessors (car open-nodes) heu))))
                   (cons (car open-nodes) closed-nodes))
        )
  )
)


(defun open-nodes-a (open-nodes sucessors)
  "Returns a sorted list with the open nodes"
  (quicksort (append sucessors open-nodes))
)

(defun quicksort (no)
  (cond ((null no) nil)
    (t (let* ((x (car no))
        (r (cdr no))
        (fn (lambda (a) (< (node-heuristic a) (node-heuristic x)))))
        (append (quicksort (remove-if-not fn r))
          (list x)
          (quicksort (remove-if fn r)))
      )
    ) 
  )
)