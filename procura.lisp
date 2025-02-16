;;;; Projeto Adji-boto*
;;;; UC Inteligencia Artificial 2024/2025
;;;; Realizado por:
;;;; Raul Rohjans - 202100518

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

(defun existsp (node closed-nodes) 
  (some (lambda (closed-node) (equal (node-entry node) (node-entry closed-node))) closed-nodes)
)

(defun default-heuristic (node)
  (cond ((null node) 0)
    (t (- (entry-piece-count (node-entry node)) (cadr node)))
  )
)
;; ************************* DFS *************************

