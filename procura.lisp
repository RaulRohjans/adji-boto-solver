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

