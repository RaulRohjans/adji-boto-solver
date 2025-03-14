;;;; Projeto Adji-boto*
;;;; UC Inteligencia Artificial 2024/2025
;;;; Realizado por:
;;;; Raul Rohjans - 202100518


;; ************************* Helper Functions *************************
(defun get-project-path ()
  "Gets the project path"
  "/home/raul/Documents/GitHub/adji-boto-solver"
)

(defun read-game-entries ()
  "Reads the list of game entries from the file problemas.dat"
  (with-open-file (file (concatenate 'string (get-project-path) "/problemas.dat") 
                        :direction :input :if-does-not-exist :error)
                  (read file)
  )
)

(defun get-game-entry (num entries)
  "Return the game entry corresponding to the num index"
  (nth num entries)
)

(defun select-entry ()
  "Allows the user to select a game entry."
  (let ((entries (read-game-entries))) ;; Read entries first
    (show-available-entries) ;; Display all available entries    

    (format t "~%Choose an entry: ")
    (let ((choice (read)))
      (cond
        ((and (integerp choice) (>= choice 1) (<= choice (length entries)))
         (get-game-entry (1- choice) entries))

        (t
         (format t "Invalid input. Please enter a number between 1 and %D.~%" (length entries))
         (select-entry))
      )
    )
  )
)

(defun select-depth ()
  "Allows the user to select a maximum depth for the DFS algorithm."
  (format t "Max depth: ")
  (let ((depth (read)))
    (cond ((and (integerp depth) (> depth 0)) depth)
          (T (progn (format t "Invalid input.~%")
               (select-depth)))
    )
  )
)

(defun select-heuristic ()
  "Allows the user to select a heuristic to use in A* and its derivatives."
  (format t "~%1. Provided Heuristic")
  (format t "~%2. Custom Heuristic")
  (format t "~%Select an option: ")
  (let ((choice (read)))
    (case choice
      (1 #'default-heuristic)
      (2 #'custom-heuristic)
      (t (format t "Invalid option.~%") (select-heuristic))
    )
  )
)


;; ************************* Program Init *************************

(defun init ()
  "Starts the program by importing other files and loading main menu"

  ;; File imports
  (load (concatenate 'string (get-project-path) "/procura.lisp"))
  (load (concatenate 'string (get-project-path) "/puzzle.lisp"))

  ;; Load entries
  (main-menu)
)


;; ************************* Menu Display & Handler Functions *************************

(defun main-menu ()
  "Displays the main menu and handles user input."
  (format t "~%=== Adji-boto* ===")
  (format t "~%1. Solve Game")
  (format t "~%2. Show Available Entries")
  (format t "~%0. Exit")
  (format t "~%Select an option: ")
  (let ((choice (read)))
    (case choice
      (1 (algorithm-menu))
      (2 (show-available-entries))
      (0 (format t "Exiting...~%"))
      (t (format t "Invalid option. Try again.~%") (menu))))
)

(defun algorithm-menu ()
  "Displays the algorithm choices to solve the game entries."
  (format t "~%=== Adji-boto* ===")
  (format t "~%1. BFS")
  (format t "~%2. DFS")
  (format t "~%3. A*")
  (format t "~%0. Exit")
  (format t "~%Select an option: ")
  (let ((choice (read)))
    (case choice
      (1 (start-bfs))
      (2 (start-dfs))
      (3 (start-a))
      (0 (format t "Exiting...~%"))
      (t (format t "Invalid option. Try again.~%") (menu))))
)

(defun start-bfs ()
  (let ((entry (select-entry)))
    (format t "~%Starting BFS...~%")
    (let ((res (bfs (create-node entry))))
         (cond (res (show-execution-path res))
               (T (format t "No solution found."))
         )
    )
  )
)

(defun start-dfs ()
  (let ((entry (select-entry)))
    (let ((depth (select-depth)))
      (format t "~%Starting DFS...~%")
      (let ((res (dfs (create-node entry) depth)))
        (cond (res (show-execution-path res))
              (T (format t "No solution found."))
        )
      )         
    )
  )
)

(defun start-a ()
  (let ((entry (select-entry)))
    (let ((heu (select-heuristic)))
      (format t "~%Starting A*...~%")
      (let ((res (a-star (create-node entry nil 0 0 (funcall heu (list entry 0))) heu)))
        (cond (res (show-execution-path res))
              (T (format t "No solution found."))
        )
      )         
    )
  )
)

(defun show-available-entries (&optional (entries (read-game-entries)) (idx 1))
  "Displays the available game entries from problemas.dat"
  (cond ((null entries) (format t "~%Done.~%")) ;; Exit when there are no entries
    ((= idx 1) ;; On the first call print headers
     (format t "~%=== Available Game Entries ===~%")
     (format t "~D: ~A~%" idx (car entries))
     (show-available-entries (cdr entries) (+ idx 1))
    )
    (t
     (format t "~D: ~A~%" idx (car entries))
     (show-available-entries (cdr entries) (+ idx 1))
    )
  )
)

(defun show-execution-path (node &optional (isRecursive nil))
  "Shows the executed path until the given node and writes it to a file."
  (let ((file-path (concatenate 'string (get-project-path) "/execution_output.txt")))
    (with-open-file (file file-path :direction :output :if-exists :supersede)
      (let ((output ""))
        (labels ((collect-path (n)
                   (when n
                     (setf output (concatenate 'string output (format nil "~A ~A ~A ~A~%" (node-entry n) (node-depth n) (node-removed-pieces n) (node-heuristic n))))
                     (format t "~A ~A ~A ~A~%" (node-entry n) (node-depth n) (node-removed-pieces n) (node-heuristic n))
                     (collect-path (node-parent n)))))
          (if (and (null node) (null isRecursive))
              (progn
                (format t "There is no path to show.")
                (setf output "There is no path to show."))
              (collect-path node))
          (write-sequence output file)
        )
      )
    )
  )
)








