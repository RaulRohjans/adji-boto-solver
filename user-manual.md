# **User Manual - Projeto Adji-Boto**

## **Introduction**
The **Adji-Boto** project is a **Common Lisp** application designed to solve game board problems based on the **Oware** game. It employs search algorithms (BFS, DFS, and A*) to find the best sequence of moves for either capturing the maximum number of pieces or emptying the board.

This manual is intended for end users and provides guidance on how to use the application, including examples, outputs, and limitations.


## **Objective**
To solve Adji-Boto game boards using search algorithms that explore different strategies to reach the final game state (an empty board).


## **Requirements**
- A compatible Lisp environment (e.g., SBCL, CLISP, or equivalent).
- Project files:
  - `procura.lisp`
  - `puzzle.lisp`
  - `projeto.lisp`
  - Input file: `problemas.dat` (containing predefined boards).
- A compatible operating system (Windows, Linux, or macOS).


## **Getting Started**
1. Ensure all required files are in the same directory.
2. Open a terminal or Lisp interpreter.
3. Load the main file:
   ```lisp
   (load "projeto.lisp")
   ```
4. Start the program:
   ```lisp
   (init)
   ```


## **Interacting with the Program**

### **Main Menu**
After starting the program, the following menu options will be displayed:

| Option | Description |
|--------|------------|
| 1      | Solve Game |
| 2      | Show Available Entries |
| 0      | Exit |


### **Solving a Board**
Selecting **option 1** will take you to the algorithm selection menu:

1. **Choose an Algorithm**:
   - **1 - BFS (Breadth-First Search)**: Explores nodes in the order they are discovered, ensuring the shortest path.
   - **2 - DFS (Depth-First Search)**: Explores the deepest path first, with a user-defined depth limit.
   - **3 - A (A-Star Search)**: Uses heuristics to prioritize promising solutions.

2. **Select a Board Entry**:
   - A numbered list of available boards will be displayed.
   - Enter the corresponding number for the board you wish to solve.

3. **Algorithm Execution**:
   - The program will attempt to solve the board using the selected algorithm.
   - If using A*, the user must choose a heuristic:
     - **1 - Default heuristic** (based on remaining pieces).
     - **2 - Custom heuristic** (based on piece distribution).

4. **Results**:
   - If a solution is found:
     - The execution path will be displayed, including board states, depth, and heuristic values.
   - If no solution is found:
     - The program will notify the user that no solution was possible.


### **Displaying Available Boards**
Choosing **option 2** will list all board entries available in `problemas.dat`. Each board is numbered, allowing the user to review the initial configurations before selecting one to solve.


### **Example Execution**
#### **Steps**:
1. Start the program and choose **option 1** (solve a board).
2. Select **A*** as the search algorithm and choose the default heuristic.
3. Select board **1** from the list.

#### **Output**:
```lisp
=== Adji-boto* ===
1. Solve Game
2. Show Available Entries
0. Exit
Select an option: 1

=== Adji-boto* ===
1. BFS
2. DFS
3. A*
0. Exit
Select an option: 3

=== Available Game Entries ===
1: ((0 0 0 0 0 2) (0 0 0 0 4 0))
2: ((2 2 2 2 2 2) (2 2 2 2 2 2))
3: ((0 3 0 3 0 3) (3 0 3 0 3 0))
4: ((1 2 3 4 5 6) (6 5 4 3 2 1))
5: ((2 4 6 7 10 12) (12 10 8 6 4 2))
6: ((48 0 0 0 0 0) (0 0 0 0 0 48))
7: ((8 8 8 8 8 8) (8 8 8 8 8 8))

Done.

Choose an entry: 1

1. Provided Heuristic
2. Custom Heuristic
Select an option: 1

Starting A*...

(((0 0 0 0 0 0) (0 0 0 0 0 0)) 6 6 -6) 
(((0 0 0 0 0 0) (0 0 0 0 0 1)) 5 5 -4) 
(((0 0 0 0 0 1) (0 0 0 0 0 1)) 4 4 -2) 
(((0 0 0 0 1 1) (0 0 0 0 0 1)) 3 3 0) 
(((0 0 0 0 0 0) (0 0 0 0 4 0)) 2 2 2) 
(((0 0 0 0 1 0) (0 0 0 0 4 0)) 1 1 4) 
(((0 0 0 0 0 2) (0 0 0 0 4 0)) 0 0 6) 
NIL
```


## **Generated Information**
### **During Execution**:
- **Execution Path**: Sequence of board states.
- **Depth**: Number of moves performed.
- **Heuristic Value**: Estimated quality of the current state.
- **Solution Found**: Final board state (if a solution exists).

### **Error Messages**:
- Invalid input (when entering an option not listed in the menu).
- No solution found (when the algorithm fails to resolve the board).


## **Limitations**
1. **User Input & Interaction**:
   - The program only supports text-based input.
   - No graphical feedback is provided.

2. **Complex Cases**:
   - Search algorithms may take excessive time or fail for large problem instances.
   - The heuristic may not be optimal for certain board configurations.

3. **Statistical Analysis**:
   - The program does not automatically generate detailed performance metrics such as execution time or branching factor.


## **Conclusion**
The **Adji-Boto** application effectively solves board problems using classical search algorithms. This manual provides all necessary instructions for operating the program, selecting search strategies, and interpreting results.

For additional information or troubleshooting, refer to the technical documentation or seek further assistance.

