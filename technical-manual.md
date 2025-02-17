# Technical Report

## Projeto Adji-boto*
**UC Inteligência Artificial 2024/2025**  
**Realizado por:** Raul Rohjans - 202100518

## Introduction
The goal of this project was to develop an algorithm solver for the Adji-boto* puzzle using search algorithms. The implemented algorithms include:
- Breadth-First Search (BFS)
- Depth-First Search (DFS)
- A* Search with two different heuristics (default and custom)

The implementation was done in **Common Lisp**, and the project consists of three main files:
- `projeto.lisp`: Contains the main functions and UI for user interaction.
- `procura.lisp`: Implements the search algorithms.
- `puzzle.lisp`: Defines the puzzle mechanics and node operations.

## Heuristic Functions
### Default Heuristic
The default heuristic estimates the number of remaining moves by evaluating how many pieces are still present in the puzzle. It is calculated as:

\[ h(n) = 
{remaining pieces} - {steps taken} \]

This heuristic provides a basic estimate but does not consider piece distribution or complexity of movement.

### Custom Heuristic
The custom heuristic takes a more refined approach by measuring how spread out the pieces are within the puzzle. The rationale is that a more concentrated arrangement of pieces generally allows for more efficient moves. This heuristic is defined as:

\[ h(n) = 
{spread of pieces} \]

It calculates the number of pieces present in each row and column, prioritizing moves that consolidate them into fewer positions. This results in a more informed search compared to the default heuristic.

## Implementation Details
### Search Algorithms
#### Breadth-First Search (BFS)
BFS explores the search tree level by level. It guarantees finding the shortest solution but is memory-intensive.

#### Depth-First Search (DFS)
DFS explores a branch fully before backtracking. During testing, a depth limit of 10 was used to prevent excessive recursion. However, deeper solutions might not be found.

#### A* Search
A* uses a heuristic function to guide the search toward the goal more efficiently.
- **Default heuristic**: Estimates remaining moves based on the number of pieces left.
- **Custom heuristic**: Evaluates the spread of pieces, favoring configurations where pieces are concentrated in fewer columns.

## Auxiliary Functions
### Board Manipulation
- `upper-entry-line(entry)`: Returns the upper row of a given board.
- `lower-entry-line(entry)`: Returns the lower row of a given board.
- `entry-piece-count(entry)`: Counts the number of pieces in a board.
- `replace-piece(line, cell, entry, piece)`: Replaces a piece at a specific position.

### Node Operations
- `create-node(entry, parent-node, depth, removed-pieces, heuristic)`: Creates a node for state representation.
- `expand-node(node)`: Expands a node to generate successors.
- `existsp(node, closed-nodes)`: Checks if a node already exists in the closed list.

## Limitations
1. **Performance Issues**:
   - BFS and DFS are impractical for large problem instances due to memory constraints.
   - A* relies heavily on the effectiveness of the heuristic function.
2. **Unsolved Cases**:
   - For highly complex board configurations, none of the algorithms return a solution due to stack overflows or excessive computation time.

## Critical Analysis
1. **Algorithm Efficiency**:
   - **BFS**: Guarantees shortest solutions but is highly memory-intensive.
   - **DFS**: Uses less memory but may not find the optimal solution.
   - **A***: Provides the best balance but requires well-designed heuristics.

2. **Heuristic Performance**:
   - The **default heuristic** is effective in simple cases but lacks strategic insight.
   - The **custom heuristic** improves efficiency by prioritizing clustered piece arrangements.

3. **A Algorithm Inefficiency**:
   - The implementation of A* sometimes results in errors or inefficient execution.

4. **Statistics**:
   - The program does show the execution path correctly, however there are no metrics about the execution, such as generated nodes or expanded nodes.


## Conclusion
This project successfully implemented search algorithms for solving the Adji-boto* puzzle. The use of heuristics in A* demonstrated significant improvements over uninformed search strategies. However, the current implementation struggles with large problem instances due to recursion depth and memory limitations.

### Possible Improvements
- Optimize DFS and BFS to prevent stack overflows.
- Add more heuristic functions for A* to better guide the search.

