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






# Relatório Técnico

## Projeto Adji-boto*
**UC Inteligência Artificial 2024/2025**  
**Realizado por:** Raul Rohjans - 202100518

## Introdução
O objetivo deste projeto foi desenvolver um algoritmo para resolver o puzzle Adji-boto* utilizando algoritmos de pesquisa. Os algoritmos implementados incluem:
- Pesquisa em Largura (BFS - Breadth-First Search)
- Pesquisa em Profundidade (DFS - Depth-First Search)
- Pesquisa A* com duas heurísticas diferentes (padrão e personalizada)

A implementação foi realizada em **Common Lisp**, e o projeto consiste em três ficheiros principais:
- `projeto.lisp`: Contém as funções principais e a interface de interação com o utilizador.
- `procura.lisp`: Implementa os algoritmos de pesquisa.
- `puzzle.lisp`: Define a mecânica do puzzle e as operações sobre os nós.

## Funções Heurísticas
### Heurística Padrão
A heurística padrão estima o número de movimentos restantes ao avaliar quantas peças ainda estão presentes no puzzle. É calculada da seguinte forma:

\[ h(n) = {pecas restantes} - {passos dados} \]

Esta heurística fornece uma estimativa básica, mas não considera a distribuição das peças ou a complexidade dos movimentos.

### Heurística Personalizada
A heurística personalizada adota uma abordagem mais refinada, medindo a dispersão das peças dentro do puzzle. A ideia é que uma distribuição mais concentrada das peças permite movimentos mais eficientes. Esta heurística é calculada da seguinte forma:

\[ h(n) = {dispersao das peças} \]

Calcula o número de peças presentes em cada linha e coluna, priorizando movimentos que as concentrem em menos posições. Isto resulta numa pesquisa mais informada em comparação com a heurística padrão.

## Detalhes da Implementação
### Algoritmos de Pesquisa
#### Pesquisa em Largura (BFS)
O BFS explora a árvore de pesquisa nível por nível. Garante encontrar a solução mais curta, mas é intensivo em memória.

#### Pesquisa em Profundidade (DFS)
O DFS explora um ramo completamente antes de retroceder. Durante os testes, foi utilizado um limite de profundidade de 10 para evitar recursão excessiva. No entanto, soluções mais profundas podem não ser encontradas.

#### Pesquisa A*
O A* utiliza uma função heurística para guiar a pesquisa de forma mais eficiente em direção ao objetivo.
- **Heurística padrão**: Estima os movimentos restantes com base no número de peças restantes.
- **Heurística personalizada**: Avalia a dispersão das peças, favorecendo configurações onde as peças estão concentradas em menos colunas.

## Funções Auxiliares
### Manipulação do Tabuleiro
- `upper-entry-line(entry)`: Retorna a linha superior de um dado tabuleiro.
- `lower-entry-line(entry)`: Retorna a linha inferior de um dado tabuleiro.
- `entry-piece-count(entry)`: Conta o número de peças num tabuleiro.
- `replace-piece(line, cell, entry, piece)`: Substitui uma peça numa posição específica.

### Operações sobre Nós
- `create-node(entry, parent-node, depth, removed-pieces, heuristic)`: Cria um nó para representação de estado.
- `expand-node(node)`: Expande um nó para gerar sucessores.
- `existsp(node, closed-nodes)`: Verifica se um nó já existe na lista fechada.

## Limitações
1. **Problemas de Performance**:
   - BFS e DFS são impraticáveis para instâncias grandes do problema devido a restrições de memória.
   - A* depende fortemente da eficácia da função heurística.
2. **Casos Não Resolvidos**:
   - Para configurações de tabuleiro altamente complexas, nenhum dos algoritmos retorna uma solução devido a um stack overflow ou tempo de computação excessivo.

## Análise Crítica
1. **Eficiência dos Algoritmos**:
   - **BFS**: Garante soluções mais curtas, mas consome muita memória.
   - **DFS**: Usa menos memória, mas pode não encontrar a solução ótima.
   - **A***: Fornece o melhor equilíbrio, mas exige heurísticas bem projetadas.

2. **Desempenho das Heurísticas**:
   - A **heurística padrão** é eficaz em casos simples, mas carece de insight estratégico.
   - A **heurística personalizada** melhora a eficiência ao priorizar configurações mais agrupadas de peças.

3. **Ineficiência do Algoritmo A***:
   - A implementação do A* por vezes resulta em erros ou execução ineficiente.

4. **Estatísticas**:
   - O programa exibe corretamente o caminho da execução, contudo, não apresenta métricas sobre a mesma, como nós gerados ou expandidos.

## Conclusão
Este projeto implementou com sucesso algoritmos de pesquisa para resolver o puzzle Adji-boto*. O uso de heurísticas no A* demonstrou melhorias significativas sobre estratégias de pesquisa não informadas. No entanto, a implementação atual enfrenta dificuldades com instâncias grandes devido à profundidade de recursão e limitações de memória.

### Possíveis Melhorias
- Otimizar DFS e BFS para evitar stack overflow.
- Adicionar mais funções heurísticas para o A* para melhor guiar a pesquisa.