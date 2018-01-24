# Benchmarks

To keep up my promise of performance, here are some measurements taken on my
computer.

### Construction

| Function      | New       | Old       |
|---------------|-----------|-----------|
| mkGraph       | 3.479  s  | 705.5 ms  |
| fromList      | 81.92 μs  | 316.4 ms  |

### Basic Interface

| Function      | New       | Old       |
|---------------|-----------|-----------|
| null          | 7.866 ns  | 7.731 ns  |
| match         | 53.05 ms  | 53.88 ms  |
| matchAny      | 50.43 ms  | 54.60 ms  |
| nodes         | 19.33 μs  | 21.87 μs  |
| order         | 6.891 μs  | 8.307 μs  |
| edges         | 297.8 ms  | 203.3 ms  |
| size          | 14.49 ms  | 228.2 ms  |
| (&)           |           | 97.20 ns  |
| (!)           | 29.44 ns  |           |
| (!?)          | 27.56 ns  |           |

### Maps

| Function      | New       | Old       |
|---------------|-----------|-----------|
| nmap          | 1.059  s  | 20.88 μs  |
| emap          | 1.080  s  | 21.09 μs  |
| nemap         | 1.055  s  | 22.02 μs  |
| ctxMap        |           | 20.58 μs  |

### Folds

| Function      | New       | Old       |
|---------------|-----------|-----------|
| foldr         | 47.47 μs  | 287.0 ms  |

### Queries

| Function      | New       | Old       |
|---------------|-----------|-----------|
| member        | 28.25 ns  | 1.113 ms  |
| neighbors     | 311.7 μs  | 1.227 ms  |
| preds         | 14.12 μs  | 1.221 ms  |
| succs         | 13.81 μs  | 1.208 ms  |
| inEdges       | 22.94 μs  | 1.224 ms  |
| outEdges      | 23.04 μs  | 1.228 ms  |
| inDegree      | 58.29 ns  | 1.201 ms  |
| outDegree     | 58.37 ns  | 1.196 ms  |
| degree        | 125.4 ns  | 1.169 ms  |
| hasEdge       | 260.1 ns  | 1.218 ms  |
| hasNeighbor   | 20.99 μs  | 1.165 ms  |

### Filters

| Function      | New       | Old       |
|---------------|-----------|-----------|
| gfiltermap    |           | 1.635  s  |
| nfilter (lab) | 275.4 ms  | 23.18 μs  |
| nfilter (node)| 275.4 ms  | 23.25 μs  |
| subgraph      |           | 243.5 ms  |
| efilter       | 265.9 ms  |           |

### Insertion and Deletion

| Function      | New       | Old       |
|---------------|-----------|-----------|
| insNode       | 84.83 ns  | 67.92 ns  |
| delNode       | 2.858 ms  | 1.105 ms  |
| insEdge       | 501.8 ns  | 160.9 ns  |
| delEdge       |           | 4.095 ms  |
| insNodes      |           | 746.0 ns  |
| delNodes      |           | 237.3 ms  |
| insEdges      |           | 159.6 ns  |
| delEdges      |           | 2.756  s  |

### Algorithms

| Function      | New       | Old       |
|---------------|-----------|-----------|
| bfs           | 147.4 ms  | 576.5 ms  |
| dfs           | 123.8 ms  | 286.1 ms  |
| mst           | 112.3 ms  | 617.8 ms  |

## Detailed

| Function      | New       | Old       |
|---------------|-----------|-----------|
| insNode       | 84.91 ns  | 70.28 ns  |
| insNode-dup   | 80.08 ns  | 91.51 ns  |
| insEdge       | 507.2 ns  | 156.8 ns  |
| insEdge-dup   | 275.8 ns  | 172.2 ns  |
| delNode       | 3.106 ms  | 146.5 μs  |
| delNode-miss  | 19.57 ns  | 36.07 ns  |
| delEdge       | 512.6 ns  | 1.038 ms  |
| delEdge-miss  | 235.8 ns  | 305.8 ns  |
| lookup        | 27.23 ns  |           |
| lookup-miss   | 18.08 ns  |           |

