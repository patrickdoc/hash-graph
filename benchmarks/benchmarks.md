# Benchmarks

To keep up my promise of performance, here are some measurements taken on my
computer. For the biggest wins, look to the sections on [queries](#queries) and
[algorithms](#algorithms).

N.B. FGL's implementation is based on Data.IntMap.Lazy, whereas this library is
based on Data.HashGraph.Strict. I've tried to make this comparison fair with
appropriate applications of `nf`. However, if you feel that I've compromised
any of the measurements, please let me know.

I would take these measurements with a grain of salt. Many of the measurements
differ wildly between `whnf` and `nf`, and I have to look a little closer at the
sources to determine the best way to compare them. Help here would be
appreciated :)

All functions run on a graph with 1000 nodes and 1 million edges.

### Construction

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| mkGraph       | 3.479  s  | 705.5 ms  |
| fromList      | 81.92 μs  | 316.4 ms  |

### Basic Interface

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| null          | 7.866 ns  | 7.731 ns  |
| match         | 53.05 ms  | 53.88 ms  |
| matchAny      | 50.43 ms  | 54.60 ms  |
| nodes         | 19.33 μs  | 21.87 μs  |
| order         | 6.891 μs  | 8.307 μs  |
| edges         | 297.8 ms  | 203.3 ms  |
| size          | 14.49 ms  | 228.2 ms  |
| (&)           | 169.0 μs  | 97.20 ns  |
| (!)           | 29.44 ns  |           |
| (!?)          | 27.56 ns  |           |

### Maps

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| nmap          | 1.059  s  | 52.02 ms  |
| emap          | 1.080  s  | 513.9 ms  |
| nemap         | 1.055  s  | 517.3 ms  |
| ctxMap        |           | 748.5 ms  |

### Folds

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| foldr         | 47.47 μs  | 287.0 ms  |

### Queries

| Function      | New       | FGL       |
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

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| gfiltermap    |           | 1.635  s  |
| nfilter (lab) | 275.4 ms  | 51.93 ms  |
| nfilter (node)| 275.4 ms  | 52.28 ms  |
| efilter       | 265.9 ms  | 1.870  s  |
| subgraph      |           | 243.5 ms  |

### Insertion and Deletion

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| insNode       | 84.83 ns  | 67.92 ns  |
| delNode       | 2.858 ms  | 1.105 ms  |
| insEdge       | 501.8 ns  | 160.9 ns  |
| delEdge       | 523.6 ns  | 4.095 ms  |
| insNodes      |           | 746.0 ns  |
| delNodes      |           | 237.3 ms  |
| insEdges      |           | 159.6 ns  |
| delEdges      |           | 2.756  s  |

### Algorithms

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| bfs           | 147.4 ms  | 576.5 ms  |
| dfs           | 123.8 ms  | 286.1 ms  |
| mst           | 112.3 ms  | 617.8 ms  |

## Detailed

| Function      | New       | FGL       |
|---------------|-----------|-----------|
| insNode       | 84.91 ns  | 70.28 ns  |
| insNode-dup   | 80.08 ns  | 91.51 ns  |
| delNode       | 3.106 ms  | 146.5 μs  |
| delNode-miss  | 19.57 ns  | 36.07 ns  |
| insEdge       | 507.2 ns  | 156.8 ns  |
| insEdge-dup   | 275.8 ns  | 172.2 ns  |
| delEdge       | 512.6 ns  | 1.038 ms  |
| delEdge-miss  | 235.8 ns  | 305.8 ns  |
| lookup        | 27.23 ns  |           |
| lookup-miss   | 18.08 ns  |           |

