# Benchmark to measure the execution time of the function to solve an optimal supervision tree

Benchmark to verify that the proposed algorithm is fast enough for many realistic cases. In the experiment, a randomly generated graph is input to the proposed algorithm and the execution time is measured. The number of vertices and edges are varied, with the number of vertices varying from 10, 20, ..., 70, and the number of edges is 1, 2, or 3 times the number of vertices. 100 trials are performed for each variable combination.

The benchmark is run as follows:

```sh
./benchmark
```

The benchmark program outputs a CSV file that records the number of vertices and edges and the execution time. The experimental results can be converted to a figure as follows:

```sh
python3 generate_figure.py
```

The figure also displays the median time for each number of vertices.
