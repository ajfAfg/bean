import matplotlib.pyplot as plt
import numpy as np

results = np.loadtxt(
    "execution_time_of_function_that_solve_optimal_supervision_tree.csv",
    delimiter=",",
    skiprows=1,
)

ratios = results[:, 1] / results[:, 0]
results_per_edge = [results[ratios == ratio] for ratio in np.unique(ratios)]

for result in results_per_edge:
    # Scatter plot
    x = result[:, 0].astype(int)
    y = result[:, 2]
    plt.rcParams["font.size"] = 16
    plt.scatter(x, y, s=100, c="pink", alpha=0.5, edgecolors="red")
    plt.xticks(list(x), list(map(str, x)))
    plt.xlabel("# of gen_servers", labelpad=15)
    plt.ylabel("Execution time (s)", labelpad=20)
    plt.tight_layout()

    # Line chart
    x = np.unique(result[:, 0].astype(int))
    y = [
        np.median(matrix[:, 2])
        for matrix in [result[result[:, 0] == vertex_num] for vertex_num in x]
    ]
    plt.plot(x, y, label="Median")
    plt.legend(loc="upper left")

    # Generate a graph
    ratio = int((result[:, 1] / result[:, 0])[0])
    plt.savefig(f"execution_time_when_edge_num_is_{ratio}_times_vertex_num.pdf")
    plt.clf()
