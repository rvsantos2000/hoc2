# HOC2 - Horizon-optimized convex combinations

This repository contains R source code for the Horizon-optimized Convex Combinations (HOC2) framework, a forecast combination framework for time-series, authored by [Rafael de O. Valle dos Santos](https://www.linkedin.com/in/rafael-de-olivaes-valle-dos-santos-5667b2163/), [Celso F. Araujo F.](https://www.linkedin.com/in/celsoaraujobr/), [Ricardo M. S. Accioly](https://www.linkedin.com/in/ricardo-accioly-722270/) e [Fernando Luiz Cyrino Oliveira](https://www.linkedin.com/in/fernando-cyrino-422b4951/).

For more information about HOC2, read/cite the paper:

  - **Horizon-optimized weights for forecast combination with cross-learning** (To appear in
    ["Pesquisa Operacional"](https://www.scielo.br/j/pope/)).
    
The data used in the paper can be found at the [M4 Competition repo](https://github.com/Mcompetitions/M4-methods) (M4 dataset) and at the [MComp](https://cloud.r-project.org/web/packages/Mcomp/index.html) R package (M1 and M3 datasets).

# Repository description
There is a folder for each phase of the framework, *01-methods*, *02-clustering*, *03-training* and *04-test*, each one including source code and binary files. 

**Attention!** For simplcity sake, all the source files here are set to use **YEARLY** data from the **M4** dataset, with only **10%** of the original data for the *training* phase. 
 
The *methods* folder includes a source file (*run-methods.R*) to fit the individual methods to be combined (ETS, AUTOARIMA, THETA, TBATS and SNAIVE) and another (*simpleAVG.R*) to compute their simple average. Results are stored in binary (RData) and text (CSV) files.

The *clustering* folder includes a source file (*run-cluster.R*) to cluster time series by their ETS model form. Results are stored in a binary (RData) file.

The *training* folder contains one training script for each non-empty cluster (in this case, clusters 1, 2, 3, 10, 11 and 12). E.g.: *train01.R* trains time-series data from cluster 1. 

Each data cluster may be trained in paralalel, but all resulting training files - e.g. *trainLog01.RData* for cluster 1 - should be joined with *join.R* to generate a single result binary file (*trainLog.RData*), that includes the calculated mean weights for each cluster. This last file shall then be used as input (alongside others) for the *test* phase.

**Important!** All folders contains scripts to report results - e.g. *report-test.R* in the *test* folder. 

Enjoy!
