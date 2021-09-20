# HOC2 - Horizon-optimized convex combinations

This repository contains R source code for the Horizon-optimized Convex Combinations (HOC2) framework, a forecast combination framework for time-series, authored by [Rafael de O. Valle dos Santos](https://www.linkedin.com/in/rafael-de-olivaes-valle-dos-santos-5667b2163/), [Celso F. Araujo F.](https://www.linkedin.com/in/celsoaraujobr/), [Ricardo M. S. Accioly](https://www.linkedin.com/in/ricardo-accioly-722270/) e [Fernando Luiz Cyrino Oliveira](https://www.linkedin.com/in/fernando-cyrino-422b4951/).

For more information about HOC2, read/cite the paper:

  - **Horizon-optimized weights for forecast combination with cross-learning** (To appear in
    ["Pesquisa Operacional"](https://www.scielo.br/j/pope/)).
    
The data used in the paper can be found at the [M4 Competition repo](https://github.com/Mcompetitions/M4-methods) (M4 dataset) and at the [MComp](https://cloud.r-project.org/web/packages/Mcomp/index.html) R package (M1 and M3 datasets).

# Repository description
There is a folder for each phase of the framework: *methods*, *clustering*, *training* and *test*, each one including source code and binary files. 

For simplcity sake, all the source files here are set to use **YEARLY** data from the **M4** dataset, with only 10% of the original data for the *training* phase. 
 
The *methods* folder include source files to fit each individual method considered to be combined (in this case, 5: ETS, AUTOARIMA, THETA, TBATS and SNAIVE) and also a source file to compute their simple average (*simpleAVG.R*). Results are computed in binary (RData) and text (CSV) files.

Note that theres is a *training* file for each non-empty *cluster* (in this case, clusters 1, 2, 3, 10, 11 and 12). 

The clusters may be trained in paralalel but all resulting training file - e.g. *trainLog01.RData* for cluster 1 - should be joined with *join.R* in a single binary file: *trainLog.RData*. This last file shall then be used as input for the *test* phase.

Each folder contains *report* files for reporting results. 

Enjoy!
