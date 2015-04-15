# sig2trial
Pipeline R package for creating simple gene signatures and testing their value in a clinical trial setting. Automated, standardized reporting of results.

### Description

This package provides a function that builds simple, interpretable gene signatures from gene expression data. The user needs to provide a matrix of
gene expression values by samples and a vector of outcomes (currently guarantees support for binary outcomes). A small tree algorithm (<=10 genes) is returned,
along with a complete report of the analysis steps and results reflecting the accuracy of the predictor. The tree algorithm uses Top Scoring Pairs (TSPs) as
features and employs feature selection methods that take advantage of the use of these features. Broad details of the methodology are explained in every report 
built using this package. Specific details are provided in the manuscript that this package accompanies (forthcoming).

Additionally, we leverage work done by Colantuoni and Rosenblum (2015, in press) to provide direct estimates of how valuable this predictor would be in a
clinical trial setting. Within the report, we provide a crude estimate of how much precision gain we might expect if we adjusted our estimate of the treatment effect
between two arms of a clinical trial using the predictions from our tree. We provide an additional function that takes the built tree and patient data as an input and 
provides a better approximation to the possible precision gain via simulation (this is more time-intensive and hence optional).

### Installation

1. Have R installed
2. Ensure that the devtools package is installed. Use `install.packages("devtools")` otherwise.
3. `install_github("prpatil/sig2trial")`

### R Package Dependencies
rpart, rattle, pROC, knitr, rmarkdown, knitrBootstrap

### External dependencies
pandoc

### Authors
Prasad Patil
Jeff Leek
