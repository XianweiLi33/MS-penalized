# Introduction
This repository contains R code to implement the methodology described in the paper "Variable Selection for Progressive Multistate Processes under Intermittent Observation".
## aglasso
The aglasso folder contains a modified version of the **grplasso** (https://cran.r-project.org/web/packages/grplasso/index.html) package incorporating adaptive weights for coefficients. 

## helper_functions
This file contains several helper functions, including:
1. Calculation of the transition probability matrix
2. Computation of conditional expectations for the E-step in the EM algorithm
3. Computation of the observed data log-likelihood
   
## data_generation
This file contains functions to generate sample progressive multistate data for simulation purposes. It includes functions to create:

1. Complete multistate data under continuous observation
2. Observed multistate data under an **independent** intermittent observation scheme
3. Observed multistate data under a **dependent** intermittent observation scheme

## EM
This file contains the code for the EM algorithm.

## example
This file contains the algorithm's implementation for a specific scenario. It serves as an example of how to apply the algorithm to multistate data under intermittent observation.
