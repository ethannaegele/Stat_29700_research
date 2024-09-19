Dependencies:

library(knockoff)

library(pracma)

library(tidyverse)

library(cknockoff)

library(dbh)

library(TreeTools)


Install dbh from github at: https://github.com/lihualei71/dbh

Install cknockoff from github at: https://github.com/yixiangLuo/cknockoff

To use the simulation code in this repository:

1. Run utils.R
2. Run methods.R
3. Run simulation-functions.R

These files give you the functions necessary to run the simulations. 

The simulations are as follows:

1. settings1-3-simulations.R are the three settings described in Sarkar and Tang's paper with an autoregressive design matrix
2. mcc-iid-simulations.R are MCC and IID Normal design settings, which roughly replicate the simulations done in Luo, Fithian, and Lei's paper.
   Note that for these simulations, the signal strength is fixed such that base BH will give a power of about 35% or 50%; the first
   four rows of the dataframe from running the simulation are for 35%, and the last four are for 50%. In this setting, we vary alpha.
4. CKnockoff-simulations.R are the simulations for each setting using the CKnockoff method,
   which are put into a separate file due to the extremely long computation time of the method. It is not recommended to put this method in the same
   job as the other methods for this reason.
5. ep-moment-set1-v4.R gives the results for different moments with the ep moment method, as well as with dampening for Setting 1.
6. Likewise for Setting 2 and Setting 3, except higher moments are not used due to computation concerns.
7. ep-moment-mult2-set1.R shows how different multipliers affect the power of the ep-moment method.
8. method1-known-sigma-set1-3-sims-v4.R simulates power for Sarkar and Tang's method when sigma is known instead of estimated.
9. ep-moment-known-sigma-set1-3-sims-v4.R simulates power for the ep-moment method when sigma is known instead of estimated.
10. ep-BH-cutoff-set1-sims-v4.R simulates power when using Sarkar and Tang's method with different cutoffs instead of alpha^(1/2) in Setting 1
11. Likewise for Setting 2 and Setting 3 with ep-BH-cutoff-set2-sims-v4.R and ep-BH-cutoff-set3-sims-v4.R

To view the simulation results:

1. Run the code in Sarkar-Tang-graph-replication-final.Rmd to see the graphs in Sarkar and Tang's paper replicated
2. Run the code in ep-BH-moment-graph-summary-final.Rmd to see performance of all methods in Settings 1-3
   and to see performance of different moments, and performance of dampening
3. Run the code in ep-moment-vs-method1-sigma-graphs-final.Rmd to see the performance of the ep-moment method compared to Method 1 from Sarkar and Tang
4. Run the code in mcc-iid-methods-graph-summary.Rmd to see the performance of methods in the MCC and IID Normal simulation settings
5. Run the code in ep-BH-cutoff-graphs-summary-v4.Rmd to see performance of Method 1 using different cutoffs
6. Run the code in ep-moment-mult-graph-summary.Rmd to see how different multipliers affect the performance of the ep-moment method
