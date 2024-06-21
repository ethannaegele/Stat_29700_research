# This includes the same simulations as in Sarkar-Tang-knockoffs-baseline-v2 but is in a separate R script so 
# that the simulations can be run as a background job

library(knockoff)
library(pracma)
library(tidyverse)

set.seed(12)
knockoffs_setting1_v2 <- run_knockoffs_setting1_simulation()

set.seed(12)
knockoffs_setting2_v2 <- run_knockoffs_setting2_simulation()

set.seed(12)
knockoffs_setting3_v2 <- run_knockoffs_setting3_simulation()
