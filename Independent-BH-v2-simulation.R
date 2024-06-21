# This includes the same simulations as in Independent-BH-v2 but is in a separate R script so 
# that the simulations can be run as a background job

library(knockoff)
library(pracma)
library(tidyverse)

set.seed(12)
independent_BH_setting1_v2 <- run_independent_BH_setting1_simulation()

set.seed(12)
independent_BH_setting2_v2 <- run_independent_BH_setting2_simulation()

set.seed(12)
independent_BH_setting3_v2 <- run_independent_BH_setting3_simulation()

