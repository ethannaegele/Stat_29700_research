# This includes the same simulations as in Sarkar-Tang-method2-v2 but is in a separate R script so 
# that the simulations can be run as a background job. Now, the function uses a multiplier of 1.9 to obtain the D matrix for T1

library(knockoff)
library(pracma)
library(tidyverse)

set.seed(12)
method2_setting1_v4 <- run_sarkar_tang_method2_setting1_simulation()

set.seed(12)
method2_setting2_v4 <- run_sarkar_tang_method2_setting2_simulation()

set.seed(12)
method2_setting3_v4 <- run_sarkar_tang_method2_setting3_simulation()