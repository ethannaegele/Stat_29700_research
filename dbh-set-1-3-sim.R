library(knockoff)
library(pracma)
library(tidyverse)
library(dbh)

set.seed(12)
dbh_setting1 <- run_dbh_setting1_simulation()

set.seed(12)
dbh_setting2 <- run_dbh_setting2_simulation()

set.seed(12)
dbh_setting3 <- run_dbh_setting3_simulation()
