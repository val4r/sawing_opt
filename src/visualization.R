library(here)
library("assertthat")
source(here("src/functions.R"))

path1 <- paste0(here(), "/data/")

number_of_logs <- readRDS(paste0(path1, "n_log_all_iters_1.rds"))

radius <- readRDS(paste0(path1, "r_all_iters_1.rds"))

production <- readRDS(paste0(path1, "prod_all_iters_1.rds"))

#lasketaan keskiarvot simulaatioiteraatioiden yli



