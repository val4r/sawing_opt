library(here)
library("assertthat")
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)

source(here("src/functions.R"))

path1 <- paste0(here(), "/data/")

number_of_logs_obj_1_orderA <- readRDS(paste0(path1, "n_log_all_iters_A_1_200.rds"))
number_of_logs_obj_2_orderA <- readRDS(paste0(path1, "n_log_all_iters_A_2_200.rds"))
number_of_logs_obj_3_orderA <- readRDS(paste0(path1, "n_log_all_iters_A_3_200.rds"))
# number_of_logs_obj_1_orderB <- readRDS(paste0(path1, "n_log_all_iters_B_1_200.rds"))
# number_of_logs_obj_2_orderB <- readRDS(paste0(path1, "n_log_all_iters_B_2_200.rds"))
# number_of_logs_obj_3_orderB <- readRDS(paste0(path1, "n_log_all_iters_B_3_200.rds"))

radius_obj_1_orderA <- readRDS(paste0(path1, "r_all_iters_A_1_200.rds"))
radius_obj_2_orderA <- readRDS(paste0(path1, "r_all_iters_A_2_200.rds"))
radius_obj_3_orderA <- readRDS(paste0(path1, "r_all_iters_A_3_200.rds"))
# radius_obj_1_orderB <- readRDS(paste0(path1, "r_all_iters_B_1_200.rds"))
# radius_obj_2_orderB <- readRDS(paste0(path1, "r_all_iters_B_2_200.rds"))
# radius_obj_3_orderB <- readRDS(paste0(path1, "r_all_iters_B_3_200.rds"))

production_obj_1_orderA <- readRDS(paste0(path1, "prod_all_iters_A_1_200.rds"))
production_obj_2_orderA <- readRDS(paste0(path1, "prod_all_iters_A_2_200.rds"))
production_obj_3_orderA <- readRDS(paste0(path1, "prod_all_iters_A_3_200.rds"))
# production_obj_1_orderB <- readRDS(paste0(path1, "prod_all_iters_B_1_200.rds"))
# production_obj_2_orderB <- readRDS(paste0(path1, "prod_all_iters_B_2_200.rds"))
# production_obj_3_orderB <- readRDS(paste0(path1, "prod_all_iters_B_3_200.rds"))


#lasketaan keskiarvot simulaatioiteraatioiden yli


mean_df <- simulation_average_prod(production_obj_2_orderA)

long_mean <- mean_df %>% 
  pivot_longer(V1:V5) 


# Plot using ggplot
ggplot(long_mean, aes(x = log_number, y = value, color = name)) +
  geom_line() +
  labs(title = "Cumulative Sums of Numerical Values",
       x = "Row Index",
       y = "Cumulative Sum") +
  theme_minimal()

#Käyttöasteista:
#Tallennetaan jokaisen simulaatioiteraation jokaisen tukki-iteraation käyttöaste
#Plotataan (r, käyttöaste)
thick <- 48
ws <- c(21, 48, 73, 125, 150)

test <- calc_all_utils(thick, ws, production_obj_2_orderA, radius_obj_2_orderA)

plot(test[[1]], test[[2]])

#Käytettyjen tukkien jakauma:
mean(number_of_logs_obj_1_orderA)
mean(number_of_logs_obj_2_orderA)
mean(number_of_logs_obj_3_orderA)

hist(number_of_logs_obj_1_orderA, breaks=10, main="Histogram of Data", xlab="Values", ylab="Frequency", col="lightblue", border="black")
