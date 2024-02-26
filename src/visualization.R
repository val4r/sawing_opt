library(here)
library("assertthat")
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(scales)

source(here("src/functions.R"))

path1 <- paste0(here(), "/data/")

number_of_logs_obj_1_orderA <- readRDS(paste0(path1, "n_log_all_iters_A_1_1000.rds"))
number_of_logs_obj_2_orderA <- readRDS(paste0(path1, "n_log_all_iters_A_2_1000.rds"))
number_of_logs_obj_3_orderA <- readRDS(paste0(path1, "n_log_all_iters_A_3_1000.rds"))
number_of_logs_obj_1_orderB <- readRDS(paste0(path1, "n_log_all_iters_B_1_1000.rds"))
number_of_logs_obj_2_orderB <- readRDS(paste0(path1, "n_log_all_iters_B_2_1000.rds"))
number_of_logs_obj_3_orderB <- readRDS(paste0(path1, "n_log_all_iters_B_3_1000.rds"))

radius_obj_1_orderA <- readRDS(paste0(path1, "r_all_iters_A_1_1000.rds"))
radius_obj_2_orderA <- readRDS(paste0(path1, "r_all_iters_A_2_1000.rds"))
radius_obj_3_orderA <- readRDS(paste0(path1, "r_all_iters_A_3_1000.rds"))
radius_obj_1_orderB <- readRDS(paste0(path1, "r_all_iters_B_1_1000.rds"))
radius_obj_2_orderB <- readRDS(paste0(path1, "r_all_iters_B_2_1000.rds"))
radius_obj_3_orderB <- readRDS(paste0(path1, "r_all_iters_B_3_1000.rds"))

production_obj_1_orderA <- readRDS(paste0(path1, "prod_all_iters_A_1_1000.rds"))
production_obj_2_orderA <- readRDS(paste0(path1, "prod_all_iters_A_2_1000.rds"))
production_obj_3_orderA <- readRDS(paste0(path1, "prod_all_iters_A_3_1000.rds"))
production_obj_1_orderB <- readRDS(paste0(path1, "prod_all_iters_B_1_1000.rds"))
production_obj_2_orderB <- readRDS(paste0(path1, "prod_all_iters_B_2_1000.rds"))
production_obj_3_orderB <- readRDS(paste0(path1, "prod_all_iters_B_3_1000.rds"))


#lasketaan keskiarvot simulaatioiteraatioiden yli


mean_df <- simulation_average_prod(production_obj_3_orderB)
colnames(mean_df) <- gsub("V", "Tuote ", colnames(mean_df))

long_mean <- mean_df %>% 
  pivot_longer(2:6) 


# plotataan kumulatiiviset tuotannot
# orderbook <- c(500, 370, 320, 270, 225)
hex <- hue_pal()(5)
orderbook <- c(125, 270, 350, 410, 500)
ggplot(long_mean, aes(x = log_number, y = value)) +
  geom_line(aes(color = name), size = 1) +
  labs(title = "Tuotteiden keskimääräinen kumulatiivinen tuotanto sahattujen tukkien funktiona. Kysynnät esitetty katkoviivalla.",
       x = "Tukin järjestysluku",
       y = "Kumulatiivinen tuotanto",
       color = "Tuote") +
  theme_minimal() +
  geom_hline(yintercept = orderbook,
                 linetype = "dashed", color = hex, size = 0.8)

#Käyttöasteista:
#Tallennetaan jokaisen simulaatioiteraation jokaisen tukki-iteraation käyttöaste
#Plotataan (r, käyttöaste)
thick <- 48
ws <- c(21, 48, 73, 125, 150)

test <- calc_all_utils(thick, ws, production_obj_1_orderB, radius_obj_1_orderB)
test_df <- as.data.frame(test) %>% 
  rename("r" = 1,
         "util_rate" = 2,
         "idx" = 3) 

ggplot(test_df, aes(x=r, y=util_rate, color=idx)) + 
  geom_point() + 
  scale_color_gradient(low="blue", high="red") +
  labs(title = "Sahattujen tukkien hyötyaste",
     x = "Tukin säde",
     y = "Tukin hyötyaste",
     color = "Tukin järjestysluku")

# plot( test_df$idx, test_df$util_rate)

#Käytettyjen tukkien jakauma:
mean(number_of_logs_obj_1_orderA)
mean(number_of_logs_obj_2_orderA)
mean(number_of_logs_obj_3_orderA)
mean(number_of_logs_obj_1_orderB)
mean(number_of_logs_obj_2_orderB)
mean(number_of_logs_obj_3_orderB)

hist(number_of_logs_obj_2_orderA, breaks=10, main="Histogram of Data", xlab="Values", ylab="Frequency", col="lightblue", border="black")
