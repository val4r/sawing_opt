library(here)
library("assertthat")
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)

source(here("src/functions.R"))

path1 <- paste0(here(), "/data/")

number_of_logs <- readRDS(paste0(path1, "n_log_all_iters_2.rds"))

radius <- readRDS(paste0(path1, "r_all_iters_2.rds"))

production <- readRDS(paste0(path1, "prod_all_iters_2.rds"))

#lasketaan keskiarvot simulaatioiteraatioiden yli

#testikase

mean_df <- simulation_average_prod(production)

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

thick <- 48
ws <- c(21, 48, 73, 125, 150)
test_prod <- production[[1]][[50]]
test_rad <- radius[[1]][[50]]
calc_utilization(test_rad, thick, ws, test_prod) 

ans <- numeric(length(production[[1]]))

for (i in seq(length(production[[1]]))) {
  test_prod <- production[[1]][[i]]
  test_rad <- radius[[1]][[i]]
  ans[i] <- calc_utilization(test_rad, thick, ws, test_prod) 
}
mean(ans)


#Käytettyjen tukkien jakauma:


hist(number_of_logs, breaks=10, main="Histogram of Data", xlab="Values", ylab="Frequency", col="lightblue", border="black")
