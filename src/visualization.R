library(here)
library("assertthat")
library(dplyr)
library(ggplot2)
library(tidyr)

source(here("src/functions.R"))

path1 <- paste0(here(), "/data/")

number_of_logs <- readRDS(paste0(path1, "n_log_all_iters_1.rds"))

radius <- readRDS(paste0(path1, "r_all_iters_1.rds"))

production <- readRDS(paste0(path1, "prod_all_iters_1.rds"))

#lasketaan keskiarvot simulaatioiteraatioiden yli

#testikase

max_log <- max(sapply(production, length)) #eri simulaatioiteraatioiden maksimi tukkimäärä

for (i in seq(length(production))) {
  nam <- paste("prod_",i, sep = "")
  assign(nam, production[[i]])
  
  nam_df <- paste("prod_df_",i, sep = "")
  assign(nam_df, as.data.frame(do.call(rbind, get(nam))))
  #muutetaan jokainen df sisältämään yhtä monta riviä
  while(nrow(get(nam_df))<max_log) {
    assign(nam_df, rbind(get(nam_df),0))
  }
}



cumulative_prod_1 <- as.data.frame(apply(prod_1_df, 2, cumsum)) %>% 
  mutate(log_number = row_number()) %>% 
  pivot_longer(V1:V5) 


# Plot using ggplot
ggplot(cumulative_prod_1, aes(x = log_number, y = value, color = name)) +
  geom_line() +
  labs(title = "Cumulative Sums of Numerical Values",
       x = "Row Index",
       y = "Cumulative Sum") +
  theme_minimal()

