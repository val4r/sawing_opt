library(here)
library("assertthat")
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(scales)
library(reshape2)
library(plotly)
library(patchwork)
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

util_list <- calc_all_utils(thick, ws, production_obj_3_orderB, radius_obj_3_orderB)
util_df <- as.data.frame(util_list) %>% 
  rename("r" = 1,
         "util_rate" = 2,
         "idx" = 3)

#otetaan näyte koko datasta ettei mene ihan puuroksi
sample_util_df <- util_df[sample(nrow(util_df), 3000), ]
  

p_3_B <- ggplot(sample_util_df, aes(x=r, y=util_rate, color=idx)) + 
          geom_point() + 
          scale_color_gradient(low="yellow", high="blue") +
          labs(
             x = "Tukin säde (mm)",
             y = "Tukin hyötyaste",
             color = "Tukin järjestysluku")

(p_1_A | p_1_B) /
(p_2_A | p_2_B) /
(p_3_A | p_3_B) + 
  plot_annotation(title = "Tukin hyötyaste säteen ja järjestysluvun funktiona",
                  tag_levels = 'A')



# 3D plotteja
#
# plot_ly(sample_util_df, z = ~util_rate, x = ~r, y = ~idx) %>%
#   layout(scene = list(xaxis = list(title = "X-axis"),
#                       yaxis = list(title = "Y-axis"),
#                       zaxis = list(title = "Z-axis")))%>%
#   add_surface(x = ~sample_util_df$r, y = ~sample_util_df$idx, z = ~sample_util_df$util_rate)
# 
# 
# library(mgcv)
# mod <- gam(util_rate ~ te(r) + te(idx) + ti(r, idx), data=sample_util_df)
# 
# r.seq <- seq(min(sample_util_df$r, na.rm=TRUE), max(sample_util_df$r, na.rm=TRUE), length=25)
# idx.seq <- seq(min(sample_util_df$idx, na.rm=TRUE), max(sample_util_df$idx, na.rm=TRUE), length=25)
# 
# predfun <- function(x,y){
#   newdat <- data.frame(r = x, idx=y)
#   predict(mod, newdata=newdat)
# }
# 
# fit <- outer(r.seq, idx.seq, Vectorize(predfun))
# 
# plot_ly() %>% 
#   # add_markers(x = ~sample_util_df$r, y=sample_util_df$idx, z=sample_util_df$util_rate) %>% 
#   add_surface(x = ~r.seq, y = ~idx.seq, z = t(fit))


#Käytettyjen tukkien jakauma:
mean(number_of_logs_obj_1_orderA)
mean(number_of_logs_obj_2_orderA)
mean(number_of_logs_obj_3_orderA)
mean(number_of_logs_obj_1_orderB)
mean(number_of_logs_obj_2_orderB)
mean(number_of_logs_obj_3_orderB)

sd(number_of_logs_obj_1_orderA)
sd(number_of_logs_obj_2_orderA)
sd(number_of_logs_obj_3_orderA)
sd(number_of_logs_obj_1_orderB)
sd(number_of_logs_obj_2_orderB)
sd(number_of_logs_obj_3_orderB)

number_of_logs_df <- data.frame(number_of_logs_obj_1_orderA,   
                                number_of_logs_obj_2_orderA,
                                number_of_logs_obj_3_orderA,
                                number_of_logs_obj_1_orderB,
                                number_of_logs_obj_2_orderB,
                                number_of_logs_obj_3_orderB) %>% 
  rename("Tavoite 1, tilauskirja 1" = 1,
         "Tavoite 2, tilauskirja 1" = 2,
         "Tavoite 3, tilauskirja 1" = 3,
         "Tavoite 1, tilauskirja 2" = 4,
         "Tavoite 2, tilauskirja 2" = 5,
         "Tavoite 3, tilauskirja 2" = 6)

number_of_logs_df_long <- reshape2::melt(number_of_logs_df)


means <- colMeans(number_of_logs_df)
# Plotting histograms using 'ggplot2'
ggplot(number_of_logs_df_long, aes(x=value, fill=variable)) +
  geom_histogram(bins=10, alpha=0.5, position='identity') +
  facet_wrap(~variable, scales='free') +
  labs(x='Tukkien lukumäärä', y='Lukumäärä') +
  geom_vline(aes(xintercept=means[variable]), color='red', linetype='dashed', size=1) +
  ggtitle('Sahattujen tukkien lukumäärän jakauma simulaatiossa') +
  scale_x_continuous(breaks = seq(30, 70, by = 2)) +
  theme(legend.title=element_blank())



#Raaka-aineen hinta ja tilauskirjojen markkinahinnat
log_price <- 72 #tukkipuun hinta per m3
orderbook_A <- c(500, 370, 320, 270, 225)
orderbook_B <- c(125, 270, 350, 410, 500)
timber_prices <- c(0.65, 1.25, 1.89, 3.25, 3.95)

profit_A <- as.numeric(orderbook_A %*% timber_prices)
profit_B <- as.numeric(orderbook_B %*% timber_prices)

costs_1_A <- mapply(calc_costs, log_price, radius_obj_1_orderA)
costs_1_B <- mapply(calc_costs, log_price, radius_obj_1_orderB)

costs_2_A <- mapply(calc_costs, log_price, radius_obj_2_orderA)
costs_2_B <- mapply(calc_costs, log_price, radius_obj_2_orderB)

costs_3_A <- mapply(calc_costs, log_price, radius_obj_3_orderA)
costs_3_B <- mapply(calc_costs, log_price, radius_obj_3_orderB)

costs_df <- data.frame(costs_1_A,
                       costs_2_A,
                       costs_3_A,
                       costs_1_B,
                       costs_2_B,
                       costs_3_B) %>% 
  rename("Tavoite 1, tilauskirja 1" = 1,
         "Tavoite 2, tilauskirja 1" = 2,
         "Tavoite 3, tilauskirja 1" = 3,
         "Tavoite 1, tilauskirja 2" = 4,
         "Tavoite 2, tilauskirja 2" = 5,
         "Tavoite 3, tilauskirja 2" = 6)

costs_df_long <- reshape2::melt(costs_df)

cost_means <- colMeans(costs_df)
sapply(costs_df, sd)

ggplot(costs_df_long, aes(x=value, fill=variable)) +
  geom_histogram(bins=20, alpha=0.5, position='identity') +
  facet_wrap(~variable, scales='free') +
  labs(x='Hinta (€)', y='Lukumäärä') +
  geom_vline(aes(xintercept=cost_means[variable]), color='red', linetype='dashed', size=0.8) +
  # scale_x_continuous(breaks = seq(30, 70, by = 2)) +
  theme(legend.position="none")

