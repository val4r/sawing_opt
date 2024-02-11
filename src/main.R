# {Initialization}
# For each quality class (grade)
# 
# Enter customer demand (grade) {Enter Order-
#     book}
# 
# Initialize cost coefficients (grade)
# 
# Initialize Timber Production (grade) {set produc-
#     tion to zero}
# 
# While Orderbook not satisfied
# Saw a log into flitches/cant
# For each flitch/cant
# Optimize edging, trimming & grading
# 
# {using DP recursion with dynamic cost coeffi-
#     cients to produce graded boards, Equation 1}
# 
# For each graded board
# Update Timber Production (board grade, board
#                           olume)
# Update Orderbook (board grade, board olume)
# For each quality class (grade)
# Generate new cost coefficients based on updated
# orderbook
#Pseudokoodi Todoroki & Rönnqvist
library(here)
source(here("src/functions.R"))

timber_thickness <- 50 #puutavaran paksuus. Nyt käsitellään vain yhtä paksuutta
timber_widths <- c(25, 50, 100, 150) #eri leveydet. Vastaavat likimäärin yleisimpiä tuotteita
log_r <- 200 #tukin säde senttimetreissä

demand_A <- c() 
demand_B <- c()


#Mitä tarvitaan ulos?
#Käytettyjen tukkien määrä
#Sahattujen tuotteiden määrä jokaisella tukki-iteraatiolla per tuote
#Dynaamisessa hinnassa voisi ottaa ulos myös tuotteiden arvot per iteraatio

#BONUS:
#jokaisella iteraatiolla käytettyjen sahausten määrä
#jokaisella iteraatiolla hyödynnetyn pinta-alan osuus



