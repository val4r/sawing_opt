library(here)
library("assertthat")
source(here("src/functions.R"))

#VAKIOT
N_iter <- 3;
log_r_low <- 150;
log_r_high <- 350;
timber_thickness <- 48; #puutavaran paksuus. Nyt käsitellään vain yhtä paksuutta
timber_widths <- c(21, 48, 73, 125, 150); #eri leveydet. Vastaavat likimäärin yleisimpiä tuotteita
m = length(timber_widths);
timber_prices <- c(0.65, 1.25, 1.89, 3.25, 3.95); #puutavaran markkinahinnat
assert_that(m == length(timber_prices))
orderbook <- c(500, 500, 500, 500, 500); #puutavaroiden kysynnät
assert_that(length(orderbook) == m);

#TALLENNETTAVA DATA:
n_log_all_iters <- numeric(N_iter) #eri simulaatioiteraatioiden käytetyt tukit, dimensio 1 x N_iter 
r_all_iters <- c() #eri sim.iteraatioiden eri tukkien säteet, dimensio N_iter x N_log
prod_all_iters <- c() #eri sim.iteraatioiden eri tukki-iteraatioiden tuotantomäärät, dimensio N_iter X N_log X m

objective <- 3; #mitä maksimoidaan (c_i): 1 = markkinahinta-tuotto, 2 = tuotetun tavaran tilavuutta, 3 = dynaaminen hinta-tuotto

for (i in seq(N_iter)) {
  prod <- numeric(length(orderbook)) #tuotettu puutavara kpl
  prod_list <- list() #list of lists, jokaisen tukin kohdalla tuotettu tavara kpl
  dynamic_price <- mapply(create_dynamic_tmbr_prices, timber_prices, orderbook, prod) #lasketaan jokaiselle puutavaralla dynaaminen hinta
  N_log <- 0 #käytetyt tukit
  r_list <- list()
  #otetaan uusia tukkeja niin kauan kunnes jokaisen puutavaraluokan kohdalla on tuotanto > kysyntä
  k <- 1;
  while(sum(orderbook <= prod) < m) {
    r <- runif(1, log_r_low, log_r_high)
    r_list <- append(r_list, r)
    
    prod_per_log <- prod #alustetaan edellisen iteraation tuotantomäärällä
    
    saw_points <- get_flitch_saw_points(r, timber_thickness) #tukin läpisahauspisteet
    
    
    full_prod <- full_prod_idx(prod, orderbook) #indeksit tuotteista joilla kysyntä täysi
    timber_prices_new <- timber_prices
    timber_prices_new[full_prod] <- 0 #asetetaan kysynnän täyttäneiden tuotteiden hinnat nollaan
    
    width_cost_coeff <- timber_widths
    width_cost_coeff[full_prod] <- 0 #asetetaan kysynnän täyttäneiden tuotteiden "leveyshinnat" nollaan, kts. objective 2
    
    #loopataan seuraavaksi kaikki tukista syntyvät lankut ja luodaan niistä sahatavaraa:
    for(flitch in saw_points) {
      flitch_width <- get_flitch_width(r, flitch[1], flitch[2]) #lasketaan lankun lyhyemmän sivun leveys
      
      timber_idx <- fit_width_idx(timber_widths, flitch_width) #indeksit tuotteista joiden leveys ei ylitä särmätyn lankun leveyttä
      
      #käsitellään vain leveyksiä alle lankun leveyden
      timber_widths_new <- timber_widths[timber_idx]
      
      #asetetaan hinta c_i tapauskohtaisesti
      
      if(objective == 1) {
        price <- timber_prices_new[timber_idx]
      } else if(objective == 2) {
        price <- width_cost_coeff[timber_idx] #c_i = w_i
      } else if(objective == 3) {
        price <- dynamic_price[timber_idx]
      } else {
        print("virhe, valitse arvostus")
      }
      
      #Seuraavaksi leikataan lankku puutavaraksi. 
      #cut_flitch-funktion algoritmi vaatii useampaa kuin yhtä tuotetta, siksi seuraava if-else:
      if(length(timber_idx) == 0) {
        break #mikäli särmätty lankku on kapeampi kuin kapein tuote, ei synny tuotetta
      } else if(length(timber_idx) == 1) {
        prod[timber_idx[1]] <- prod[timber_idx[1]] + 1 #mikäli tuotteet on järjestetty kapeimmasta leveimpään, timber_idx[1] = 1
      } else {
        opt_timber_pieces <- cut_flitch(flitch_width, timber_widths_new, price) #lankusta leikattavien kappaleiden lukumäärät-vektori
        #päivitetään prod-vektori
        for(j in seq_along(timber_idx)) {
          idx <- timber_idx[j] #alkuperäisen listan indeksi joka vastaa leikatun listan
          prod[idx] <- prod[idx] + opt_timber_pieces[j] 
        }
      }
    }
    
    #päivitetään dynamic_price & N_log
    N_log <- N_log + 1
    dynamic_price <- mapply(create_dynamic_tmbr_prices, timber_prices, orderbook, prod)
    #tallennetaan tuotetut kappaleet
    prod_per_log <- prod - prod_per_log #tuotanto tukilla i = kumulatiivinen tuotanto tukilla i - kumulatiivinen tuotanto tukilla (i-1)
    prod_list[[k]] <- prod_per_log
    k <- k + 1;
  }
  #tallennetaan
  n_log_all_iters[i] <- N_log
  r_all_iters[[i]] <- r_list
  prod_all_iters[[i]] <- prod_list
}

#mean(n_log_all_iters)

#Tallennetaan
path1 <- paste0(here(), "/data/")
#Käytettyjen tukkien määrä
saveRDS(n_log_all_iters, paste0(path1,"n_log_all_iters_1.rds"))
#Tukkien säde
saveRDS(r_all_iters, paste0(path1, "r_all_iters_1.rds"))
#Sahattujen tuotteiden määrä jokaisella tukki-iteraatiolla per tuote
saveRDS(prod_all_iters, paste0(path1, "prod_all_iters_1.rds"))








