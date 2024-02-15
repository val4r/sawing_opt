library(here)
library("assertthat")
source(here("src/functions.R"))

N_iter <- 1;

log_r_low <- 150
log_r_high <- 300

timber_thickness <- 48; #puutavaran paksuus. Nyt käsitellään vain yhtä paksuutta
timber_widths <- c(21, 48, 73, 125, 150); #eri leveydet. Vastaavat likimäärin yleisimpiä tuotteita
m = length(timber_widths);
timber_prices <- c(0.5, 1.25, 1.89, 3.25, 3.95); #puutavaran markkinahinnat
assert_that(m == length(timber_prices))

orderbook <- c(100, 100, 100, 100, 100); #puutavaroiden kysynnät
assert_that(length(orderbook) == m);

objective <- 1; #mitä maksimoidaan (c_i): 1 = markkinahinta-tuotto, 2 = tuotetun tavaran tilavuutta, 3 = dynaaminen hinta-tuotto

for (i in seq(N_iter)) {
  prod <- numeric(length(orderbook)) #tuotettu puutavara kpl
  dynamic_price <- mapply(create_dynamic_tmbr_prices, timber_prices, orderbook, prod) #lasketaan jokaiselle puutavaralla dynaaminen hinta
  N_log <- 0 #käytetyt tukit
  #otetaan uusia tukkeja niin kauan kunnes jokaisen puutavaraluokan kohdalla on tuotanto > kysyntä
  while(sum(orderbook <= prod) < m) {
    r <- runif(1, log_r_low, log_r_high)
    saw_points <- get_flitch_saw_points(r, timber_thickness) #tukin läpisahauspisteet
    
    #loopataan seuraavaksi kaikki tukista syntyvät lankut ja luodaan niistä sahatavaraa:
    for(flitch in saw_points) {
      flitch_width <- get_flitch_width(r, flitch[1], flitch[2]) #lasketaan lankun lyhyemmän sivun leveys
      timber_idx <- fit_width_idx(timber_widths, flitch_width) #indeksit tuotteista joiden leveys ei ylitä särmätyn lankun leveyttä
      full_prod <- full_prod_idx(prod, orderbook) #indeksit tuotteista joilla kysyntä täysi. Asetetaan niiden arvo nollaan seuraavassa if-elsesssä

      #käsitellään vain leveyksiä alle lankun leveyden
      #lisäksi asetetaan hinta c_i tapauskohtaisesti
      #TODO: aseta nollaan täyden tuotannon tavarat
      timber_widths_new <- timber_widths[timber_idx]
      if(objective == 1) {
        price <- timber_prices[timber_idx]
      } else if(objective == 2) {
        price <- timber_widths[timber_idx] #c_i = w_i
      } else if(objective == 3) {
        price <- dynamic_price[timber_idx]
      } else {
        print("virhe, valitse arvostus")
      }
      
      #Seuraavaksi leikataan lankku puutavaraksi. 
      #cut_flitch-funktion algoritmi vaatii useampaa kuin yhtä tuotetta, siksi seuraava if-else:
      if(length(timber_idx) == 0) {
        break
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
  }
}



#Mitä tarvitaan ulos?
#Käytettyjen tukkien määrä
#Sahattujen tuotteiden määrä jokaisella tukki-iteraatiolla per tuote
#Dynaamisessa hinnassa voisi ottaa ulos myös tuotteiden arvot per iteraatio

#BONUS:
#jokaisella iteraatiolla käytettyjen sahausten määrä
#jokaisella iteraatiolla hyödynnetyn pinta-alan osuus





