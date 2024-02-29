library("assertthat")

#tarpeelliset funktiot


#Funktio joka palauttaa tukin sahausetäisyydet tukin keskipisteestä
#Annettuna:
  #Tukin säde
  #Tuppeensahatun lankkujen leveys
#Palauttaa:
  #list of tuples (a,b), kts. get_flitch_width
get_flitch_saw_points <- function(r, flitch_thick) {
  nr_flitchs <- (floor(r/flitch_thick)) #tuppeensahattujen lankkujen määrä per puoli,
                                        # sahaus aloitetaan keskeltä
  
  a_left_side <- seq(from = -flitch_thick, by = -flitch_thick, length.out = nr_flitchs) 
  a_right_side <- seq(from = 0, by = flitch_thick, length.out = nr_flitchs)
  a <- c(a_left_side, a_right_side)
  
  b_left_side <- seq(from = 0, by = -flitch_thick, length.out = nr_flitchs)
  b_right_side <- seq(from = flitch_thick, by = flitch_thick, length.out = nr_flitchs)
  b <- c(b_left_side, b_right_side)
  
  list_of_saw_points <- mapply(c, a, b, SIMPLIFY = FALSE)
  return (list_of_saw_points)
}

#funktio joka palauttaa läpisahatun lankun optimaalisen sahauksen annetulla arvostuksella
#arvostukset: 
  #1: max puutavaran läpileikkaus pinta-ala, i.e tässä hinta = leveys, 
  #2: max puutavaran hinta
  #3. max puutavaran dynaaminen hinta
#Annettuna:
  #Lankun leveys (kokonaisluku); 
  #eri puutavaroiden leveydet (kokonaislukuvektori);
  #edellisten arvot (kokonaislukuvektori);
#Palauttaa:
  #leikattujen tavaroiden leveydet
cut_flitch <- function(flitch_width, tmbr_widths, 
                       tmbr_values) {
  m <- length(tmbr_widths) #number of items
  
  assert_that(flitch_width >= max(tmbr_widths)) #leveimmän kappaleen täytyy olla kapeampi kuin annetun lankun
  
  #Initializing
  opt_val <- matrix(NA, nrow = m, ncol = flitch_width + 1) #F(m, y), optimum tmbr_values
  index_info <- numeric(flitch_width) #from 1 to tmbr_widths[1]-1 elements are zero
  index_info[tmbr_widths[1]:flitch_width] <- 1 #from tmbr_widths[1] to flitch_width elements are 1
  
  for (y in 0:flitch_width) {
    opt_val[1, y+1] <- tmbr_values[1]*floor(y/tmbr_widths[1])
  }
  
  #Main loops
  for (k in 2:m) {
    
    for (y1 in 0:(tmbr_widths[k])) {
      opt_val[k, y1+1] <- opt_val[k-1,y1+1]
    }
    
    for (y2 in (tmbr_widths[k]+1):(flitch_width+1)) {
      if(opt_val[k-1, y2] < opt_val[k, y2-tmbr_widths[k]] + tmbr_values[k]) {
        opt_val[k,y2] <- opt_val[k, y2-tmbr_widths[k]] + tmbr_values[k]
        index_info[y2] <- k
      } else {
        opt_val[k,y2] <- opt_val[k-1,y2]
      }
    }
  }

  #obtaining optimal solution
  x_opt <- numeric(m)
  y_opt <- flitch_width
  
  while(index_info[y_opt] > 0) {
    idx <- index_info[y_opt]
    x_opt[idx] <- x_opt[idx] + 1
    y_opt <- (y_opt - tmbr_widths[idx])
    if(y_opt == 0) { #viimeisellä iteraatiolla 0, rikkoo indeksöinnin
      break
    }
  }
  
  return(x_opt)
  
}


#funktio joka palauttaa läpisahatun lankun lyhyemmän sivun leveyden
#Annettuna:
  #Tukin säde; 
  #lankun sahausetäisyydet tukin keskipisteestä a & b, a<b.
get_flitch_width <- function(r, a, b) {
  assert_that(a < b)
  assert_that(2*r > a && 2*r > b)
  if(b <= 0) {
    return (floor(2*sqrt(r^2-abs(a)^2)))
  } else if(a < 0 && b > 0) {
    return (floor(min(2*sqrt(r^2-abs(a)^2), 2*sqrt(r^2-(b)^2))))
  } else if(a >= 0){
    return (floor(2*sqrt(r^2-(b)^2)))
  }
}

#funktio palauttaa puutavaran dynaamisen hinnan:
#Annettuna:
  #alkuperäinen hinta; kysyntä puutavaralle; tuotettu määrä 
create_dynamic_tmbr_prices <- function(orig_price, demand, prod) {
  return(max(0,orig_price*(1 - prod/demand)))
}

#funktio palauttaa indeksit tuotteista joilla kysyntä täynnä
#Annettuna: 
  #tuotettu määrä (vektori); kysynnät (vektori);
full_prod_idx <- function(prod, demand) {
  return(which(prod >= demand))
}

#apufunktio joka palauttaa indeksit tuotteista jotka EIVÄT ole annettua lankkua leveämpiä
#Indekseillä muodostetaan uudet hinta- ja painovektorit
#Annettuna:
  #alkuperäinen puutavara-leveyslista;
  #lankun leveys
#Palauttaa:
  #indeksit
fit_width_idx <- function(tmbr_width_orig, flitch_width) {
  return(which(tmbr_width_orig <= flitch_width))
}


#funktio joka laskee tukin käyttöasteen
#Annettuna:
  #tukin säde r, tuotteen paksuus width, (lista) leveydet, (lista) lukumäärä per tuote
calc_utilization <- function(r, thickness, widths, prod) {
  log_area <- pi*r^2
  prod_area <- (thickness * widths) %*% prod
  return(as.numeric(prod_area / log_area))
}

#funktio joka laskee keskimääräisen tuotannon jokaisella tukki-iteraatiolla.
#eli montako tuotetta i valmistettiin tukista j keskimäärin N:llä eri simulaatioiteraatiolla
#Annettuna:
  #tuotanto-list of lists joka tallennetaan simulation.R-tiedoston lopussa.
  #listan elementteinä on eri simulaatioiteraatioiden tuotannot. 
  #Nämä elementit itsessään ovat listoja vektoreista. 
  #Vektorit sisältävät jokaisen tuotteen valmistusmäärät tukin j kohdalla.
simulation_average_prod <- function(prod_list_of_lists) {
  max_log <- max(sapply(prod_list_of_lists, length)) #eri simulaatioiteraatioiden maksimi tukkimäärä
  
  for (i in seq(length(prod_list_of_lists))) {
    nam <- paste("prod_",i, sep = "")
    assign(nam, prod_list_of_lists[[i]])
    
    nam_df <- paste("prod_df_",i, sep = "")
    assign(nam_df, as.data.frame(do.call(rbind, get(nam))))
    #muutetaan jokainen df sisältämään yhtä monta riviä
    while(nrow(get(nam_df))<max_log) {
      assign(nam_df, rbind(get(nam_df),0))
    }
    
    #kumulatiiviset summat tukki-iteraatioiden yli
    nam_cum_df <- paste("cumulative_prod", i, sep = "")
    assign(nam_cum_df, as.data.frame(apply(get(nam_df), 2, cumsum)) %>% 
             mutate(log_number = row_number()))
  }
  
  #muodostetaan lista kaikista dataframeista
  df_list <- mget(ls(pattern = "^cumulative_prod\\d+"))
  #lasketaan jokaisen elementin keskiarvo dataframejen yli
  mean_df <- as.data.frame(rbindlist(df_list)[,lapply(.SD,mean), list(log_number)])
  
  return(mean_df)
}

#funktio joka laskee käyttöasteet jokaiselle tukille jokaisessa simulaatio-
#iteraatiossa. palauttaa ne yhtenä listana
calc_all_utils <- function(thick, widths, prod_list, radius_list) {
  N <- length(prod_list)
  
  util_all <- list()
  r_all <- list()
  index_all <- numeric()
  
  for (j in seq(N)) {
    ans <- numeric(length(prod_list[[j]]))
    for (i in seq(length(prod_list[[j]]))) {
      prod <- prod_list[[j]][[i]]
      rad <- radius_list[[j]][[i]]
      ans[i] <- calc_utilization(rad, thick, widths, prod) 
    }
    r_all[[j]] <- radius_list[[j]]
    util_all[[j]] <- ans
    index_all <- append(index_all, seq(length(prod_list[[j]])))
  }
  
  util_all_flatten <- unlist(util_all)
  r_all_flatten <- unlist(r_all)  
  return(list(r_all_flatten, util_all_flatten, index_all))
}

#funktio joka palauttaa yhdellä simulaatiokierroksella käytetyn raaka-aineen hinnan
#Annettuna:
  #tukin hinta per m^3
  #sim. kierroksen aikana sahattujen tukkien säteet listana
calc_costs <- function(log_price, radius_list) {
  area_list <- mapply(function(x) (1e-06)*pi*x^2, radius_list)
  total_cost <- sum(area_list*log_price)
  return(total_cost)
}



