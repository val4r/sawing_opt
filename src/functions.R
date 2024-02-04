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

#funktio joka palauttaa läpisahatun lankun optimaalisen sahauksen annetulla tavoitefunktiolla
#tavoitefunktiot: 
  #1: max puutavaran läpileikkaus pinta-ala, i.e tässä hinta = leveys, 
  #2: max puutavaran hinta
  #3. max puutavaran dynaaminen hinta
#Annettuna:
  #Lankun leveys; 
  #eri puutavaroiden leveydet (int);
  #edellisten arvot (huom arvo riippuu tavoitteesta)
#Palauttaa:
  #leikattujen tavaroiden leveydet
cut_flitch <- function(flitch_width, tmbr_widths, 
                       tmbr_values) {
  # function unboundedKnapsack(weights, values, capacity):
  #   n = length(weights)
  #   dp = array of size (capacity + 1) initialized to 0
  #   
  #   for w from 1 to capacity:
  #     for i from 0 to n - 1:
  #     if weights[i] <= w:
  #       dp[w] = max(dp[w], dp[w - weights[i]] + values[i])
  #   
  #   return dp[capacity]
  
  #This is bottom-up dynamic programming approach
  n <- length(tmbr_widths)
  
  dp <- numeric(flitch_width + 1)
  
}


#funktio joka palauttaa läpisahatun lankun lyhyemmän sivun leveyden
#Annettuna:
  #Tukin säde; 
  #lankun sahausetäisyydet tukin keskipisteestä a & b, a<b.
get_flitch_width <- function(r, a, b) {
  assert_that(a < b)
  assert_that(2*r > a && 2*r > b)
  if(b <= 0) {
    return (2*sqrt(r^2-abs(a)^2))
  } else if(a < 0 && b > 0) {
    return (min(2*sqrt(r^2-abs(a)^2), 2*sqrt(r^2-(b)^2)))
  } else if(a >= 0){
    return (2*sqrt(r^2-(b)^2))
  }
}

#funktio palauttaa puutavaran dynaamisen hinnan:
#Annettuna:
#alkuperäinen hinta; kysyntä puutavaralle; tuotettu määrä 
create_dynamic_tmbr_prices <- function(orig_price, demand, prod) {
  return(max(0,orig_price*(1 - prod/demand)))
}



#funktio joka muodostaa plotin joka visualisoi tukin leikkauksen
#Argumentit:
#tukin säde (numeric); läpisahausetäisyydet keskipisteestä (list of numerics)

visualize_log_cut <- function(log_diam, saw_points) {
  #geom_rect + geom_point
}

#funktio joka muodostaa plotin joka visualisoi tuppeensahatun lankun leikkauksen
#Argumentit:
#lankun pitkä sivu (numeric); lankun lyhyt sivu (numeric), sahauspisteet (list of numerics)
visualize_flitch_cut <- function(long_side, short_side, saw_points) {
  #geom_rect + geom_point
}



