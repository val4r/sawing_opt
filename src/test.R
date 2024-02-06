unboundedKnapsack <- function(weights, values, capacity) {
  m <- length(weights) #number of items
  
  #Initializing
  opt_val <- matrix(NA, nrow = m, ncol = capacity + 1) #F(m, y), optimum values
  index_info <- numeric(capacity) #from 1 to weights[1]-1 elements are zero
  index_info[weights[1]:capacity] <- 1 #from weights[1] to capacity elements are 1
  
  for (y in 0:capacity) {
    opt_val[1, y+1] <- values[1]*floor(y/weights[1])
  }
  
  #Main loops
  for (k in 2:m) {
    
    for (y1 in 0:(weights[k])) {
      opt_val[k, y1+1] <- opt_val[k-1,y1+1]
    }
    
    for (y2 in (weights[k]+1):(capacity+1)) {
      if(opt_val[k-1, y2] < opt_val[k, y2-weights[k]] + values[k]) {
        opt_val[k,y2] <- opt_val[k, y2-weights[k]] + values[k]
        index_info[y2] <- k
      } else {
        opt_val[k,y2] <- opt_val[k-1,y2]
      }
    }
  }
  
  #obtaining optimal solution
  x_opt <- numeric(m)
  y_opt <- capacity+1
  
  while(index_info[y_opt] > 0) {
    idx <- index_info[y_opt]
    x_opt[idx] <- x_opt[idx] + 1
    y_opt <- y_opt - weights[idx]
  }
  
  return(x_opt)
}



# Example usage
weights <- c(4, 7, 9, 5)
values <- c(5, 10, 12, 6)
capacity <- 15

result <- unboundedKnapsack(weights, values, capacity)
cat("Maximum Value:", result$maxValue, "\n")
cat("Selected Items:", result$selectedItems, "\n")