unboundedKnapsack <- function(weights, values, capacity) {
  n <- length(weights)
  dp <- numeric(capacity + 1)
  selectedItems <- numeric(capacity + 1)
  
  for (w in 1:capacity) {
    for (i in 1:n) {
      if (weights[i] <= w) {
        if (dp[w - weights[i]] + values[i] > dp[w]) {
          dp[w] <- dp[w - weights[i]] + values[i]
          selectedItems[w] <- i
        }
      }
    }
  }
  
  # Reconstruct the selected items
  selectedItemsList <- integer()
  w <- capacity
  while (w > 0 & selectedItems[w] > 0) {
    selectedItemsList <- c(selectedItemsList, selectedItems[w])
    w <- w - weights[selectedItems[w]]
  }
  
  result <- list(maxValue = dp[capacity], selectedItems = selectedItemsList)
  return(result)
}

# Example usage
weights <- c(2, 1, 3, 2)
values <- c(12, 10, 20, 15)
capacity <- 5

result <- unboundedKnapsack(weights, values, capacity)
cat("Maximum Value:", result$maxValue, "\n")
cat("Selected Items:", result$selectedItems, "\n")