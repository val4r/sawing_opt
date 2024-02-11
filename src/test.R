library(here)
source(here("src/functions.R"))


prices <- c(52, 120, 123, 612);
weights <- c(42, 73, 91, 57);

capacity <- 901;

cut_flitch(capacity, weights, prices)

r <- 250
thick <- 50
abs <- get_flitch_saw_points(r, thick)

for (points in abs) {
  width <- get_flitch_width(r, points[1], points[2])
  print(width)
}
