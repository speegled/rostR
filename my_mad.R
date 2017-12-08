my_mad <- function(x, center, expon = 1) {
  if(missing(center)) center <- mean(x)
  mean(abs(x - center)^expon, na.rm = TRUE)
}
