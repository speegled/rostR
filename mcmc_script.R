#install.packages("mcmc")
library(mcmc)

#'
#'
#' In this script, we implement MCMC simple substitution cipher decoding. 
#'
#'

mcmc_decode <- function(encoded_text, scoring_function, num_iter = 1000, scale = 1.2, current_cipher = "abcdefghijklmnopqrstuvwxyz",  ...) {
  
  
  alphabet <- "abcdefghijklmnopqrstuvwxyz"
  
  decoded_text <- chartr(old = alphabet, new = current_cipher, x = encoded_text)
  browser()
  l1 <- scoring_function(decoded_text, bigraphs)
  probs <- l1
  for(i in 1:num_iter) {
    c1 <- stringi::stri_rand_strings(n = 1, length = 1, pattern = "[a-z]")
    c2 <- stringi::stri_rand_strings(n = 1, length = 1, pattern = "[a-z]")
    if(str_locate(current_cipher, c1)[1] < str_locate(current_cipher, c2)[1]) {
      temp <- c1
      c1 <- c2
      c2 <- temp
    }
    current_cipher_proposed <- str_replace(current_cipher, c1, c2)
    current_cipher_proposed <- str_replace(current_cipher_proposed, c2, c1)
    decoded_text_proposed <- chartr(old = alphabet, new = current_cipher_proposed, x = encoded_text )
    l2 <- likely_text(decoded_text_proposed, bigraphs)
    if(runif(1) < scale^(l2 - l1)) {
      l1 <- l2
      current_cipher <- current_cipher_proposed
      decoded_text <- decoded_text_proposed
      probs <- c(probs, l2)
    }
  }
  list(dtext = decoded_text, probs = probs, current_cipher = current_cipher)
  
}