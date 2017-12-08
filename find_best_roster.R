find_best_roster <- function(roster, roster_long, weight_vec, myscale, score_roster, num_iter = 1000) {

  current_score <- score_roster(roster_long, roster, weight_vec, num_teams = 9)
  probs <- current_score
  Ids <- unique(roster$Id)
  for(i in 1:num_iter) {
    cboth <- sample(Ids, 2)
    c1 <- cboth[1]
    c2 <- cboth[2]
    t1 <- roster$Team[min(which(roster$Id == c1))]
    t2 <- roster$Team[min(which(roster$Id == c2))]
    
    if(t1 != t2) {
      roster_proposed <- roster
      roster_proposed$Team[which(roster$Id == c1)] <- t2
      roster_proposed$Team[which(roster$Id == c2)] <- t1
      roster_long_proposed <- roster_long
      roster_long_proposed$Team[which(roster_long$Id == c1)] <- t2
      roster_long_proposed$Team[which(roster_long$Id == c2)] <- t1
      roster_long_proposed$Team_baggage[which(roster_long$Baggage == c1)] <- t2
      roster_long_proposed$Team_baggage[which(roster_long$Baggage == c2)] <- t1
      score_proposed <- score_roster(roster_long_proposed, roster_proposed, weight_vec, num_teams = 9)
      if(runif(1) < 10^(my_scale * (current_score - score_proposed))) {
        current_score <- score_proposed
        roster <- roster_proposed
        roster_long <- roster_long_proposed
        probs <- c(probs, current_score)
      }
    }
  }
  list(roster = roster, roster_long = roster_long, probs = probs)
}
