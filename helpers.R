my_mad <- function(x, center, expon = 1) {
  if(missing(center)) center <- mean(x)
  mean(abs(x - center)^expon, na.rm = TRUE)
}


checkRoster <- function(roster) {
   
  if(any(sapply(c("Id", "Power", "Female"), function(x) !(x %in% names(roster)) ))) return(-1)
  return(0) 
}

fixBaggage <- function(roster, baggage) {
  
  names(baggage)[1:2] <- c("Baggager", "Baggage")

  baggage <- filter(baggage, Baggager %in% roster$Id & Baggage %in% roster$Id)
  
}


#'
#'
#' score_roster caculates a score that mcmc is trying to minimize. Note that we are assuming that
#'   1. power ratings have been scaled to have mean 0 and standard deviation 1
#'   2. the number of 
#'
#'

score_roster <- function(roster_long, roster, weight_vec, num_teams, men_per_line, women_per_line = 2) {
  #browser()
  women_per_line <- 7 - men_per_line
  num_no_baggage <-  sum(setDT(roster_long)[,  list(B = all(!Team %in% Team_baggage)) , by = Id]$B)/100
  
  num_women <- as.numeric(table(roster$Team[which(roster$Female> 0)]))
  diff_number_women <- my_mad(num_women, center = mean(num_women))/400 + 
    max(max(num_women) - min(num_women)  - 2, 0) * .01 + 
    max(max(num_women) - min(num_women) - 1, 0) * .005
  
  # diff_number_women <- roster %>% 
  #   group_by(Team) %>% 
  #   summarize(num_women = mean(Female)) %>% 
  #   ungroup() %>% 
  #   summarize(sd_women = sd(num_women)) %>% 
  #   pull(sd_women)
  
  num_players_teams <- as.numeric(table(roster$Team))
  diff_number_total <- my_mad(num_players_teams, center = mean(num_players_teams))/1000 + 
    max(max(num_players_teams) - min(num_players_teams) - 2, 0) * .01
  
  sorted <- setorder(setDT(roster), Team, Female, -Power)[, indx := seq_len(.N), Team][indx <= men_per_line]
  women_sorted <- setorder(setDT(roster), Team, -Female, -Power)[, indx := seq_len(.N), Team][indx <= women_per_line]
  sorted <- rbind(sorted, women_sorted)
  power_best_line <- aggregate(x = sorted$Power, by = list(sorted$Team), FUN = mean)$x
  diff_mean_best_line <- my_mad(power_best_line, expon = 1.2)/10 +
    max(max(power_best_line) - min(power_best_line) - .25, 0)/10
  
  power_all <- setDT(roster)[,list(B = mean(Power)), by = 'Team']$B
  diff_mean_all_players <- my_mad(power_all, expon = 1.2)/10 +
    max(max(power_all) - min(power_all) - .25, 0) /10
  
  num_baggage_not_granted <- (1 - sum(roster_long$Team == roster_long$Team_baggage, na.rm = TRUE)/500)
  
  results_vec <- c(num_no_baggage, diff_number_women, diff_number_total, diff_mean_best_line, diff_mean_all_players ,num_baggage_not_granted)
  
  as.numeric(sum(weight_vec * results_vec)   )
  
  #as.numeric(100 * (diff_mean_all_players) + 100 * (diff_mean_best_line) + (diff_number_total) + 100 * (diff_number_women) + 500 * (num_baggage_not_granted) + 100 * (num_no_baggage))
  #+ diff_number_total * 3 + num_no_baggage * .0001 + diff_mean_all_players * 2 + diff_mean_best_line * 3)
}


checkRoster <- function(roster) {
  return(roster)
} 

checkBaggage <- function(bag) {
  return(bag)
}

makeLongRoster <- function(roster, bag, num_teams) {
  
  roster_long <- left_join(x = roster, y = bag, by = c("Id" = "Baggager"))
  roster_long <- left_join(roster_long, team_assignment, by = c("Baggage" = "Id"), suffix = c("", "_baggage"))
  roster_long
}


score_roster_debug <- function(roster_long, roster, weight_vec, num_teams, meanscore = 0, sdev = 1, men_per_line = 5) {
  
  
  women_per_line = 7 - men_per_line
  
  num_no_baggage <- roster_long %>% 
    group_by(Id) %>% 
    summarize(no_baggage = all(!Team %in% Team_baggage)) %>% 
    ungroup() %>% 
    summarize(n = (sum(no_baggage))) %>% 
    pull(n)
  
  num_no_baggage <- num_no_baggage - sum(is.na(roster_long$Baggage))
  
  num_women <- as.numeric(table(roster$Team[which(roster$Female> 0)]))
  diff_number_women <- my_mad(num_women, center = mean(num_women))/400 + 
    max(max(num_women) - min(num_women)  - 2, 0) * .02 
  
  # diff_number_women <- roster %>% 
  #   group_by(Team) %>% 
  #   summarize(num_women = mean(Female)) %>% 
  #   ungroup() %>% 
  #   summarize(sd_women = sd(num_women)) %>% 
  #   pull(sd_women)
  
  num_players_teams <- as.numeric(table(roster$Team))
  diff_number_total <- mad(num_players_teams, center = mean(num_players_teams))
  diff_number_total <- my_mad(num_players_teams, center = mean(num_players_teams))/1000 + 
    max(max(num_players_teams) - min(num_players_teams) - 2, 0) * .01
 
  num_requesting_baggage <- filter(roster_long, !is.na(Baggage)) %>% distinct(Id) %>% summarize(n = n()) %>% pull(n)
   
  diff_mean_best_men <- roster %>% 
    filter(Female < 0) %>% 
    group_by(Team) %>% 
    top_n(5, Power) %>% 
    ungroup() %>% 
    summarize(diff = my_mad(Power, center = mean(Power))) %>% 
    pull(diff)
  
  diff_mean_all_players <- roster %>% 
    group_by(Team) %>% 
    summarize(mn = mean(Power)) %>% 
    summarize(diff = my_mad(mn, center = mean(mn))) %>% 
    pull(diff)
  
  num_baggage_not_granted <- (1 - mean(roster_long$Team == roster_long$Team_baggage, na.rm = TRUE))
  
  total_baggage_requests <- sum(!is.na(roster_long$Baggage))
  
  mean_men_athleticism <- 0 
  
  results_vec <- c(num_no_baggage, diff_number_women, diff_number_total, diff_mean_best_men, diff_mean_all_players ,num_baggage_not_granted)
  
  probs <- sum(weight_vec * results_vec)
  
  sorted <- setorder(setDT(roster), Team, Female, -Power)[, indx := seq_len(.N), Team][indx <= men_per_line]
  women_sorted <- setorder(setDT(roster), Team, -Female, -Power)[, indx := seq_len(.N), Team][indx <= women_per_line]
  sorted <- rbind(sorted, women_sorted)
  power_best_line <- aggregate(x = sorted$Power, by = list(sorted$Team), FUN = mean)
  power_best_line$x <- power_best_line$x * sdev + meanscore
  names(power_best_line) <- c("Team", "Mean_Power_Best_Line")
  
  team_data <- power_best_line
  team_data$Num_Women <- as.integer(num_women)
  team_data$Mean_Power <- setDT(roster)[,list(B = mean(Power)), by = 'Team']$B * sdev + meanscore
  team_data$Num_Players <- num_players_teams
  
  results <- list(prob = probs,
                  num_no_baggage = num_no_baggage, 
                  baggage_not_granted = sum(roster_long$Team != roster_long$Team_baggage, na.rm = TRUE),
                  team_data = team_data,
                  num_requesting_baggage = num_requesting_baggage,
                  total_baggage_requests = total_baggage_requests
  )
  
  
  return(results)
  
  
  #as.numeric(100 * (diff_mean_all_players) + 100 * (diff_mean_best_line) + (diff_number_total) + 100 * (diff_number_women) + 500 * (num_baggage_not_granted) + 100 * (num_no_baggage))
  #+ diff_number_total * 3 + num_no_baggage * .0001 + diff_mean_all_players * 2 + diff_mean_best_line * 3)
}

















find_best_roster <- function(roster, roster_long, weight_vec, my_scale = 200, score_roster, num_teams = 9, num_iter = 1000, men_per_line) {
  
  current_score <- score_roster(roster_long, roster, weight_vec, num_teams = num_teams, men_per_line = men_per_line)
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
      score_proposed <- score_roster(roster_long_proposed, roster_proposed, weight_vec, num_teams = num_teams, men_per_line = men_per_line)
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




