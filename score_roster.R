score_roster <- function(roster_long, roster, weight_vec, num_teams, men_per_line = 5, women_per_line = 2) {
  #browser()
  num_no_baggage <-  sum(setDT(roster_long)[,  list(B = all(!Team %in% Team_baggage)) , by = Id]$B)/100
  
  num_women <- as.numeric(table(roster$Team[which(roster$Female> 0)]))
  diff_number_women <- my_mad(num_women, center = mean(num_women))/400 + 
    max(max(num_women) - min(num_women)  - 2, 0) * .015 +
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


