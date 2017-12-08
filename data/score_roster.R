score_roster <- function(roster_long, roster, weight_vec, num_teams) {
  
  num_no_baggage <- roster_long %>% 
    group_by(Id) %>% 
    summarize(no_baggage = all(!Team %in% Team_baggage)) %>% 
    ungroup() %>% 
    summarize(n = (sum(no_baggage)/100)) %>% 
    pull(n)
  
  num_women <- as.numeric(table(roster$Team[which(roster$Female> 0)]))
  diff_number_women <- my_mad(num_women, center = mean(num_women)/num_teams) + 
    max(max(num_women) - min(num_women)  - 2, 0) * .02 
  
  # diff_number_women <- roster %>% 
  #   group_by(Team) %>% 
  #   summarize(num_women = mean(Female)) %>% 
  #   ungroup() %>% 
  #   summarize(sd_women = sd(num_women)) %>% 
  #   pull(sd_women)
  
  num_players_teams <- as.numeric(table(roster$Team))
  diff_number_total <- mad(num_players_teams, center = mean(num_players_teams))
  
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
  
  mean_men_athleticism <- 0 
  
  results_vec <- c(num_no_baggage, diff_number_women, diff_number_total, diff_mean_best_men, diff_mean_all_players ,num_baggage_not_granted)
  
  as.numeric(sum(weight_vec * results_vec)   )
  
  #as.numeric(100 * (diff_mean_all_players) + 100 * (diff_mean_best_line) + (diff_number_total) + 100 * (diff_number_women) + 500 * (num_baggage_not_granted) + 100 * (num_no_baggage))
  #+ diff_number_total * 3 + num_no_baggage * .0001 + diff_mean_all_players * 2 + diff_mean_best_line * 3)
}


