score_roster_debug <- function(roster_long, roster, weight_vec, num_teams, meanscore, sdev) {
  
  
  
  num_no_baggage <- roster_long %>% 
    group_by(Id) %>% 
    summarize(no_baggage = all(!Team %in% Team_baggage)) %>% 
    ungroup() %>% 
    summarize(n = (sum(no_baggage))) %>% 
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
  
  probs <- sum(weight_vec * results_vec)
  
  sorted <- setorder(setDT(roster), Team, Female, -Power)[, indx := seq_len(.N), Team][indx <= men_per_line]
  women_sorted <- setorder(setDT(women_roster), Team, -Female, -Power)[, indx := seq_len(.N), Team][indx <= women_per_line]
  sorted <- rbind(sorted, women_sorted)
  power_best_line <- aggregate(x = sorted$Power, by = list(sorted$Team), FUN = mean)
  
  
  results <- list(prob = probs,
                  num_no_baggage = num_no_baggage, 
                  num_women_team = num_women,
                  power_best_line = power_best_line * sdev + meanscore,
                  power_overall = setDT(roster)[,list(B = mean(Power)), by = 'Team'] * sdev + meanscore,
                  baggage_not_granted = sum(roster_long$Team != roster_long$Team_baggage, na.rm = TRUE),
                  num_players_team =  as.numeric(table(roster$Team))
                  )
  
  return(results)

  
  #as.numeric(100 * (diff_mean_all_players) + 100 * (diff_mean_best_line) + (diff_number_total) + 100 * (diff_number_women) + 500 * (num_baggage_not_granted) + 100 * (num_no_baggage))
  #+ diff_number_total * 3 + num_no_baggage * .0001 + diff_mean_all_players * 2 + diff_mean_best_line * 3)
}




