my_mad <- function(x, center, expon = 1) {
  if(missing(center)) center <- mean(x)
  mean(abs(x - center)^expon, na.rm = TRUE)
}

#' copied from stackoverflow
weight.community=function(row, membership, weigth.within, weight.between){
  if(as.numeric(membership[which(names(membership)==row[1])])==as.numeric(membership[which(names(membership)==row[2])])){
    weight=weigth.within
  }else{
    weight=weight.between
  }
  return(weight)
}

createHatDrawBaggage <- function(roster) {
  return(data.frame(Baggager = roster$Id, Baggage = roster$Id, Weight = 1, stringsAsFactors = FALSE))
}


createLayout <- function(roster_long, roster) {
  num_no_baggage <- sum((roster_long$Team != roster_long$Team_baggage), na.rm = TRUE)
  d1 <- select(roster_long, Id, Baggage)
  d1$weight <- 0.5
  d2 <-  roster_long %>% group_by(Team) %>% transmute(B1 = dplyr::first(Id),B2 = Id) %>% distinct(B1, B2) %>% ungroup() %>% select(B1, B2)
  names(d2) <- c("Id", "Baggage")
  d2$weight <- 10 + num_no_baggage
  
  graph_df <- graph.data.frame(d = rbind(d1, d2), directed = FALSE)
  graph_df <- simplify(graph_df, remove.loops = TRUE)
  
  set.seed(1)
  my_layout <- norm_coords(layout_with_fr(graph_df))
  return(list(layout = my_layout, graph_df = graph_df))
  
  }

createPlot <- function(roster_long, roster) {

  num_no_baggage <- sum((roster_long$Team != roster_long$Team_baggage), na.rm = TRUE)
  
  d1 <- select(roster_long, Id, Baggage)
  d1$weight <- 0.5
  d2 <-  roster_long %>% group_by(Team) %>% transmute(B1 = dplyr::first(Id),B2 = Id) %>% distinct(B1, B2) %>% ungroup() %>% select(B1, B2)
  names(d2) <- c("Id", "Baggage")
  d2$weight <- 10 + num_no_baggage
  
  graph_df <- graph.data.frame(d = rbind(d1, d2), directed = FALSE)
  graph_df <- simplify(graph_df, remove.loops = TRUE)
  vertex_labels <- round(arrange(roster, Id) %>% pull(Raw_power)) 
  vertex_colors <- ifelse(arrange(roster, Id) %>% pull(Female) > 0,"Pink", "Light Blue") 
  
  arranged_roster_long <- arrange(roster_long, Id)
  players_no_baggage <- setDT(arranged_roster_long)[,  list(No_baggage = all(!Team %in% Team_baggage)), by = Id]
  for(i in 1:length(players_no_baggage$Id)) {
    if(players_no_baggage$No_baggage[i]) {
      vertex_colors[i] <- if(vertex_colors[i] == "Pink") { 
        vertex_colors[i] <- "Hot Pink"
        } else vertex_colors[i] <- "deepskyblue"
      vertex_labels[i] <- paste(vertex_labels[i], roster$Female[i] > 0)
    }
  }
  
  #team_df <- roster_long %>% group_by(Team) %>% distinct(Id) %>%  transmute(B1 = first(Id),B2 = Id) %>% distinct(B1, B2) 
  #team_g <- graph.data.frame(d = team_df[,2:3])
  
  set.seed(1)
  
  my_layout <- norm_coords(layout_with_fr(graph_df))

  graph_df <- delete_edges(graph_df, which(E(graph_df)$weight > 1))
  team_groups <-  lapply(unique(roster_long$Team), function(x) filter(roster_long, Team == x) %>% distinct(Id) %>% pull(Id) %>% as.character)
  
  players_no_baggage <- setDT(roster_long)[,  list(No_baggage = all(!Team %in% Team_baggage)), by = Id]
  
  the_plot <- plot(graph_df, vertex.size = 5, vertex.color = vertex_colors,mark.groups = team_groups, rescale = FALSE, layout = my_layout, vertex.label = vertex_labels, margin = c(-0.5,0,0,0), xlim = c(-0.1, 0.1), arrow.width = 0, arrow.size = 0, arrow.mode = 0, vertex.alpha = 0.2, vertex.label.cex = 0.7)
  
    
  id_loc <- NULL
  # if(!is.null(click_pos)) {
   # if(min(abs(click_pos$x - my_layout[,1]) + abs(click_pos$y - my_layout[,2])) < .5) {
   #   id_loc <- which.min(abs(click_pos$x - my_layout[,1]) + abs(click_pos$y - my_layout[,2]))
  #    id_for_display <- as.integer(V(graph_df)$name[id_loc])
  #    gender_for_display <- ifelse(filter(roster, Id == id_for_display) %>% pull(Female) > 0, "Female", "Male")
  #    baggage_for_display <- filter(roster_long, Id == id_for_display) %>% pull(Baggage) %>% as.character()
    #} else {
    #  id_loc <- NULL
    #  id_for_display <- NULL
    #  gender_for_display <- NULL
    #}
  
  #legend('topright', legend = c("No Baggage M", "No Baggage F"), col = c("Hot pink", "deepskyblue"), pch = 16)  
  plot(graph_df, vertex.size = 5, vertex.color = vertex_colors,mark.groups = team_groups, rescale = FALSE, layout = my_layout, vertex.label = vertex_labels, arrow.width = 0, arrow.size = 0, arrow.mode = 0, vertex.alpha = 0.2, vertex.label.cex = 0.7)
  #if(!is.null(id_loc))
  #   legend(x = -1, y = 1,cex = 0.5, paste("Id = ", id_for_display, gender_for_display, "Baggage ", baggage_for_display ))

  }

checkRoster <- function(roster) {
   
  if(any(sapply(c("Id", "Power", "Female"), function(x) !(x %in% names(roster)) ))) return(-1)
  if(nrow(roster) < 16) return(-1)
  return(0) 
}

fixBaggage <- function(roster, baggage) {
  
  names(baggage)[1:2] <- c("Baggager", "Baggage")
  baggage <- filter(baggage, Baggager %in% roster$Id & Baggage %in% roster$Id)

  if(ncol(baggage) > 2) {
    names(baggage)[3] <- "Weight"
    baggage$Weight <- baggage$Weight/mean(abs(baggage$Weight))
  }
  if(ncol(baggage) == 2 && nrow(baggage) > 0) 
    baggage$Weight <- 1

  baggage <- baggage[,1:3]
  for(i in roster$Id){
    if(!i %in% baggage$Baggager)
      baggage <- rbind(baggage, c(i,i,1))
  }
  return(baggage)
}


#'
#'
#' score_roster caculates a score that mcmc is trying to minimize. Note that we are assuming that
#'   1. power ratings have been scaled to have mean 0 and standard deviation 1
#'   2. the number of 
#'
#'

score_roster <- function(roster_long, roster, weight_vec, num_teams, men_per_line, women_per_line = 2, power_thresh = 0.25) {
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
  
  power_weight <- c(-1/50,-1/100,1/50,1/100)
  
  sorted <- setorder(setDT(roster), Team, Female, -Power)[, indx := seq_len(.N), Team][indx <= men_per_line]
  women_sorted <- setorder(setDT(roster), Team, -Female, -Power)[, indx := seq_len(.N), Team][indx <= women_per_line]
  sorted <- rbind(sorted, women_sorted)
  power_best_line <- sort(aggregate(x = sorted$Power, by = list(sorted$Team), FUN = mean)$x)
  diff_mean_best_line <- sum(power_best_line[c(1,2,num_teams - 1, num_teams)] * power_weight) +
    max(power_best_line[num_teams] - power_best_line[1] - power_thresh, 0)/10
    

  
  power_all <- sort(setDT(roster)[,list(B = mean(Power)), by = 'Team']$B)
  diff_mean_all_players <-  sum(power_all[c(1,2,num_teams - 1, num_teams)] * power_weight) +
    max(power_all[num_teams] - power_all[1] - power_thresh, 0)/10
   
  
  num_baggage_not_granted <- (1 - sum((roster_long$Team == roster_long$Team_baggage)*roster_long$Weight, na.rm = TRUE)/100)
  
  results_vec <- c(num_no_baggage, diff_number_women, diff_number_total, diff_mean_best_line, diff_mean_all_players ,num_baggage_not_granted)
  
  as.numeric(sum(weight_vec * results_vec)   )
  
  #as.numeric(100 * (diff_mean_all_players) + 100 * (diff_mean_best_line) + (diff_number_total) + 100 * (diff_number_women) + 500 * (num_baggage_not_granted) + 100 * (num_no_baggage))
  #+ diff_number_total * 3 + num_no_baggage * .0001 + diff_mean_all_players * 2 + diff_mean_best_line * 3)
}



makeLongRoster <- function(roster, bag, num_teams) {
  
  roster_long <- left_join(x = roster, y = bag, by = c("Id" = "Baggager"))
  roster_long <- left_join(roster_long, team_assignment, by = c("Baggage" = "Id"), suffix = c("", "_baggage"))
  roster_long
}


score_roster_debug <- function(roster_long, roster, weight_vec, num_teams, meanscore = 0, sdev = 1, men_per_line = 5, power_thresh = 0.25) {
  
  
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
 
  num_requesting_baggage <- filter(roster_long, Baggage!= Id) %>% distinct(Id) %>% summarize(n = n()) %>% pull(n)
   
  diff_mean_best_men <- roster %>% 
    filter(Female <= 0) %>% 
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
  
  total_baggage_requests <- filter(roster_long, Id != Baggage) %>% summarize(n = n()) %>% pull(n)
  mean_men_athleticism <- 0 
  
  results_vec <- c(num_no_baggage, diff_number_women, diff_number_total, diff_mean_best_men, diff_mean_all_players ,num_baggage_not_granted)
  
  probs <- score_roster(roster_long, roster, weight_vec, num_teams, men_per_line = men_per_line, power_thresh = power_thresh)
  
  sorted <- setorder(setDT(roster), Team, Female, -Raw_power)[, indx := seq_len(.N), Team][indx <= men_per_line]
  women_sorted <- setorder(setDT(roster), Team, -Female, -Raw_power)[, indx := seq_len(.N), Team][indx <= women_per_line]
  sorted <- rbind(sorted, women_sorted)
  power_best_line <- aggregate(x = sorted$Raw_power, by = list(sorted$Team), FUN = mean)
  power_best_line$x <- power_best_line$x
  names(power_best_line) <- c("Team", "Mean_Power_Best_Line")
  
  team_data <- power_best_line
  team_data$Num_Women <- as.integer(num_women)
  team_data$Mean_Power <- setDT(roster)[,list(B = mean(Raw_power)), by = 'Team']$B
  team_data$Num_Players <- num_players_teams
  
  baggage_not_granted <- sum(roster_long$Team != roster_long$Team_baggage, na.rm = TRUE)
  
  results <- list(prob = probs,
                  num_no_baggage = num_no_baggage, 
                  baggage_not_granted = baggage_not_granted,
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




