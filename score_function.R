library(dplyr)
library(igraph)

roster <- read.csv("/Users/speegled/flroster", sep = "\t", header = FALSE, as.is = TRUE)
head(roster, 9)
names(roster)[4] <- "Name"
names(roster)[5] <- "Power"
names(roster)[c(15,16)] <- c("Email", "Gender")
head(roster, 9)
names(roster)[1] <- "Id"
names(roster)[7:9] <- c("Experience", "Skill", "Athleticism")

roster$V2 <- NULL
roster$V3 <- NULL
roster$V6 <- NULL
names(roster)[7] <- "Age"
head(roster)
roster <- roster[,c(1,3:7, 13)]
roster <- mutate(roster, Experience = str_extract(Experience, "[0-9]"))
roster <- mutate(roster, Skill = str_extract(Skill, "[0-9]"))
roster <- mutate(roster, Athleticism = str_extract(Athleticism, "[0-9]"))
head(roster)
roster[,2:6] <- sapply(2:6, function(x)as.numeric(roster[,x]))
names(roster)[7] <- "Female"
roster$Female <- roster$Female == "Female"
str(roster)
summary(roster)

roster$Power <- as.numeric(scale(roster$Power))
summary(roster)
roster$Female <- as.integer(roster$Female)
roster$Female <- (roster$Female - mean(roster$Female))/sd(roster$Female)


head(roster)
length(unique(roster$Id))
num_teams <- 9
team_assignment <- data.frame(Id = unique(roster$Id))
team_assignment$Team <- sample(c(rep(1:5,16), rep(6:9, 17)))
head(team_assignment)
roster <- left_join(roster, team_assignment)
head(roster)

bag <- read.csv("/Users/speegled/baggage", header = FALSE, sep = "\t", as.is = TRUE)
bag <- bag[,c(1,3)]
roster_long <- left_join(x = roster, y = bag, by = c("Id" = "V1"))
names(roster_long)[9] <- "Baggage"
roster_long <- left_join(roster_long, team_assignment, by = c("Baggage" = "Id"), suffix = c("", "_baggage"))
roster <- roster %>% 
  group_by(Female) %>% 
  mutate(isTop = ifelse(Power > quantile(Power, .9), 2, ifelse(Power > quantile(Power, .75), 1, 0)))
roster$isTop <- as.numeric(scale(roster$isTop))

score_roster <- function(roster_long, roster) {
  num_no_baggage <- roster_long %>% 
    group_by(Id) %>% 
    summarize(no_baggage = all(!Team %in% Team_baggage)) %>% 
    ungroup() %>% 
    summarize(sum(no_baggage))

  num_missing_forced_baggage <- 0
  diff_number_women <- roster %>% 
    group_by(Team) %>% 
    summarize(num_women = mean(Female)) %>% 
    ungroup() %>% 
    summarize(sd_women = sd(num_women)) %>% 
    pull(sd_women)
        
  diff_number_total <- 0  
  if(0) {
    diff_number_total <- roster %>% 
    group_by(Team) %>% 
    summarize(num_men = sum(!Female)) %>% 
    ungroup() %>%
    arrange(desc(num_men)) %>% 
    summarize(n = first(num_men) - last(num_men)) %>% 
    pull(n)
  }
  
  diff_mean_best_line <- roster %>% 
    group_by(Team) %>% 
    summarize(n = mean(isTop)) %>% 
    summarize(diff = sd(n)) %>% 
    pull(diff)
    
  diff_mean_all_players <- roster %>% 
    group_by(Team) %>% 
    summarize(mean = mean(Power)) %>% 
    summarize(diff = sd(mean)) %>% 
    pull(diff)
  
  num_baggage_granted <- roster_long %>% 
    group_by(Id) %>% 
    summarize(num_granted = sum(Team_baggage == Team)) %>% 
    ungroup() %>% 
    pull(num_granted) %>% 
    sum(na.rm = TRUE) 
  
  mean_men_athleticism <- 0 

  
  as.numeric(diff_number_women * 50 + diff_mean_all_players * 5 + num_no_baggage - 33 + diff_mean_best_line * 20 +  -1 * num_baggage_granted / 50)
  #+ diff_number_total * 3 + num_no_baggage * .0001 + diff_mean_all_players * 2 + diff_mean_best_line * 3)
}

current_score <- score_roster(roster_long, roster)
probs <- current_score
Ids <- unique(roster$Id)
for(i in 1:1000) {
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
    score_proposed <- score_roster(roster_long_proposed, roster_proposed)
    if(runif(1) < 10^(15 * current_score - 15 * score_proposed)) {
      current_score <- score_proposed
      roster <- roster_proposed
      roster_long <- roster_long_proposed
      probs <- c(probs, current_score)
    }
  }
}

sum(diff(probs) > 0)/sum(diff(probs) < 0)
roster_long %>% group_by(Id) %>% summarize(no_baggage = all(!Team %in% Team_baggage)) %>% ungroup() %>% summarize(sum(no_baggage))
roster_long %>% distinct(Id, Team, Female) %>% group_by(Team) %>% summarize(n = sum(Female > 0))
roster_long %>% distinct(Id, Team, Power) %>% group_by(Team) %>% summarize(n = mean(Power)) %>% summarize(sd(n))
roster_long %>% group_by(Id) %>% summarize(num_granted = sum(Team_baggage == Team)) %>% ungroup() %>% pull(num_granted) %>% sum(na.rm = TRUE) / sum(!is.na(roster_long$Team_baggage))
roster %>% group_by(Team) %>% summarize(n = mean(isTop)) %>% summarize(diff = sd(n)) %>% pull(diff)

roster_long %>% distinct(Id, Baggage) %>% summarize(sum(is.na(Baggage)))


roster %>% 
  group_by(Id) %>% 
  summarize(no_baggage = all(!Team %in% Team_baggage)) %>% 
  ungroup() %>% 
  summarize(sum(no_baggage))

roster %>% 
  distinct(Id, Team, Female) %>% 
  group_by(Team) %>% 
  summarize(num_women = sum(Female)) %>% 
  ungroup() %>%
  arrange(desc(num_women)) %>% 
  summarize(n = first(num_women) - last(num_women)) %>% 
  pull(n)

roster %>% 
  distinct(Id, Team, Female) %>% 
  group_by(Team) %>% 
  summarize(num_men = sum(!Female)) %>% 
  ungroup() %>%
  arrange(desc(num_men)) %>% 
  summarize(n = first(num_men) - last(num_men)) %>% 
  pull(n)

roster %>% 
  distinct(Id, Team, Power, Female) %>% 
  group_by(Team, Female) %>% 
  filter(Power >= quantile(Power, .75)) %>% 
  ungroup()  %>% 
  group_by(Team)  %>% 
  summarize(mean = mean(Power)) %>% 
  arrange(mean) %>% 
  summarize(diff = last(mean) - first(mean)) %>% 
  pull(diff)

roster %>% 
  distinct(Id, Team, Power) %>% 
  group_by(Team) %>% 
  summarize(mean = mean(Power)) %>% 
  arrange(mean) %>% 
  summarize(diff = last(mean) - first(mean)) %>% 
  pull(diff)

roster_long %>% 
  group_by(Id) %>% 
  summarize(num_granted = sum(Team_baggage == Team)) %>% 
  ungroup() %>% 
  pull(num_granted) %>% 
  sum(na.rm = TRUE) / sum(!is.na(roster_long$Team_baggage))

ros <- read.csv("/Users/speegled/USAURoster", sep = "\t", header = FALSE, as.is = TRUE)
bag <- semi_join(x = bag, y = ros, by = c("V1" = "V1"))
bag <- semi_join(x = bag, y = ros, by = c("V3" = "V1"))
toRemove <- data.frame(V1 = c(2918, 2862, 2163, 3808, 3807, 2994, 3628))  #Players who didn't pay
ros <- anti_join(ros, toRemove)
bag <- anti_join(bag, toRemove, by = c("V1" = "V1"))
bag <- anti_join(bag, toRemove, by = c("V3" = "V1"))
bag <- rbind(bag, data.frame(V1 = 3820, V2 = "Auyong, Brent ", V3 = 3821, V4 = "Reinckens, Bernard "))
ros <- arrange(ros, V16)
forPrintRos <- ros[,c(4, 5, 16)]
#write.table(forPrintRos, file = "FallRoster2017", sep = "\t", col.names = FALSE, row.names = FALSE)
forJoin <- ros[,c(1,4)]
bag <- left_join(bag, forJoin, by = c("V1" = "V1"))
bag <- left_join(bag, forJoin, by = c("V3" = "V1"))
forPrintBag <- bag[,c(5,6)]
#write.table(forPrintBag, file = "FallRoster2017", sep = ": ", col.names = FALSE, row.names= FALSE, append = TRUE)
filter(bag, grepl("emma", V4))
forUSAU <- ros[,c(4, 10, 13, 14, 15, 16, 17, 18)]
names(forUSAU) <- c("Name", "Age", "USAUID", "Telephone", "Email", "Gender", "DOB", "Address") 
write.csv(x = forUSAU, file = "forUSAU.csv", row.names = FALSE)
head(read.csv("forUSAU.csv"))
head(ros)
Team1 <- data.frame(V1 = c(285, 3803, 3066, 3448, 2572, 3187, 440, 190, 1627, 188, 1727, 3803, 3797, 3505,  1570))
g2 <- graph.data.frame(bag[,c(1,3)], directed = FALSE)
answers <- data.frame(V1 = unique(c(bag[,1], bag[,3])))
vertexcolors <- left_join(answers, ros)$V16
vertexlabels <- left_join(answers, ros)$V5
vertexcolors[vertexcolors == "Female "] <- "Pink"
vertexcolors[vertexcolors == "Male "] <- "LightBlue"
plot(g2, vertex.size = 5, vertex.color = vertexcolors[1:129],vertex.label = vertexlabels[1:129], arrow.width = 0, arrow.size = 0, arrow.mode = 0, vertex.alpha = 0.2, vertex.label.cex = 0.7)
finalRoster <- read.csv("/Users/speegled/finalFallLeagueRoster2017_2", header = TRUE, as.is = TRUE, sep = " ")
#finalRoster$V4[77] <- 8
library(stringr)
#ros$V4 <- str_replace_all(ros$V4, " ", "")
#ros$V4 <- str_replace_all(ros$V4, ",", "")
#ros$V4
str_replace_all(ros$V4,regex("\([^)]+\)") ,"")
write.csv(ros$V4, "temporary", row.names = FALSE, col.names = FALSE)
read.csv("/Users/speegled/temporary", header = FALSE, as.is = TRUE)
length(ros$V4)
ros$V4 <- read.csv("/Users/speegled/temporary", header = FALSE, as.is = TRUE)$V1
head(ros)
sum(!sapply(ros$V4, function(x) x %in% finalRoster$V3))
teamSpeegs <- anti_join(ros, finalRoster, by = c("V4" = "V3"))[,c(16, 5, 4, 12)]
names(teamSpeegs) <- c("V1", "V2", "V3", "V4")
teamSpeegs$V1[1:12] <- "M"
teamSpeegs$V1[13:17] <- "F"
teamSpeegs$V4 <- 8
teamSpeegs
finalRoster <- rbind(finalRoster, teamSpeegs)
anyDuplicated(finalRoster$V3)
finalRoster
finalRoster <- left_join(finalRoster, ros[,c(1,4)], by = c("V3" = "V4"))
group_by(finalRoster, V4) %>% summarize(teamRating = mean(V2))
names(finalRoster) <- c("Gender", "Power", "Name", "Team", "LeagueID")
group_by(finalRoster, Team) %>% summarize(sum(Gender == "F"), sum(Gender == "M"))
bag <- bag[,1:4]
names(bag) <- c("ReqID", "ReqName", "BagID", "BagName")
group_by(bag, BagName) %>% summarize(count = n()) %>% arrange(count) %>% tail(n = 5)

sapply(0:8, function(y) sapply(filter(finalRoster, Team == y)$LeagueID, function(x) 
  length(intersect(bag$BagID[which(bag$ReqID == x)],filter(finalRoster, Team == y)$LeagueID)) > 0 
  || !(x %in% bag$V1)))

sum(unlist(sapply(0:8, function(y) sapply(filter(finalRoster, Team == y)$LeagueID, function(x) 
  length(intersect(bag$BagID[which(bag$ReqID == x)],filter(finalRoster, Team == y)$LeagueID))))))

alexros <- read.csv("/Users/speegled/AlexFallRoster.out", header = FALSE, as.is = TRUE)
head(alexros)
names(alexros) <- c("Gender", "Power", "LeagueID", "Team")
alexros <- left_join(alexros, mos, by = c("LeagueID" = "V1"))[,c(7, 2, 1, 18, 4)]
group_by(alexros, Team) %>% summarize(mean = mean(Power))
write.csv(x = alexros, file = "alexroster2017.csv", row.names = FALSE)




mos <- read.csv("/Users/speegled/Downloads/alexroster.csv", sep = ",", header = FALSE, as.is = TRUE)
forPrintRos <- mos[,c(1,5, 16)]
forPrintRos
sum(forPrintRos$V16 == "Male ")
group_by(forPrintRos, V16) %>% summarize(mean(V5))
bag <- read.csv("/Users/speegled/bag", as.is  = TRUE, header = FALSE)
head(bag)
players <- select(mos, V1)
bag <- semi_join(bag, players)
bag <- semi_join(bag, players, by = c("V3" ="V1"))
forPrintBag <- bag[,c(1,3)]
write.table
