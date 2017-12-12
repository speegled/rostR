library(dplyr)
library(igraph)
library(stringr)
library(data.table)

roster <- read.csv("data/flroster", sep = "\t", header = FALSE, as.is = TRUE)
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

write.csv(roster[,c(1,2,7)], "data/formatted_roster.csv", row.names = FALSE)
roster <- roster[,c(1,2,7)]
head(roster)

roster$Power <- as.numeric(scale(roster$Power))
summary(roster)
roster$Female <- as.integer(roster$Female)
roster$Female <- (roster$Female - mean(roster$Female))/sd(roster$Female)


head(roster)
num_players <- length(unique(roster$Id))
players_per_team <- 17
num_teams <- round(num_players/players_per_team)

team_assignment <- data.frame(Id = unique(roster$Id))
team_assignment$Team <- sample(c(rep(1:5,16), rep(6:9, 17)))
head(team_assignment)
roster <- left_join(roster, team_assignment)
head(roster)

bag <- read.csv("data/baggage", header = FALSE, sep = "\t", as.is = TRUE)
bag <- bag[,c(1,3)]
names(bag) <- c("Baggager", "Baggage")
#write.csv(bag, "data/formatted_baggage.csv", row.names = FALSE)


roster_long <- left_join(x = roster, y = bag, by = c("Id" = "Baggager"))
head(roster_long)
roster_long <- left_join(roster_long, team_assignment, by = c("Baggage" = "Id"), suffix = c("", "_baggage"))
#'
#' I believe roster_long doesn't need Power or Female, but leaving for now.
#'
roster <- roster %>% 
  group_by(Female) %>% 
  mutate(isTop = ifelse(Power > quantile(Power, .9), 2, ifelse(Power > quantile(Power, .75), 1, 0)))
roster$isTop <- as.numeric(scale(roster$isTop))

num_requesting_baggage <- num_players - sum(is.na(roster_long$Baggage))
baggage_requests <- sum(!is.na(roster_long$Baggage))

#save(roster_long, roster, file = "data/roster_data")
#load("data/roster_data")

things_to_optimize <- data.frame(property =  c("num_no_baggage", "num_women", "num_players_team", "diff_mean_best_line", "diff_mean_all_players" , "num_bagge_not_granted"), IsOptimized = rep(TRUE, 6), OptimizedValue = rep(0,6))
for(i in which(things_to_optimize$IsOptimized)) {
  weight_vec <- rep(1,6)
  weight_vec[i] <- 1
  out <- find_best_roster(roster, roster_long, weight_vec, myscale, score_roster, num_iter = 2000)
   
}

out <- find_best_roster(out$roster, out$roster_long, weight_vec, myscale, score_roster, num_iter = 2000)
out$probs
#sum(diff(probs) > 0)/sum(diff(probs) < 0)
out$roster_long %>% group_by(Id) %>% summarize(no_baggage = all(!Team %in% Team_baggage)) %>% ungroup() %>% summarize(sum(no_baggage))
out$roster_long %>% distinct(Id, Team, Female) %>% group_by(Team) %>% summarize(n = sum(Female > 0))
out$roster_long %>% distinct(Id, Team, Power) %>% group_by(Team) %>% summarize(n = mean(Power)) %>% summarize(sd(n))
out$roster_long %>% group_by(Id) %>% summarize(num_granted = sum(Team_baggage == Team)) %>% ungroup() %>% pull(num_granted) %>% sum(na.rm = TRUE) / sum(!is.na(roster_long$Team_baggage))
out$roster %>% group_by(Team) %>% summarize(n = mean(isTop)) %>% summarize(diff = sd(n)) %>% pull(diff)




final_roster <- read.csv("data/flroster", sep = "\t", header = FALSE, as.is = TRUE)
head(final_roster)
final_roster
names(final_roster)[1] <- "Id"
final_roster <- left_join(final_roster, select(out$roster, Id, Team))[,c(1, 4,5,16,19 )]
final_roster <- final_roster %>% group_by(Team) %>% mutate(pow = mean(V5)) %>% arrange(Team) %>% print(n = 148)
final_roster %>% group_by(V16, Team) %>% arrange(desc(pow)) %>% top_n(3, V5) %>% group_by(Team) %>% mutate(top_mean = mean(V5)) %>% distinct(Team, top_mean)


%>% mutate(bp = mean(top_n(n = 3, pow)))


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
