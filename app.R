library(shiny)
library(dplyr)
library(data.table)
library(stringr)
library(shinyBS)

rosters_best <- list(r1 = NULL, r1_long = NULL)
power_mean <- 0
power_sd <- 1

ui <- fluidPage(
  titlePanel("RostR"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("File Upload",
                 fileInput("roster", h4("Player file in csv format. required vars: Id, Power, Female")),
                 fileInput("baggage", h4("Baggage file in csv format: required vars: Baggager, Baggage")),
                 actionButton("RostRize", "RostRize")
        ),
        tabPanel("Fine Control",
                 fluidRow (
                   column(4,
                          radioButtons("team_mean_weight", h4("Team Mean Power"),
                                       choices = list("Low" = 1, "Medium" = 2,
                                                      "High" = 3),selected = 2),
                          bsTooltip("team_mean_weight", "Tries to have the mean power rating of each team equal", placement = "top")
                   ),
                   column(4,
                          radioButtons("best_line_mean_weight", h4("U-Line Mean Power"),
                                       choices = list("Low" = 1, "Medium" = 2,
                                                      "High" = 3),selected = 2),
                          bsTooltip("best_line_mean_weight", "Mean power rating of best seven players (by gender ratio) equal", placement = "top")
                   ),
                   column(4,
                          radioButtons("num_women_weight", h4("Num Women per Team"),
                                       choices = list("Low" = 1, "Medium" = 2,
                                                      "High" = 3),selected = 2),
                          bsTooltip("num_women_weight", "Number of women on each team the same", placement = "top")
                   )
                 ),
                 fluidRow(
                   column(4,
                          radioButtons("num_players_weight", h4("Players per Team"),
                                       choices = list("Low" = 1, "Medium" = 2,
                                                      "High" = 3),selected = 2),
                          bsTooltip("num_players_weight", "Number of players on each team the same. (Not implemented)", placement = "top")
                   ),
                   column(4,
                          radioButtons("num_no_baggage", h4("Granting at least one Baggage"),
                                       choices = list("Low" = 1, "Medium" = 2,
                                                      "High" = 3),selected = 2),
                          bsTooltip("num_no_baggage", "Minimizes number of people who get none of their requested baggage", placement = "top")
                   ),
                   column(4,
                          radioButtons("num_baggage_all", h4("Granting Baggage"),
                                       choices = list("Low" = 1, "Medium" = 2,
                                                      "High" = 3),selected = 2),
                          bsTooltip("num_baggage_all", "Minimizes number of baggage requests denied", placement = "top")
                   )
                 ),
                 h2("Other Options"), 
                 fluidRow (
                   column(4,
                          radioButtons("force_one", h4("Force"),
                                       choices = list("One Baggage" = 1, "Women on Roster" = 2, "Both" = 3, "Neither" = 4),selected = 4),
                          bsTooltip("force_one", "Force everyone to have at least one baggage or equal women on roster. Forcing both is not recommended...", placement = "bottom")
                   ),
                   column(4,
                          numericInput("my_scale", h4("Scale for Metropolis-Hastings"), step = 0.05,min = .05,max = 200, value = 1),
                          bsTooltip("my_scale", "Larger ignores worse rosters in MCMC. Try changing by 10% to start.", placement = "bottom")
                   ),
                   column(4,
                          numericInput("num_teams", h4("Number of Teams"), value = 9),
                          bsTooltip("num_teams", "Set this before Iterating the first time.", placement = "bottom")
                   )
                 ),
                 fluidRow(
                   column(6, selectInput("gender_ratio", h3("Gender Ratio (M/F)"), 
                                         choices = list("7/0" = 7, "6/1" = 6, "5/2" = 5, "4/3" = 4, "3/4" = 3, "2/5" = 2, "1/6" = 1, "0/7" = 1), selected = 5),
                          bsTooltip("gender_ratio", "Not yet implemented. Default is 5/2", placement = "bottom")
                   ),
                   column(6, numericInput("num_iter", h3("Number of Iterations"), value = 200))
                 ),
                 actionButton("goButton", "Iterate")
        )
      )
    ),
    mainPanel(tabsetPanel(
      tabPanel("Welcome",
               h2("Welcome to RostR, an MCMC Based, Interactive Roster Builder"),
               tags$br(),
               "RostR uses Markov Chain Monte Carlo to assign players to teams. When assigning players, there are competing goals. Friends want to play on the same team. Teams should be reasonably competitive. If it is a mixed league, the gender rations of the teams should be similar. RostR allows users to assign importance levels to the various competing goals, and finds a roster which is suited to those goals.",
               tags$br(),
               tags$br(),
               "In the default version, RostR tries to put each player on the same team with at least one person that they requested. It also tries to have similar number of women on each team. Given those two, it tries to make the relative strengths of the best 7 players on a team, and the relative strength of all players on a team, more or less equal. Finally, it tries to grant as many individual baggage requests as possible. If you play around with the program, you'll see that it doesn't necessarily do all of those things in that order, but that is roughly the priority assigned. Here are the instructions for getting started.",
               tags$hr(),
               tags$hr(),
               h3("Upload Players"),
               tags$ol(
                          tags$li("The player file must be in csv format."),
                          tags$li("The first row of the file must be the list of variable names."),
                          tags$li("Required variable names are Id, Power and Female"),
                          tags$li("Optional variable name is Name"),
                          tags$li("Additional variable names are allowed and returned unchanged with final roster."),
                          tags$li("Suggestions:",
                                  tags$ol(tags$li("Id is a small integer."), tags$li("Power is an integer between 0 and 100"), tags$li("Female is 1 if female, 0 otherwise. (This is more than a suggestion).")))
                          
                          
                        ),
               h3("Upload Baggage"),
               tags$ol(
                 tags$li("The baggage file is optional. There are two required columns and one optional column."),
                 tags$li("The names of the columns are ignored, but should be included in the file."),
                 tags$li("The first column contains the Id of people requesting to play on the same team as someone else."),
                 tags$li("The second column contains the Id of the person the requester wants to play with."),
                 tags$li("The third column is an optional weight to assign to the baggage request."),
                 tags$li("Negative weights mean that those two players do not want to be on the same team."),
                 tags$li("The weights are rescaled to have mean absolute value of 1, so only the relative weights are imortant.")
                        ),
               h3("Run the Builder"),
               tags$ol(
                 tags$li("Upload your two files."),
                 tags$ol(h4("Default"),
                         tags$li("If you want to see what the default settings do, click Build my Roster."),
                         tags$li("This will take several minutes to run."),
                         tags$li("You can observe the progress by viewing the Diagnostics, Roster or Graph tab."),
                        tags$li("When the builder is done, the download roster button will be active in the Download tab.")
                         ),
                 
               tags$ol(h4("Fine Control"),
                 tags$li("If you want finer control, use the Importance Levels tab and the Iterate button inside that tab."),
                 tags$li("You will want to Iterate multiple times, perhaps changing parameters, until you find a roster that you like."),
                 tags$li("You can download your roster under the Download tab.")
               )
               )
               
               
      ),
      tabPanel("Diagnostics", fluidRow(
        column(12,
               tableOutput('table')
        )
      ),
      tags$hr(),
      textOutput('num_no_baggage'),
      textOutput('num_baggage_missing'),
      tableOutput('probs'),
      textOutput('maybe_done'),
      tags$hr(),
      textOutput('my_scale_warning'),
      textOutput('iteration_warning')
      ),
      tabPanel("Roster",
               tableOutput('roster_by_team')
      )
    )
    )
  )
)


server <- function(input, output) {
  
  output$text1 <- renderText({
    if (is.null(input$roster) || is.null(input$baggage)) {
      
    }
  })
  
  
  #'
  #'
  #' my_scale default is 200, which can be made into a number between 50 and 800 by the user. 
  #'
  #'
  
  get_num_men <- reactive({
    return(as.integer(input$gender_ratio))
  })
  
  get_my_scale <- reactive({
    if(input$my_scale > 4 || input$my_scale < 1/4) {
      output$my_scale_warning <- renderText({paste("Scale should be a number between 1/4 and 4, and realistically should be more like 1.2 or 0.8. Resetting to 1.")})
      return(200)
    } else {
      my_scale <- input$my_scale * 200 
      output$my_scale_warning <- renderText({NULL})
    }
    return(my_scale)
  })
  
  get_num_no_baggage <- reactive({
    temp <- switch(input$num_baggage_all,
                   "1" = 1,
                   "2" = 2,
                   "3" = 5)
    if(input$force_one == "1")
      temp <- 30
    if(input$force_one == "3")
      temp <- 15
    return(temp)
  })
  
  get_num_women_weight <- reactive({
    temp <- switch(input$num_women_weight,
                   "1" = 1,
                   "2" = 2,
                   "3" = 5)
    if(input$force_one == "2")
      temp <- 30
    if(input$force_one == "3")
      temp <- 15
    return(temp)
  })
  
  get_num_players_weight <- reactive({
    switch(input$num_players_weight,
           "1" = return(1),
           "2" = return(2),
           "3" = return(5))
  })
  
  get_best_line_mean_weight <- reactive({
    switch(input$best_line_mean_weight,
           "1" = return(0.5),
           "2" = return(2),
           "3" = return(3))
  })
  
  get_team_mean_weight <- reactive({
    switch(input$team_mean_weight,
           "1" = return(0.5),
           "2" = return(2),
           "3" = return(3))
  })
  
  get_num_baggage_all <- reactive({
    temp <- switch(input$num_baggage_all,
                   "1" = 1,
                   "2" = 2,
                   "3" = 5)
    if(input$force_one == "1")
      temp <- temp
    return(temp)
  })
  
  
  get_r1_and_r1_long <- reactive({
    input$num_teams
    r1 <- read.csv(input$roster$datapath)
    bag <- read.csv(input$baggage$datapath)
    #'
    #' Look at me and my defensive programming!
    #'
    names(r1) <- tolower(names(r1))
    names(r1) <- str_replace_all(names(r1), "[^a-z]", "")
    names(r1) <- stringi::stri_trans_totitle(names(r1))
    if(checkRoster(r1) == -1)
      stop("Please make sure roster is in correct format.")
    if(ncol(bag) < 2)
      stop("Please make sure baggage is in correct format.")
    bag <- fixBaggage(r1, bag)
    #'
    #' End of defensive programming
    #'
    
    team_assignment <- data.frame(Id = unique(r1$Id))
    team_assignment$Team <- c(sample(rep(1:input$num_teams,floor(nrow(r1)/input$num_teams))), sample(1:input$num_teams, nrow(r1) %% input$num_teams))
    r1 <- left_join(r1, team_assignment)
    scaled_Power <- scale(r1$Power)
    power_mean <<- mean(r1$Power)
    power_sd <<- sd(r1$Power)
    r1$Power <- as.numeric(scaled_Power)
    
    
    r1_long <- left_join(x = r1, y = bag, by = c("Id" = "Baggager"))
    r1_long <- left_join(r1_long, team_assignment, by = c("Baggage" = "Id"), suffix = c("", "_baggage"))
    r_value <- list(r1 = r1, r1_long = r1_long)
    return(r_value)
  })
  
  get_out <- eventReactive(eventExpr = input[["goButton"]],
    valueExpr = {
     inFile <- input$file1
    
     if (is.null(input$roster) || is.null(input$baggage))
      return(NULL)
    
     weight_vec <- c(get_num_no_baggage(), 
                    get_num_women_weight(), 
                    get_num_players_weight(), 
                    get_best_line_mean_weight(), 
                    get_team_mean_weight(),
                    get_num_baggage_all()) 
    
    
    
    my_scale <- get_my_scale()
    
    
    
    if(is.null(rosters_best$r1)) {
      r_value <- get_r1_and_r1_long()
      r1 <- r_value$r1
      r1_long <- r_value$r1_long
      out <- find_best_roster(roster = r1, 
                              roster_long = r1_long, 
                              weight_vec = weight_vec, 
                              my_scale = my_scale, 
                              score_roster = score_roster,
                              num_iter = input$num_iter,
                              num_teams = input$num_teams)
      rosters_best <<- list(r1 = out$roster, r1_long = out$roster_long)
    } else {
      out <- find_best_roster(roster = rosters_best$r1, 
                              roster_long = rosters_best$r1_long, 
                              weight_vec = weight_vec, 
                              my_scale = my_scale, 
                              score_roster = score_roster,
                              num_iter = input$num_iter,
                              num_teams = input$num_teams)
      rosters_best <<- list(r1 = out$roster, r1_long = out$roster_long)
    }
    
    
    
    out2 <- score_roster_debug(roster_long = out$roster_long, out$roster, weight_vec = weight_vec, num_teams =  input$num_teams, meanscore = power_mean, sdev = power_sd, men_per_line = 5)
    list(out = out, out2 = out2)
    }
    )
  

  
  output$table <- renderTable({
    g_out <- get_out()
    g_out$out2$team_data
  })
  
  
  output$num_no_baggage <- renderText({
    g_out <- get_out()
    if(is.null(g_out)) return(NULL)
    paste("Number of players with no baggage is", g_out$out2$num_no_baggage, "out of", g_out$out2$num_requesting_baggage)
  })
  
  output$num_baggage_missing <- renderText({
    g_out <- get_out()
    if(is.null(g_out)) return(NULL)
    paste("Number of baggage requests denied is", g_out$out2$baggage_not_granted, "out of", g_out$out2$total_baggage_requests)
    
  })
  
  output$probs <- renderTable({
    
    g_out <- get_out()
    if(is.null(g_out)) return(NULL)
    data.frame(max_prob = max(g_out$out$probs), min_prob = min(g_out$out$probs))
  })
  
  output$maybe_done <- renderText({
    g_out <- get_out()
    if(is.null(g_out)) return(NULL)
    if(input$num_iter < 1000) return(NULL)
    if(sum(diff(g_out$out$probs) > 0) > sum(diff(g_out$out$probs) < 0)) 
      paste("Algorithm seems to be wandering between essentially equally likely states. If this continues, consider changing increasing number of iterations or increasing scale.")
  })
  
  output$text <- renderText("Test")
  
  output$roster_by_team <- renderTable({
    if(is.null(rosters_best$r1))
      return(NULL)
    
    r1 <- rosters_best$r1
    new_female <- r1$Female
    new_female[r1$Female > 0] <- "F"
    new_female[r1$Female < 0] <- "M"
    r1$Female <- new_female
    if("Name" %in% names(r1)){
      return(r1 %>% dplyr::select(Id, Name, Power, Female, Team) %>% dplyr::arrange(Team, Female, desc(Power)))
    } else return(r1 %>% dplyr::select(Id, Power, Female, Team) %>% dplyr::arrange(Team, Female, desc(Power)))
    
    
    
    
  })
  
  
  output$iteration_warning <- renderText({
    if(input$num_iter <= 200 & input$goButton >4) 
      return(paste("Number of iterations set to 200 initially so you could get a feel for whether algorithm is working. Consider increasing iterations to 2000."))
    return(NULL)
  })
  
  output$lots_of_iterations <- renderText({
    if(input$goButton == 20)
      return(paste("You've done a lot of runs now. Consider raising the number of iterations. Once you are more or less happy, run one last time with scale equal to 10 to find a good roster close to current roster listed."))
    return(NULL)
  })
  
}


shinyApp(ui = ui, server = server)