library(shiny)
library(dplyr)
library(data.table)
library(stringr)
library(shinyBS)

rosters_best <- list(r1 = NULL, r1_long = NULL)

ui <- fluidPage(
  titlePanel("Roster Builder"),
  
  sidebarLayout(
    sidebarPanel(h2("Importance Levels"),
                 
                 
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
      column(3,
             selectInput("reciprocal_baggage_delete", h4("Delete Unreciprocated Baggage"),
                         choices = list("Yes" = TRUE, "No" = FALSE),selected = FALSE),
      bsTooltip("reciprocal_baggage_delete", "Not yet implemented", placement = "bottom")
      ),
      column(3,
             selectInput("reciprocal_baggage_priority", h4("Prioritize Reciprocated Baggage"),
                         choices = list("Yes" = TRUE, "No" = FALSE),selected = TRUE),
             
             bsTooltip("reciprocal_baggage_priority", "Not yet implemented", placement = "bottom")
             
             ),
      
      
      column(3,
             numericInput("my_scale", h4("Scale for Metropolis-Hastings"), value = 1),
             bsTooltip("my_scale", "Faster converges faster, but maybe not to global best", placement = "bottom")
             
             ),
      
      column(3,
             numericInput("num_teams", h4("Number of Teams"), value = 9),
             
             bsTooltip("num_teams", "Set this before uploading rosters.", placement = "bottom")
             
             )

      
    ),
    
    fluidRow(
      column(6, selectInput("gender_ratio", h3("Gender Ratio (M/F)"), 
                choices = list("7/0", "6/1", "5/2", "4/3", "3/4", "2/5", "1/6", "0/7")),
             
             bsTooltip("gender_ratio", "Not yet implemented. Default is 5/2", placement = "bottom")
             
             ),
      column(6, numericInput("num_iter", h3("Number of Iterations"), value = 1000))
    ),
    
    h2("File Upload"),
    
    fileInput("roster", h4("csv format. required vars: Id, Power, Female")),
    fileInput("baggage", h4("csv: required vars: Baggager, Baggage")),
    actionButton("goButton", "Iterate")
        
    
    
    
    
    
    
    

  ),
  
  
  mainPanel(tabsetPanel(
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
            textOutput('my_scale_warning')
                        
            ),
  
  tabPanel("Roster",
          tableOutput('roster_by_team')
              )

))
)
)


server <- function(input, output) {
  
  output$text1 <- renderText("Test")
  
  
  #'
  #'
  #' my_scale default is 200, which can be made into a number between 2 and 20000 by the user. 
  #'
  #'
  get_my_scale <- reactive({
    if(input$my_scale > 100 || input$my_scale < 0.01) {
      output$my_scale_warning <- renderText({paste("Scale should be a number between 0.01 and 100. \n The extremes indicate being 100 times less likely or 100 times more likely to accept a proposal that makes the score worse. \n A typical change would be to try a scale of 2 or 0.5 to start. Scale reset to 1.")})
      return(1)
    } else {
      my_scale <- log(input$my_scale, base = 10) * 200 
      output$my_scale_warning <- renderText({NULL})
    }
    return(my_scale)
  })
  
  get_num_no_baggage <- reactive({
    switch(input$num_no_baggage,
           "1" = return(1),
           "2" = return(2),
           "3" = return(5))
  })
  
  get_num_women_weight <- reactive({
    switch(input$num_women_weight,
           "1" = return(1),
           "2" = return(2),
           "3" = return(5))
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
    switch(input$num_baggage_all,
           "1" = return(1),
           "2" = return(2),
           "3" = return(5))
  })
  
  
  get_r1_and_r1_long <- reactive({
    input$num_teams
    r1 <- read.csv(input$roster$datapath)
    bag <- read.csv(input$baggage$datapath)
    team_assignment <- data.frame(Id = unique(r1$Id))
    team_assignment$Team <- c(sample(rep(1:input$num_teams,floor(nrow(r1)/input$num_teams))), sample(1:input$num_teams, nrow(r1) %% input$num_teams))
    r1 <- left_join(r1, team_assignment)
    
    r1_long <- left_join(x = r1, y = bag, by = c("Id" = "Baggager"))
    r1_long <- left_join(r1_long, team_assignment, by = c("Baggage" = "Id"), suffix = c("", "_baggage"))
    r_value <- list(r1 = r1, r1_long = r1_long)
    return(r_value)
  })
  
  get_out <- reactive({
    
    input$goButton
    
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
    
    
    
    out2 <- score_roster_debug(roster_long = out$roster_long, out$roster, weight_vec = weight_vec, num_teams =  input$num_teams, men_per_line = 5)
    list(out = out, out2 = out2)
  })
  
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
      paste("probs don't seem to be improving. If this continues, consider changing parameters.")
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
    r1 %>% dplyr::select(Id, Power, Female, Team) %>% dplyr::arrange(Team, Female, desc(Power))
    
    
  })
  
  
  }


shinyApp(ui = ui, server = server)