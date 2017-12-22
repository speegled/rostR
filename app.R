library(shiny)
library(dplyr)
library(data.table)
library(stringr)
library(shinyBS)
library(igraph)

source("helpers.R")
rosters_best <- list(r1 = NULL, r1_long = NULL)
power_mean <- 0
power_sd <- 1

ui <- fluidPage(
  titlePanel("RostR"),
  
  sidebarLayout(fluid = FALSE,
    sidebarPanel(
      tabsetPanel(
        tabPanel("File Input",
                 fileInput("roster", h4("Player file")),
                 fileInput("baggage", h4("Baggage file")),
                 checkboxInput("no_baggage", label = "No Baggage"),
                 fluidRow(
                   column(6,
                          numericInput("num_teams", h4("Number of Teams"), step = 1, min = 1, max = 100, value = 1),
                          bsTooltip("num_teams", "Set this before Iterating the first time.", placement = "bottom")
                   ),
                   column(6, selectInput("gender_ratio", h4("Gender Ratio (M/F)"), 
                                         choices = list("7/0" = 7, "6/1" = 6, "5/2" = 5, "4/3" = 4, "3/4" = 3, "2/5" = 2, "1/6" = 1, "0/7" = 0), selected = 5),
                          bsTooltip("gender_ratio", "Used for Power Calculation. Default is 5/2", placement = "bottom")
                   )
                 ),
                 actionButton("iterate_1", "Start Making My Rosters")
        ),
        tabPanel("Fine Control",
                 fluidRow(
                 column(6, actionButton("goButton", "Iterate")),
                 column(6, numericInput("num_iter", h3("Number of Iterations"), value = 200))
                 ),
                 h2("Relative Weights"),
                 fluidRow (
                   column(4,
                          numericInput("team_mean_weight", h4("Team Mean Power"), step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("team_mean_weight", "Tries to have the mean power rating of each team equal", placement = "top")
                   ),
                   column(4,
                          numericInput("best_line_mean_weight", h4("U-Line Mean Power"),  step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("best_line_mean_weight", "Mean power rating of best seven players (by gender ratio) equal", placement = "top")
                   ),
                   column(4,
                          numericInput("num_women_weight", h4("Num Women per Team"), step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("num_women_weight", "Number of women on each team the same", placement = "top")
                   )
                 ),
                 fluidRow(
                   column(4,
                          numericInput("num_players_weight", h4("Players per Team"),  step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("num_players_weight", "Number of players on each team the same. (Not implemented)", placement = "top")
                   ),
                   column(4,
                          numericInput("num_no_baggage", h4("Granting at least one Baggage"),  step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("num_no_baggage", "Minimizes number of people who get none of their requested baggage", placement = "top")
                   ),
                   column(4,
                          numericInput("num_baggage_all", h4("Granting Baggage"), step = 0.05,min = 0,max = 200, value = 1),
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
                   )
                 )
        ),
        tabPanel("Download",
                 downloadButton("downloadData", "Download Roster"))
      )
    ),
    mainPanel(tabsetPanel(id = "inTabset",
      tabPanel(title = "Welcome", value = "one",
               h2("Welcome to RostR, an MCMC Interactive Roster Builder"),
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
                 tags$li("Upload your two files, or a player file + no baggage"),
                 tags$li("Choose Number of Teams and Gender Ratio, or let RostR do it automatically"),
                 tags$li("Start Making My Rosters"),
                 tags$li("Click Iterate. Do it a few times to get a feel for what program is doing."),
                 tags$li("You can adjust importance levels in the Fine Control Tab. For example, you can increase the relative importance of baggage granted from Default = 1 to 2 or 5 or 10."),
                 tags$li("Reasonable numbers for importance level are up to about 30 or down to about 1/30."),
                 tags$li("If you like it, increase Iterate."),
                 tags$li("Sometimes you may need to play with relative importance, or force something to happen."),
                 tags$li("You can view the current roster under the Roster tab."),
                 tags$li("You can also view a graphica visualization of the team structure under the Graph tab"),
                 tags$li("Before downloading, increase Scale to 20 or 100 and run one last time to get a locally good roster."),
                 tags$li("Download from the Download tab.")
               )
               
               
      ),
      tabPanel(title = "Diagnostics",value = "two",fluidRow(
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
      tabPanel(title = "Roster", value = "three",
               tableOutput('roster_by_team')
      ),
      tabPanel(title = "Graph", value = "four",
               fluidRow(
                 column(10,
                   plotOutput('igraph_output', height = "1000px", click = "plot_click", hover = hoverOpts(
                   id = "plot_hover")
                   ),
                   column(2, tableOutput('vertex_info'))
               )
               )
               )
    )
    )
  )
)


server <- function(input, output, session) {
  
  
  
  
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
    if(input$my_scale > 100 || input$my_scale < .01) {
      output$my_scale_warning <- renderText({paste("Are you sure you need such a drastic change of scale?")})
    } 
    my_scale <- input$my_scale * 200 
    output$my_scale_warning <- renderText({NULL})
    return(my_scale)
  })
  
  get_num_no_baggage <- reactive({
    temp <- as.numeric(input$num_no_baggage)
    if(temp < 0) temp <- 0
    if(input$force_one == "1")
      temp <- 30
    if(input$force_one == "3")
      temp <- 15
    return(temp)
  })
  
  get_num_women_weight <- reactive({
    temp <- as.numeric(input$num_women_weight)
    if(temp < 0) temp <- 0
    if(input$force_one == "2")
      temp <- 30
    if(input$force_one == "3")
      temp <- 15
    return(temp)
  })
  
  get_num_players_weight <- reactive({
    temp <- as.numeric(input$num_players_weight)
    temp <- max(temp, 0)
    return(temp)
  })
  
  get_best_line_mean_weight <- reactive({
    temp <- as.numeric(input$best_line_mean_weight)
    temp <- max(temp,0)
    return(temp)
    })
  
  get_team_mean_weight <- reactive({
    temp <- as.numeric(input$team_mean_weight)
    max(temp,0)
    })
  
  get_num_baggage_all <- reactive({
    temp <- as.numeric(input$num_baggage_all)
    return(max(temp, 0))
  })
  
  get_num_teams <- reactive({
    if(input$num_teams <= 1 && !is.null(input$roster)) {
      r1 <- read.csv(input$roster$datapath)
      if(nrow(r1) > 14)
        return(floor(nrow(r1)/15))
      else return(2)
    }
    return(input$num_teams)
  })
  
  get_bag <- reactive({
    if(is.null(input$baggage) && !input$no_baggage) return(NULL)
    if(!is.null(input$baggage)) {
      bag <- read.csv(input$baggage$datapath)
    } else bag <- createHatDrawBaggage(roster)
    return(bag)
  })
  
  get_r1_and_r1_long <- reactive({
    r1 <- read.csv(input$roster$datapath)
    bag <- get_bag()
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
    team_assignment$Team <- c(sample(rep(1:get_num_teams(),floor(nrow(r1)/get_num_teams()))), sample(1:get_num_teams(), nrow(r1) %% get_num_teams()))
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
  
  get_hover_pos <- reactive({
    if(!is.null(input$plot_hover))
      c(input$plot_hover$x, input$plot_hover$y)
  })
  
  get_click_pos <- reactive({
      input$plot_click
  })
  
  iterate_or_go <- reactive({
    paste(input$iterate_1, input$goButton)
  })
  
  xxchange <- reactive({
    paste(input$iterate_1 , input$goButton, input$plot_click)
  })
  
  get_out <- eventReactive(eventExpr = iterate_or_go(),
    valueExpr = {
     inFile <- input$file1
    
     if (is.null(input$roster) || (is.null(input$baggage) && TRUE))
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
                              num_teams = get_num_teams(),
                              men_per_line = get_num_men())
      rosters_best <<- list(r1 = out$roster, r1_long = out$roster_long)
    } else {
      out <- find_best_roster(roster = rosters_best$r1, 
                              roster_long = rosters_best$r1_long, 
                              weight_vec = weight_vec, 
                              my_scale = my_scale, 
                              score_roster = score_roster,
                              num_iter = input$num_iter,
                              num_teams = get_num_teams(),
                              men_per_line = get_num_men())
      rosters_best <<- list(r1 = out$roster, r1_long = out$roster_long)
    }
    out2 <- score_roster_debug(roster_long = out$roster_long, out$roster, weight_vec = weight_vec, num_teams =  get_num_teams(), meanscore = power_mean, sdev = power_sd, men_per_line = get_num_men())
    list(out = out, out2 = out2)
    }
    )
  
  get_layout <- reactive({
    xxchange()
    createLayout(roster_long = rosters_best$r1_long, roster = rosters_best$r1)
  })
 
  vertex_info <- eventReactive(eventExpr = input$plot_click, valueExpr = {
    
    my_layout <- get_layout()
    
    d1 <- select(rosters_best$r1_long, Id, Baggage)
    d2 <-  rosters_best$r1_long %>% group_by(Team) %>% transmute(B1 = first(Id),B2 = Id) %>% distinct(B1, B2) %>% ungroup() %>% select(B1, B2)
    names(d2) <- c("Id", "Baggage")
    
    graph_df <- graph.data.frame(d = rbind(d1, d2), directed = FALSE)
    graph_df <- simplify(graph_df, remove.loops = TRUE)
    
    id_loc <- which.min(abs(input$plot_click$x - my_layout[,1]) + abs(input$plot_click$y - my_layout[,2]))
    id_for_display <- as.integer(V(graph_df)$name[id_loc])
    team_for_display <- filter(rosters_best$r1, Id == id_for_display) %>% pull(Team)
    gender_for_display <- ifelse(filter(rosters_best$r1, Id == id_for_display) %>% pull(Female) > 0, "Female", "Male")
    baggage_for_display <- filter(rosters_best$r1_long, Id == id_for_display) %>% pull(Baggage) %>% as.character()
    
    for_out <- data.frame(Id = id_for_display,
                          Gender = gender_for_display, 
                          Team = team_for_display, 
                          Baggage_List = paste(baggage_for_display, sep = '', collapse = ' '),
                          x = input$plot_click$x,
                          y = input$plot_click$y,
                          stringsAsFactors = FALSE)
    
    return(for_out)
  }
  )
   
  output$vertex_info <- renderTable({
    vertex_info()
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
    req(input$num_iter)
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
    new_female[r1$Female <= 0] <- "M"
    r1$Female <- new_female
    if("Name" %in% names(r1)){
      return(r1 %>% dplyr::select(Id, Name, Power, Female, Team) %>% dplyr::arrange(Team, Female, desc(Power)))
    } else return(r1 %>% dplyr::select(Id, Power, Female, Team) %>% dplyr::arrange(Team, Female, desc(Power)))
    
    
    
    
  })
  
  output$igraph_output <- renderPlot({
    xxchange()
    
   createPlot(roster_long = rosters_best$r1_long, roster = rosters_best$r1, mean_power = power_mean, sd_power = power_sd)
  }
  )
  
  output$iteration_warning <- renderText({
    req(input$num_iter)
    if(input$num_iter <= 200 & input$goButton >4) 
      return(paste("Number of iterations set to 200 initially so you could get a feel for whether algorithm is working. Consider increasing iterations to 2000."))
    return(NULL)
  })
  
  output$lots_of_iterations <- renderText({
    if(input$goButton == 20)
      return(paste("You've done a lot of runs now. Consider raising the number of iterations. Once you are more or less happy, run one last time with scale equal to 10 to find a good roster close to current roster listed."))
    return(NULL)
  })
  
  output$downloadData <- downloadHandler(
    filename = "roster.csv",
    content = function(file) {
      if("Name" %in% names(rosters_best$r1)) {
        write.csv(arrange(rosters_best$r1, Team, Female, Name), file, row.names = FALSE)
      } else write.csv(arrange(rosters_best$r1, Team, Female), file, row.names = FALSE)
    }
  )
  
  observeEvent(iterate_or_go, {
    if(input$iterate_1 > 0 || input$goButton > 0)
      updateTabsetPanel(session, "inTabset", selected = "two")
  })
  
  observeEvent(input$iterate_1, {
    updateActionButton(session, "iterate_1", label = "Iterate")
  })
  
  
}


shinyApp(ui = ui, server = server)