#'
#' Not sure why the roster graph isn't working correctly. My current guess is that the order
#' of the Id's is different in the layout than in V(graph_df)$names. But, I just don't know.
#'
#'
library(shiny)
library(dplyr)
library(data.table)
library(stringr)
library(shinyBS)
library(igraph)

source("helpers.R")
best_roster <- list(r1 = NULL, r1_long = NULL, probs = NULL)
initial_roster <- NULL

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
                   plotOutput('igraph_output', height = "1000px", click = "click", hover = hoverOpts(
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
  
  #'
  #'
  #' BEGIN: get all of the options for the find_best_roster function in helpers. 
  #'
  #'
  
  get_num_teams <- reactive({
    if(input$num_teams <= 1 && !is.null(input$roster)) {
      r1 <- read.csv(input$roster$datapath)
      if(nrow(r1) > 14)
        return(floor(nrow(r1)/15))
      else return(2)
    }
    return(input$num_teams)
  })
  
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
  
  get_num_no_baggage_weight <- reactive({
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
  
  get_num_baggage_all_weight <- reactive({
    temp <- as.numeric(input$num_baggage_all)
    return(max(temp, 0))
  })
  
  get_weight_vec <- reactive({
    c(get_num_no_baggage_weight(), 
      get_num_women_weight(), 
      get_num_players_weight(), 
      get_best_line_mean_weight(), 
      get_team_mean_weight(),
      get_num_baggage_all_weight()) 
  })
  #'
  #'
  #' END: get parameters for building best roster.
  #'
  #'

  
  get_bag <- reactive({
    if(is.null(input$baggage) && !input$no_baggage) return(NULL)
    if(!is.null(input$baggage)) {
      bag <- read.csv(input$baggage$datapath)
    } else bag <- createHatDrawBaggage(roster)
    return(bag)
  })

  get_best_roster <- observeEvent(eventExpr = input$iterate_1 + input$goButton, priority = 1, handlerExpr = {
    
    #Before clicking on make my roster
    if(input$iterate_1 + input$goButton == 0) return(NULL)
    #First time they click on make my roster
    if(input$iterate_1 + input$goButton == 1) {
      if(is.null(input$roster) || (is.null(input$baggage) && !input$no_baggage)){
        stop("Please Enter File Before Making Roster")
      }
      r1 <- read.csv(input$roster$datapath)
      bag <- get_bag()
      names(r1) <- tolower(names(r1))
      names(r1) <- str_replace_all(names(r1), "[^a-z]", "")
      names(r1) <- stringi::stri_trans_totitle(names(r1))
      initial_roster <<- r1
      if(checkRoster(r1) == -1)
        stop("Please make sure roster is in correct format.")
      if(ncol(bag) < 2)
        stop("Please make sure baggage is in correct format.")
      bag <- fixBaggage(r1, bag)
      team_assignment <- data.frame(Id = unique(r1$Id))
      #
      #set.seed(1) uncomment in debug mode
      #
      # Note: this next line is part of a bug when not every team is assigned at least one woman.
      team_assignment$Team <- c(sample(rep(1:get_num_teams(),floor(nrow(r1)/get_num_teams()))), sample(1:get_num_teams(), nrow(r1) %% get_num_teams()))
      r1 <- left_join(r1, team_assignment)
      scaled_Power <- scale(r1$Power)
      r1$Raw_power <- r1$Power      
      r1$Power <- as.numeric(scaled_Power)
      
      
      r1_long <- left_join(x = r1, y = bag, by = c("Id" = "Baggager"))
      r1_long <- left_join(r1_long, team_assignment, by = c("Baggage" = "Id"), suffix = c("", "_baggage"))
      best_roster <<- list(r1 = r1, r1_long = r1_long)
    } else{ #Subsequent times they click on make my roster
      r1 <- find_best_roster(roster = best_roster$r1, 
                             roster_long = best_roster$r1_long, 
                             weight_vec = get_weight_vec(), 
                             my_scale = get_my_scale(), 
                             score_roster = score_roster, 
                             num_teams = get_num_teams(), 
                             num_iter = input$num_iter, 
                             men_per_line = get_num_men()
                             )
      best_roster <<- list(r1 = r1$roster, r1_long = r1$roster_long, probs = probs)
      #write.csv(r1$roster, "data/debug_r1", row.names = FALSE)
      #write.csv(r1$roster_long, "data/debug_r1_long", row.names = FALSE)
    }
  })
  
  get_roster_summary <- eventReactive(eventExpr = input$iterate_1 + input$goButton, valueExpr = {
    score_roster_debug(roster_long = best_roster$r1_long, 
                       roster = best_roster$r1,
                       weight_vec = get_weight_vec(),
                       num_teams = get_num_teams(),
                       men_per_line = get_num_men())
  }
  )
  
  output$igraph_output <- renderPlot({
    if(input$iterate_1  + input$goButton < 1) return(NULL)
      createPlot(roster_long = best_roster$r1_long,  roster = best_roster$r1)  
  })
   
  output$vertex_info <- renderTable({
    if(is.null(input$click)) return(NULL)
    x_coord <- input$click$x
    y_coord <- input$click$y
    r1 <- best_roster
    roster_long <- r1$r1_long
    roster <- r1$r1
    layout_long <- createLayout(roster_long = roster_long, roster = roster)
    my_layout <- layout_long$layout
    graph_df <- layout_long$graph_df
    id_loc <- which.min(abs(isolate(input$click$x) - my_layout[,1]) + abs(isolate(input$click$y) - my_layout[,2]))
    id_for_display <- as.integer(V(graph_df)$name[id_loc])
    team_for_display <- filter(roster, Id == id_for_display) %>% pull(Team)
    gender_for_display <- ifelse(filter(roster, Id == id_for_display) %>% pull(Female) > 0, "Female", "Male")
    baggage_for_display <- filter(roster_long, Id == id_for_display) %>% pull(Baggage) %>% as.character()
    
    for_out <- data.frame(Id = id_for_display,
                          Gender = gender_for_display, 
                          Team = team_for_display, 
                          Baggage_List = paste(baggage_for_display, sep = '', collapse = ' '),
                          # x = isolate(input$plot_click$x),
                          # y = isolate(input$plot_click$y),
                          stringsAsFactors = FALSE)
    
    return(for_out)
    
    
  })
  
  

  
  
  
  output$table <- renderTable({
    g_out <- get_roster_summary()
    g_out$team_data
  })
  

  
  output$num_no_baggage <- renderText({
    g_out <- get_roster_summary()
    if(is.null(g_out)) return(NULL)
    paste("Number of players with no baggage is", g_out$num_no_baggage, "out of", g_out$num_requesting_baggage)
  })
  
  output$num_baggage_missing <- renderText({
    g_out <- get_roster_summary()
    if(is.null(g_out)) return(NULL)
    paste("Number of baggage requests denied is", g_out$baggage_not_granted, "out of", g_out$total_baggage_requests)
    
  })
 
  #'
  #' 
  #'  This is currently broken. It would be better if it showed the range of probabilities
  #'  rather than the current one.
  #'   
  #'     
  output$probs <- renderTable({
    g_out <- get_roster_summary()
    if(is.null(g_out)) return(NULL)
    data.frame(worst_score = max(best_roster$probs), best_score = min(best_roster$probs), current_score = g_out$prob)
  })
  
  output$maybe_done <- renderText({
    req(input$num_iter)
    g_out <- best_roster
    if(is.null(g_out$probs)) return(NULL)
    if(input$num_iter < 1000) return(NULL)
    if(sum(diff(g_out$probs) > 0) > sum(diff(g_out$probs) < 0)) 
      paste("Algorithm seems to be wandering between essentially equally likely states. If this continues, consider changing increasing number of iterations, increasing importance levels, or increasing scale.")
  })
  
  output$roster_by_team <- renderTable({
    if(is.null(best_roster$r1))
      return(NULL)
    r1 <- best_roster$r1
    new_female <- r1$Female
    new_female[r1$Female > 0] <- "F"
    new_female[r1$Female <= 0] <- "M"
    r1$Female <- new_female
    if("Name" %in% names(r1)){
      return(r1 %>% dplyr::select(Id, Name, Power, Female, Team) %>% dplyr::arrange(Team, Female, desc(Power)))
    } else return(r1 %>% dplyr::select(Id, Power, Female, Team) %>% dplyr::arrange(Team, Female, desc(Power)))
  })
 
  output$iteration_warning <- renderText({
    req(input$num_iter)
    if(input$num_iter <= 200 & (input$goButton + input$iterate_1 >4)) 
      return(paste("Number of iterations set to 200 initially so you could get a feel for whether algorithm is working. Consider increasing iterations to 2000."))
    return(NULL)
  })
  
  output$lots_of_iterations <- renderText({
    if((input$goButton + input$iterate_1) == 20)
      return(paste("You've done a lot of runs now. Consider raising the number of iterations. Once you are more or less happy, run one last time after increasing scale significantly to find a good roster close to current roster listed."))
    return(NULL)
  })
  
  output$downloadData <- downloadHandler(
    filename = "roster.csv",
    content = function(file) {
      return_roster <- left_join(best_roster$r1, initial_roster, by = "Id")
      if("Name" %in% names(best_roster$r1)) {
        write.csv(arrange(return_roster, Team, Female, Name), file, row.names = FALSE)
      } else write.csv(arrange(return_roster, Team, Female), file, row.names = FALSE)
    }
  )
  
  observeEvent(eventExpr = input$iterate_1 + input$goButton, handlerExpr = 
    {
      if(input$iterate_1 > 0 || input$goButton > 0)
        updateTabsetPanel(session, "inTabset", selected = "two")
  })
  
  observeEvent(input$iterate_1, {
    updateActionButton(session, "iterate_1", label = "Iterate")
  })
}


shinyApp(ui = ui, server = server)