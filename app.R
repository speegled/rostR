library(shiny)
library(dplyr)
library(data.table)
library(stringr)
library(shinyBS)
library(igraph)
library(shinyjs)

source("helpers.R")
best_roster <-   NULL
initial_roster <- NULL

ui <- fluidPage(
  useShinyjs(),
  titlePanel("RostR"),
  
  sidebarLayout(fluid = FALSE,
    sidebarPanel(
      tabsetPanel(
        tabPanel("File Input",
                 fileInput("roster", h4("Player file")),
                 fileInput("baggage", h4("Baggage file")),
                 fluidRow(
                   column(6, checkboxInput("no_baggage", label = "No Baggage")),
                   column(6, checkboxInput("use_default", label = "Use Test Data"))),
                 fluidRow(
                   column(6,
                          numericInput("num_teams", h5("Number of Teams"), step = 1, min = 1, max = 100, value = 1),
                          bsTooltip("num_teams", "Optional", placement = "bottom")
                   ),
                   column(6, selectInput("gender_ratio", h5("Gender Ratio (M/F)"), 
                                         choices = list("7/0" = 7, "6/1" = 6, "5/2" = 5, "4/3" = 4, "3/4" = 3, "2/5" = 2, "1/6" = 1, "0/7" = 0), selected = 5),
                          bsTooltip("gender_ratio", "Used for Power Calculation. Default is 5/2", placement = "bottom")
                   )
                 ),
                 disabled(
                   actionButton("iterate_1", "Load Players and Baggage First")
                 )
        ),
        tabPanel("Fine Control",
                 fluidRow(
                   column(6, numericInput("num_iter", h3("Number of Iterations"), value = 200)),
                   column(6, align = "center", style = "margin-top: 85px;", actionButton("goButton", "Iterate"), style="color: #FFFF33; background-color: #337ab7; border-color: #2e6da4")
                 ),
                 h4("Relative Weights"),
                 fluidRow (
                   column(4,
                          numericInput("team_mean_weight", h5("Team Power"), step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("team_mean_weight", "Tries to have the mean power rating of each team equal", placement = "top")
                   ),
                   column(4,
                          numericInput("best_line_mean_weight", h5("U-Line Power"),  step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("best_line_mean_weight", "Mean power rating of best seven players (by gender ratio) equal", placement = "top")
                   ),
                   column(4,
                          numericInput("num_women_weight", h5("Women Per Team"), step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("num_women_weight", "Number of women on each team the same", placement = "top")
                   )
                 ),
                 fluidRow(
                   column(4,
                          numericInput("num_players_weight", h5("Players per Team"),  step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("num_players_weight", "Number of players on each team the same. (Not implemented)", placement = "top")
                   ),
                   column(4,
                          numericInput("num_no_baggage", h5("One Baggage"),  step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("num_no_baggage", "Minimizes number of people who get none of their requested baggage", placement = "top")
                   ),
                   column(4,
                          numericInput("num_baggage_all", h5("Baggage Requests"), step = 0.05,min = 0,max = 200, value = 1),
                          bsTooltip("num_baggage_all", "Minimizes number of baggage requests denied", placement = "top")
                   )
                 ),
                 h2("Other Options"), 
                 fluidRow (
                   column(4,
                          radioButtons("force_one", h5("Force"),
                                       choices = list("One Baggage" = 1, "Women on Roster" = 2, "Both" = 3, "Neither" = 4),selected = 4),
                          bsTooltip("force_one", "Force everyone to have at least one baggage or equal women on roster. Forcing both is not recommended...", placement = "bottom")
                   ),
                   column(4,
                          numericInput("my_scale", h5("Scale for Metropolis-Hastings"), step = 0.05,min = .05,max = 200, value = 1),
                          bsTooltip("my_scale", "Larger ignores worse rosters in MCMC. Try changing by 10% to start.", placement = "bottom")
                   )
                 )
        ),
        tabPanel("Download",
                 fluidRow(
                   column(12, 
                          align = "center", 
                          downloadButton(style = "margin-top:20px;", 
                                         "downloadData", 
                                         "Download Roster", 
                                         icon = "download"))
      ))
      )
      
    ),
    mainPanel(tabsetPanel(id = "inTabset",
      tabPanel(title = "Welcome", value = "one",
               wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
               h2("Welcome to RostR, an MCMC Interactive Roster Builder"),
               tags$br(),
               HTML("<a href=\"https://github.com/speegled/rostR\">rostR</a>"), "uses Markov Chain Monte Carlo to assign players to teams. When assigning players, there are competing goals. Friends want to play on the same team. Teams should be reasonably competitive. If it is a mixed league, the gender rations of the teams should be similar. RostR allows users to assign importance levels to the various competing goals, and finds a roster which is suited to those goals.",
               tags$br(),
               tags$br(),
               "In the default version, RostR tries to put each player on the same team with at least one person that they requested. It also tries to have similar number of women on each team. Given those two, it tries to make the relative strengths of the best 7 players on a team, and the relative strength of all players on a team, more or less equal. Finally, it tries to grant as many individual baggage requests as possible. If you play around with the program, you'll see that it doesn't necessarily do all of those things in that order, but that is roughly the priority assigned. Here are the instructions for getting started.",
               tags$br(),
               tags$br(),
               "If you just want to see how the program works, choose Use Test Data and click on Start Making My Rosters.",
               tags$hr(),
               tags$hr(),
               h3("Upload Players"),
               tags$ol(
                          tags$li("The player file must be in csv format."),
                          tags$li("The first row of the file must be the list of variable names."),
                          tags$li("Required variable names are Id, Power and Female"),
                          tags$li("Optional variable name is Name"),
                          tags$li("Additional variable names are allowed and returned unchanged with final roster."),
                          tags$li("Strong Suggestions:",
                                  tags$ol(tags$li("Id is a small integer. Non-integer Id's not currently supported."), tags$li("Power is an integer between 0 and 100. Power must be numeric."), tags$li("Female is 1 if female, 0 otherwise.")))
                          
                          
                        ),
               h3("Upload Baggage"),
               tags$ol(
                 tags$li("The baggage file is optional. If you have no baggage, choose No Baggage before continuing. You will also probably need to change Scale for Metropolis-Hastings (in Fine Control tab) to 5 or 10."),
                 tags$li("If included, baggage must be in csv format. There are two required columns and one optional column."),
                 tags$li("The names of the columns are ignored, but should be included in the file."),
                 tags$li("The first column contains the Id of people requesting to play on the same team as someone else."),
                 tags$li("The second column contains the Id of the person the requester wants to play with."),
                 tags$li("The third column is an optional weight to assign to the baggage request."),
                 tags$li("Negative weights mean that those two players do not want to be on the same team; however, negative weights are not currently fully supported."),
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
                 tags$li("You can also view a graphical visualization of the team structure under the Graph tab"),
                 tags$li("In the Graph Tab, you can also click on any vertex to get information about that player. Dark colors indicate that the player received no baggage requests."),
                 tags$li("Before downloading, increase Scale to 20 or 100 and run one last time to get a locally good roster."),
                 tags$li("Download from the Download tab.")
               )
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
               wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
               tableOutput('roster_by_team')
               )
      ),
      tabPanel(title = "Graph", value = "four",
                 tableOutput('vertex_info'),
               wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                 plotOutput('igraph_output', height = "1000px", click = "click", hover = hoverOpts(
                   id = "plot_hover")
                   )
                 )
               )
      )
    )
  )
)


server <- function(input, output, session) {
  
  vertex_colors <- NULL
  vertex_colors_old <- NULL
  for_out_global <- NULL
  
  
  observeEvent(eventExpr = paste(input$roster, input$baggage, input$no_baggage, input$use_default + input$iterate_1), priority = 10, handlerExpr = {
      if(!is.null(input$roster) && !is.null(input$baggage) ||
         (!is.null(input$roster) && input$no_baggage) ||
         (input$use_default)
      ) {
        shinyjs::enable("iterate_1")
        updateActionButton(session, "iterate_1", label = "Start Making My Roster")
      }

  } 
  )
  
  click <- reactiveValues(x = NULL, y = NULL)

  observeEvent(input$click, {
    click$x <- input$click$x
    click$y <- input$click$y
  })
  
  #'
  #'
  #' BEGIN: get all of the options for the find_best_roster function in helpers. 
  #'
  #'
  
  get_num_teams <- reactive({
    if(input$num_teams <= 1 && (!is.null(input$roster) || input$use_default)) {
      if(!input$use_default){
        r1 <- read.csv(input$roster$datapath)
      } else r1 <- read.csv("data/roster_consecutive")
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
    if(is.null(input$baggage) && !input$no_baggage && !input$use_default) return(NULL)
    if(!is.null(input$baggage)) {
      bag <- read.csv(input$baggage$datapath)
    }  
    if(input$no_baggage)
      bag <- createHatDrawBaggage(roster)
    if(input$use_default)
      bag <- read.csv("data/bag_consecutive")
    return(bag)
  })

  #'
  #'
  #' BEGIN: MCMC roster builder
  #'
  #'
  
  get_best_roster <- observeEvent(eventExpr = input$iterate_1 + input$goButton, handlerExpr = {
    
    #Before clicking on make my roster
    if(input$iterate_1 + input$goButton == 0) return(NULL)
    #First time they click on make my roster
    if(input$iterate_1 + input$goButton == 1) {
      #TODO pull this out into get_roster <- reactive()
      if((is.null(input$roster) || (is.null(input$baggage) && !input$no_baggage)) && !input$use_default){
        stop("Please Enter File Before Making Roster")
      }
      if(!input$use_default)
        r1 <- read.csv(input$roster$datapath)
      if(input$use_default)
        r1 <- read.csv("data/roster_consecutive")
      #END: TODO
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
      best_roster <<- list(r1 = r1$roster, r1_long = r1$roster_long, probs = r1$probs)
    }
    
    #Every time they click iterate, we recalculate the vertex colors
    roster <- best_roster$r1
    roster_long <- best_roster$r1_long
    vertex_colors <- ifelse(arrange(roster, Id) %>% pull(Female) > 0,"Pink", "Light Blue") 
    
    arranged_roster_long <- roster_long
    players_no_baggage <- setDT(arranged_roster_long)[,  list(No_baggage = all(!Team %in% Team_baggage)), by = Id]
    for(i in 1:length(players_no_baggage$Id)) {
      if(players_no_baggage$No_baggage[i]) {
        vertex_colors[i] <- if(vertex_colors[i] == "Pink") { 
          vertex_colors[i] <- "Hot Pink"
        } else vertex_colors[i] <- "deepskyblue"
      }
    }
    vertex_colors <<- vertex_colors
    vertex_colors_old <<- vertex_colors
  })
  
  get_roster_summary <- eventReactive(eventExpr = input$iterate_1 + input$goButton, valueExpr = {
    score_roster_debug(roster_long = best_roster$r1_long, 
                       roster = best_roster$r1,
                       weight_vec = get_weight_vec(),
                       num_teams = get_num_teams(),
                       men_per_line = get_num_men())
  }
  )
  
  
  #'
  #'
  #' BEGIN create outputs to use in ui
  #'
  #'
  

  
  output$igraph_output <- renderPlot({
    if(input$iterate_1  + input$goButton < 1) return(NULL)
    if(!is.null(click$x)) {
      vertex_colors <- vertex_colors_old
      roster_long <- best_roster$r1_long
      roster <- best_roster$r1
      all_layout <- createLayout(roster_long, roster)
      my_layout <- all_layout$layout
      graph_df <- all_layout$graph_df
      id_loc <- which.min(abs(click$x - my_layout[,1]) + abs(click$y - my_layout[,2]))
      id_for_display <- V(graph_df)$name[id_loc]
      #graph.data.frame arranges vertices in order of unique(as.vector(mat)), where mat is the matrix of edges
      #V(graph_df)$name is the same as unique(as.vector(as.matrix(select(roster_long, Id, Baggage))))
      baggage_click <- sapply(filter(roster_long, Id == id_for_display) %>% pull(Baggage), function(x) which(V(graph_df)$name == x) )
      vertex_colors[unlist(baggage_click)] <- "floralwhite"
      vertex_colors[id_loc] <- "yellow"
      vertex_colors <<- vertex_colors
    }
    if(is.null(vertex_colors)) 
      vertex_colors <- vertex_colors_old
    createPlot(roster_long = best_roster$r1_long,  roster = best_roster$r1, vertex_colors)
  })
   
  output$vertex_info <- renderTable({
    if(is.null(input$click)) {
      for_out <<- for_out_global
      return(for_out)
    }
    r1 <- best_roster
    roster_long <- r1$r1_long
    roster <- r1$r1
    layout_long <- createLayout(roster_long = roster_long, roster = roster)
    my_layout <- layout_long$layout
    graph_df <- layout_long$graph_df
    id_loc <- which.min(abs(isolate(input$click$x) - my_layout[,1]) + abs(isolate(input$click$y) - my_layout[,2]))
    id_for_display <- V(graph_df)$name[id_loc]
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
    for_out_global <<- for_out
    
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
    input$iterate_1 + input$goButton
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
      if("Name" %in% names(best_roster$r1)) {
        write.csv(arrange(best_roster$r1, Team, Female, Name), file, row.names = FALSE)
      } else write.csv(arrange(best_roster$r1, Team, Female), file, row.names = FALSE)
    }
  )
  
  observeEvent(eventExpr = input$iterate_1 + input$goButton, handlerExpr = 
    {
      if(input$iterate_1+ input$goButton == 1)
        updateTabsetPanel(session, "inTabset", selected = "two")
  })
  
  observeEvent(input$iterate_1, {
    updateActionButton(session, "iterate_1", label = "Iterate")
  })
}


shinyApp(ui = ui, server = server)