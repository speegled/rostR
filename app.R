library(shiny)


ui <- fluidPage(
  titlePanel("Roster Builder"),
  
  sidebarLayout(
    sidebarPanel(h2("Importance Levels"),
    

    fluidRow (
      column(4,
             selectInput("team_mean_weight", h4("Team Mean Power"),
                          choices = list("Level 1" = 1, "Level 2" = 2,
                                         "Level 3" = 5, "Level 4" = 10),selected = 1)),
  
    column(4,
           selectInput("best_line_mean_weight", h4("U-Line Mean Power"),
                        choices = list("Level 1" = 1, "Level 2" = 2,
                                       "Level 3" = 5, "Level 4" = 10),selected = 1)),
    
    column(4,
           selectInput("num_women_weight", h4("Num Women per Team"),
                       choices = list("Level 1" = 1, "Level 2" = 2,
                                      "Level 3" = 5, "Level 4" = 10),selected = 1))
           
    ),
    
    fluidRow(
      column(4,
             selectInput("num_players_weight", h4("Players per Team"),
                          choices = list("Level 1" = 1, "Level 2" = 2,
                                         "Level 3" = 5, "Level 4" = 10),selected = 1)),
      column(4,
             selectInput("num_no_baggage", h4("Granting at least one Baggage"),
                         choices = list("Level 1" = 1, "Level 2" = 2,
                                        "Level 3" = 5, "Level 4" = 10),selected = 1)),
      column(4,
             selectInput("num_baggage_all", h4("Granting Baggage"),
                         choices = list("Level 1" = 1, "Level 2" = 2,
                                        "Level 3" = 5, "Level 4" = 10),selected = 1))
    
  
    ),
    
    h2("Parameters"), 
    
    fluidRow (
      column(3,
             selectInput("reciprocal_baggage_delete", h4("Delete Unreciprocated Baggage"),
                         choices = list("Yes" = TRUE, "No" = FALSE),selected = FALSE)),
      
      column(3,
             selectInput("reciprocal_baggage_priority", h4("Prioritize Reciprocated Baggage"),
                         choices = list("Yes" = TRUE, "No" = FALSE),selected = TRUE)),
      
      column(3,
             selectInput("my_scale", h4("Scale for Metropolis-Hastings"),
                         choices = list("Level 1" = 0.1, "Level 2" = 0.3,
                                        "Level 3" = 1, "Level 4" = 3, "Level 5" = 10),selected = 3)),
      
      column(3,
             numericInput("num_players_per_team", h4("Number of Players Per Team"), value = 17))

      
    ),
    
    selectInput("gender_ratio", h2("Gender Ratio (M/F)"), 
                choices = list("7/0", "6/1", "5/2", "4/3", "3/4", "2/5", "1/6", "0/7")),
    
    h2("File Upload"),
    
    fileInput("roster", h4("Choose .csv roster file")),
    fileInput("baggage", h4("Choose .csv baggage file"))
        
    
    
    
    
    
    
    

  ),
  
  
  mainPanel(h2("Roster Diagnostics"))

)
)


server <- function(input, output) {
  
}


shinyApp(ui = ui, server = server)