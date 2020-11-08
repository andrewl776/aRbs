ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(),
  shinydashboard::dashboardSidebar(),
  shinydashboard::dashboardBody(
    shiny::actionButton("get_arbs_ab", 
                        label = "Get Arbs!"),
    
    shiny::textOutput("results")
  )
)

server <- function(input, output) {
  
  
  output$results <- shiny::renderText({
    
    get_arbs()
    
  })
  
}

shiny::shinyApp(ui = ui, server = server)
