`%>%` <- dplyr::`%>%`
#' @keywords internal


ui <- function() {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "{aRbs}",
                                    titleWidth = "100%"),
    shinydashboard::dashboardSidebar(
      width = "0px"
    ),
    shinydashboard::dashboardBody(
      shiny::fluidRow(
        shiny::column(3,
                      shiny::uiOutput("select_subdomain") %>%
                        shinycssloaders::withSpinner()
        ),
        shiny::column(3,
                      shiny::tags$head(
                        shiny::tags$style(shiny::HTML('#get_arbs_button_btn{margin-top:27px}'))
                      ),
                      shiny::uiOutput("get_arbs_ui") %>%
                        shinycssloaders::withSpinner()
        ),
        shiny::column(3,
                      shiny::uiOutput("select_arb_ui") %>%
                        shinycssloaders::withSpinner()
        ),
        shiny::column(3,
                      shiny::uiOutput("open_new_tab_ui") %>%
                        shinycssloaders::withSpinner())
      ),
      shiny::fluidRow(
        shiny::uiOutput("win_box"),
        shiny::br(),
        shiny::column(width = 12,
                      DT::DTOutput("best_choice")
        ),
        shiny::br()
      ),

      shiny::fluidRow(
        shiny::column(width = 12,
                      shiny::uiOutput("web_page") %>%
                        shinycssloaders::withSpinner()
        ),
        shiny::br()
      )
    ), skin = "green"
  )
}
