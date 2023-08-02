# LOAD PACKAGES
library(shiny)
library(qualtRics) # for retrieving and processing Qualtrics survey data
library(tidyverse) # for data wrangling and viz
library(urbnthemes) # for styling plots
library(emojifont)
library(DT) # for creating data tables


# ------------------------------------------------------------------------------
# HELPER FUNCTIONS

emojify <- function(data){

  data %>%
    mutate(Q1 = case_when(
      Q1 == 1 ~ emoji("sunny"),
      Q1 == 2 ~ emoji("beach_umbrella"),
      Q1 == 3 ~ emoji("ocean")
    ))
}


# ------------------------------------------------------------------------------
# SET PLOT STYLE (FROM URBNTHEMES)
# set_urbn_defaults(style = "print")


# ------------------------------------------------------------------------------
# SHINY APP

# set Shiny options
options(shiny.sanitize.errors = TRUE)
options(scipen = 999)

# create user interface
ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")
    # TODO (optional): update shiny.css to change the dashboard styling
  ),

  titlePanel(NULL),

  sidebarLayout(

    sidebarPanel(

      # refresh data button
      actionButton(inputId = "refresh_data", label = "Refresh Qualtrics data"),

      # white space
      br(), br(),

      # number of responses
      uiOutput(outputId = "n"),

      # date/time of last refresh
      uiOutput(outputId = "last_refresh")

      # TODO (optional): add additional sidebar content

    ),

    mainPanel(

      # TODO: customize your main panel with plots, tables, etc.
      # this starter code (immediately below) displays one plot and one table
      # you will need to provide code for the plot and table
      # in the server function (further below)

      # plot
      plotOutput(outputId = "plot"),

    )
  )
)

# create server session
server <- function(input, output) {

  # fetch Qualtrics data
  survey <- eventReactive(input$refresh_data, {
    fetch_survey("SV_dnWkyniOFkM4v4O",
                 convert = FALSE,
                 label = FALSE,
                 force_request = TRUE)
  }, ignoreNULL = FALSE)

  # number of responses
  output$n <- renderUI({
    HTML(paste(strong(nrow(survey())), "responses as of last refresh:"))
  })

  # date/time of last refresh
  output$last_refresh <- eventReactive(input$refresh_data, {
    HTML(format(Sys.time(), "%B %d, %Y %I:%M %p"))
  }, ignoreNULL = FALSE)

  # plot
  for_plot <- eventReactive(input$refresh_data, {
    # TODO (optional): prep your Qualtrics data for your plot
    # if you wrote helper functions above, you can call them here!
  }, ignoreNULL = FALSE)

  output$plot <- renderPlot(
    (ggplot(data = survey(), aes(x = Q1)) +
       geom_bar() +
       scale_x_discrete(
         limit = c(1, 2, 3),
         labels = c("Sun", "Shade", "Waves")) +
       theme(panel.background = element_blank(),
             axis.ticks = element_blank(),
             axis.title = element_blank(),
             axis.text.y = element_blank())
    )
  )

}

# build shiny application
shinyApp(ui = ui, server = server)
