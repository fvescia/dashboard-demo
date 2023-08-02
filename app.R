# LOAD PACKAGES
library(shiny)
library(qualtRics) # for retrieving and processing Qualtrics survey data
library(tidyverse) # for data wrangling and viz
library(showtext) # to get Lato
library(urbnthemes) # for styling plots
library(emojifont)
library(DT) # for creating data tables


# ------------------------------------------------------------------------------
# SET FONT
font_add_google(name = "Lato", family = "Lato")


# ------------------------------------------------------------------------------
# SHINY APP

# set Shiny options
options(shiny.sanitize.errors = TRUE)
options(scipen = 999)

# create user interface
ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")
  ),

  titlePanel(NULL),

  sidebarLayout(

    sidebarPanel(

      # text
      p(HTML(paste(a(href = "https://urban.co1.qualtrics.com/jfe/form/SV_dnWkyniOFkM4v4O", "Cast your vote here,"),
                   "then click Refresh Qualtrics Data to see the dashboard update!")), style = "font-size: 26px"),
      # p("Cast your vote here, then click Refresh Qualtrics Data to see the dashboard update!",
      #      style = "font-size: 26px;"),

      # white space
      br(),

      # refresh data button
      actionButton(inputId = "refresh_data", label = "Refresh Qualtrics data"),

      # white space
      br(), br(),

      # number of responses
      uiOutput(outputId = "n"),

      # date/time of last refresh
      uiOutput(outputId = "last_refresh"),

      # white space
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br()

    ),

    mainPanel(

      h2(strong("What is the best way to spend a day at the beach?")),
      h2(HTML("Basking in the sun", emoji("sunny"))),
      h2(HTML("Chilling in the shade", emoji("beach_umbrella"))),
      h2(HTML("Splashing in the waves", emoji("ocean"))),
      br(), br(), br(),

      # plot
      plotOutput(outputId = "plot")

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
    p(HTML(paste(strong(nrow(survey())), "responses as of last refresh:")), style = "font-size: 26px")
  })

  # date/time of last refresh
  output$last_refresh <- eventReactive(input$refresh_data, {
    HTML(format(Sys.time(), "%B %d, %Y %I:%M %p"))
  }, ignoreNULL = FALSE)

  # plot
  for_plot <- eventReactive(input$refresh_data, {
    survey() %>%
      group_by(Q1) %>%
      summarize(n = n()) %>%
      mutate(fac = factor(Q1, levels = c(1, 2, 3)))
  }, ignoreNULL = FALSE)

  output$plot <- renderPlot(
    (ggplot(data = for_plot(), aes(x = fac, y = n, fill = fac)) +
       geom_bar(stat = "identity") +
       scale_x_discrete(labels = c("Sun", "Shade", "Waves")) +
       scale_fill_manual(values = c(
         "#fdbf11",
         "#db2b27",
         "#1696d2"
       )) +
       scale_y_continuous(limits = c(0, (max(for_plot()$n)) * 1.1)) +
       theme(panel.background = element_blank(),
             legend.position = "none",
             axis.title = element_blank(),
             axis.ticks = element_blank(),
             axis.text.x = element_text(size = 40, family = "Lato"),
             axis.text.y = element_blank()) +
       geom_text(
         aes(label = n),
         size = 12,
         vjust = -1
       )
    )
  )

}

# build shiny application
shinyApp(ui = ui, server = server)
