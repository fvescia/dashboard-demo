# LOAD PACKAGES
library(shiny)
library(qualtRics) # for retrieving and processing Qualtrics survey data
library(tidyverse) # for data wrangling and viz
library(showtext) # to get Lato


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

  fluidRow(

    # white space
    p("What's the best way to spend a day at the beach?!", align = "center"),

    # plot
    plotOutput(outputId = "plot"),

    # white space
    br(), br(),

    # refresh data button
    column(width = 12,
      actionButton(inputId = "refresh_data", label = "Refresh Qualtrics data"),
      align = "center")

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
         "#9d9d9d",
         "#1696d2"
       )) +
       scale_y_continuous(limits = c(0, (max(for_plot()$n)) * 1.3)) +
       theme(panel.background = element_blank(),
             legend.position = "none",
             axis.title = element_blank(),
             axis.ticks = element_blank(),
             axis.text.x = element_text(size = 40, family = "Lato", color = "black"),
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
