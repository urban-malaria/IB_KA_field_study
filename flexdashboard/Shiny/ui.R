library(flexdashboard)
library(shiny)
library(plotly)
library(DT)

#ui.R

fluidPage(
  titlePanel("Microstratification field study dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload a CSV file:")
    ),
    mainPanel(
      #textOutput("summary_text"),
      dataTableOutput("summary_table"),
      uiOutput("plots")
    )
  ),

  tags$head(
    tags$style(
      HTML(
        ".plot-row {
          margin-bottom: 30px; /* Adjust the margin as needed */
        }"
      )
    )
  )
)

