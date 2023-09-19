# server.R
 
# library(tidyverse)
# library(plotly)
# 
#   function(input, output, session) {
#     uploaded_data <- reactive({
#       req(input$data_file)
#       infile <- input$data_file
#       data <- read_csv(infile$datapath)
#       return(data)
#     })
# 
#     output$summary_table <- renderDataTable({
#       datatable(uploaded_data())
#     })
# 
# 
# 
#     output$plots <- renderUI({
# 
#       data <- uploaded_data()
# 
#       # Filter the data for Location == "Ibadan"
#       data <- data %>% filter(Location == "Ibadan")
# 
#       # Perform data transformations
#       data <- data %>%
#         group_by(`NAME OF HEALTH FACILITY`, `Date`) %>%
#         summarise(total_pts = n()) %>%
#         ungroup()
# 
#       data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
#       data$weeks <- format(data$Date, "%U")
# 
#       data <- data %>%
#         group_by(`NAME OF HEALTH FACILITY`, `weeks`) %>%
#         summarise(total_pts = sum(total_pts)) %>%
#         ungroup()
# 
#       data <- data %>%
#         mutate(Target = case_when(
#           `NAME OF HEALTH FACILITY` == "AGB" ~ 9,
#           `NAME OF HEALTH FACILITY` == "AMH" ~ 23,
#           `NAME OF HEALTH FACILITY` == "JSH" ~ 13,
#           `NAME OF HEALTH FACILITY` == "NMC" ~ 6,
#           `NAME OF HEALTH FACILITY` == "OADU" ~ 3,
#           `NAME OF HEALTH FACILITY` == "OP" ~ 15,
#           `NAME OF HEALTH FACILITY` == "OCH" ~ 8,
#           `NAME OF HEALTH FACILITY` == "ONCHC" ~ 4,
#           `NAME OF HEALTH FACILITY` == "UCH" ~ 10,
#           `NAME OF HEALTH FACILITY` == "RRSH" ~ 11
#         ),
# 
#         name = case_when(
#           `NAME OF HEALTH FACILITY` == "AGB" ~ "Agbongbon PHC",
#           `NAME OF HEALTH FACILITY` == "AMH" ~ "Adeoyo Maternity",
#           `NAME OF HEALTH FACILITY` == "JSH" ~ "Jericho Specialist",
#           `NAME OF HEALTH FACILITY` == "NMC" ~ "Naomi Medical Center",
#           `NAME OF HEALTH FACILITY` == "OADU" ~ "Oke Adu PHC",
#           `NAME OF HEALTH FACILITY` == "OP" ~ "Oranmiyan CHC",
#           `NAME OF HEALTH FACILITY` == "OCH" ~ "Oluyoro Catholic Hospital",
#           `NAME OF HEALTH FACILITY` == "ONCHC" ~ "Oniyanrin Comprehensive HC",
#           `NAME OF HEALTH FACILITY` == "UCH" ~ "University College Hospital",
#           `NAME OF HEALTH FACILITY` == "RRSH" ~ "Ring Road Specialist Hospital"
#         )
#         )
# 
#       plots <- lapply(unique(data$name), function(category) {
#         data <- data %>% filter(name == category)
# 
#         # Create a scatter plot
#         p <- plot_ly(data = data, x = ~weeks, y = ~as.numeric(total_pts), type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
#           layout(title = category)
# 
#         # Add a horizontal line at y (customize as needed)
#         p <- p %>% add_trace(y = ~Target, mode = "lines", line = list(dash = "dash"), name = "Target")
# 
#         # Customize x and y axis labels
#         p <- p %>% layout(xaxis = list(title = "Week of Visit"), yaxis = list(title = "Number of Patients", range = c(0, max(as.numeric(data$total_pts) + 5))))
# 
#         p
#       })
# 
#       # Create a list to hold the rows of plots, with two plots per row
#       plot_rows <- list()
#       num_plots <- length(plots)
#       for (i in seq(1, num_plots, by = 2)) {
#         # Create a row of plots with two columns
#         plot_row <- tagList(
#           div(class = "row plot-row",
#               div(class = "col-md-6", plots[[i]]),
#               div(class = "col-md-6", if (i + 1 <= num_plots) plots[[i + 1]])
#           )
#         )
#         plot_rows <- append(plot_rows, plot_row)
#       }
# 
#       # Return the grid layout
#       do.call(tagList, plot_rows)
#     })
# 
# }




# server.R

library(tidyverse)
library(plotly)

function(input, output, session) {
  # Define a reactive expression for data loading
  uploaded_data <- reactive({
    req(input$data_file)
    infile <- input$data_file
    data <- read_csv(infile$datapath)
    return(data)
  })
  
  # Define a reactive expression for data filtering within the "Hospital" subtab
  hospital_data <- reactive({
    data <- uploaded_data()
    # Filter the data for Location == "Ibadan" (or your desired condition)
    return(data %>% filter(Location == "Ibadan"))
  })
  
  # Render content for the "Hospital" subtab
  output$hospital <- renderUI({
    data <- hospital_data()
    
    # Perform data transformations specific to the "Hospital" subtab...
    # For example, you can use the same plotting code as before
    
    # Create a list to hold the rows of plots, with two plots per row
    plot_rows <- list()
    num_plots <- length(plots)
    for (i in seq(1, num_plots, by = 2)) {
      # Create a row of plots with two columns
      plot_row <- tagList(
        div(class = "row plot-row",
            div(class = "col-md-6", plots[[i]]),
            div(class = "col-md-6", if (i + 1 <= num_plots) plots[[i + 1]])
        )
      )
      plot_rows <- append(plot_rows, plot_row)
    }
    
    # Return the grid layout for the "Hospital" subtab
    do.call(tagList, plot_rows)
  })
  
  # Render the summary table for the "Hospital" subtab
  output$summary_table <- renderDataTable({
    data <- hospital_data()
    # Perform data transformations specific to the "Hospital" subtab...
    return(data)
  })
}























