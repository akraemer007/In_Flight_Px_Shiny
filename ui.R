library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Inflight Px CareCard Completion"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "Client",
        "Select a client",
        c('Client 1', 'Client 2', 'Client 3', 'Client 4', 'Client 5', 'Client 6', 'Client 7', 'Client 8', 'Client 9', 'Client 10', 'Client 11', 'Client 12', 'Client 13', 'Client 14', 'Client 15', 'Client 16', 'Client 17', 'Client 18', 'Client 19')
      )
      # ,
      # selectInput(
      #   "Denominator",
      #   "Due / Expired as Denominator",
      #   c('completion_percentage','completion_percentage.due')
      # )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      ggiraph::ggiraphOutput("CarePath"
                 # , click = "plot_click"
                 )
      # ,
      # verbatimTextOutput("info")
    ))
))