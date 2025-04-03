# Minh Nguyen
# Load required libraries
library(shiny)
library(ggplot2)
# Define UI
ui <- fluidPage(
  titlePanel("Interactive Histogram with ggplot2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a Variable:",
                  choices = c("MPG" = "mpg", "Horsepower" = "hp",
                              "Weight" = "wt"),
                  selected = "mpg") # Default selection
    ),
    mainPanel(
      plotOutput("histPlot") # Interactive plot
    )
  )
)
# Define server logic
server <- function(input, output) {
  output$histPlot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$variable)) + # Uses user-selected
    geom_histogram(fill = "blue", color = "black", bins = 10) +
      labs(title = paste("Histogram of", input$variable), x = input$variable, y =
             "Count")
  })
}
# Run the app
shinyApp(ui = ui, server = server)
