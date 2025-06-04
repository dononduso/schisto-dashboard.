
library(shiny)
library(ggplot2)
library(dplyr)

# Load data
data <- read.csv("data/schisto_data.csv")

ui <- fluidPage(
  titlePanel("Urogenital Schistosomiasis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:", choices = c("All", unique(data$Gender))),
      selectInput("water", "Water Contact:", choices = c("All", unique(data$WaterContact)))
    ),
    mainPanel(
      plotOutput("infectionPlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    df <- data
    if (input$gender != "All") {
      df <- df %>% filter(Gender == input$gender)
    }
    if (input$water != "All") {
      df <- df %>% filter(WaterContact == input$water)
    }
    df
  })

  output$infectionPlot <- renderPlot({
    ggplot(filtered(), aes(x = Age, fill = Infected)) +
      geom_histogram(binwidth = 2, position = "dodge") +
      labs(title = "Infection by Age", x = "Age", y = "Count")
  })

  output$summaryTable <- renderTable({
    filtered() %>%
      group_by(Infected) %>%
      summarise(Count = n())
  })
}

shinyApp(ui, server)
