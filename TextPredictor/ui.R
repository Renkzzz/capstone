library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel('Predict Next Word'),
  sidebarLayout(
    sidebarPanel(
      textInput('sentence', 'Enter Sentence Fragment'),
      numericInput('n', 'Number of Predictions', value = 5),
    ),
    mainPanel(
      h4('Predicted Next Words:'),
      verbatimTextOutput('prediction')
    )
  )
)
