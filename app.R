# Clear the console
cat("\014")

if (interactive()) {
  options(device.ask.default = FALSE)
  
  
  # Interface logic
  ui <- fluidPage(
    
    # App title
    titlePanel("SARS-CoV-2 Testing Simulation"),
    
    # Layout with input and output definitions
    sidebarLayout(
      
      # Sidebar panel for selecting inputs
      sidebarPanel(
        sliderInput(inputId = 'SampleSize', label = 'Sample size', min=0, max=2200000000000, value=2200000000000),
        sliderInput(inputId = 'SamplePrevalence', label = 'Sample prevalence', min=0, max=1, value=.1),
        sliderInput(inputId = 'TestSensitivity', label = 'Test sensitivity', min=0, max=1, value=.7),
        sliderInput(inputId = 'TestSpecificity', label = 'Test specificity', min=0, max=1, value=.95)
      ),
      # Main panel for displaying outputs
      mainPanel(
        ls(all.names = TRUE),
        paste("ProbabilityPositiveGivenInfected")
      )
    )
  )
  
  # Server logic
  server <- function(input, output) {
    
    output$ProbabilityPositiveGivenInfected <- verbatimTextOutput({ input$TestSensitivity })
    output$ProbabilityPositiveGivenNotInfected <- verbatimTextOutput({ 1- input$TestSpecificity })
    output$ProbabilityNegativeGivenInfected <- verbatimTextOutput({ 1 - input$TestSensitivity })
    output$ProbabilityNegativeGivenNotInfected <- verbatimTextOutput({ input$TestSpecitificity })
    
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}