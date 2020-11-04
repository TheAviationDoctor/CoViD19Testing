###############################################################################
# HOUSEKEEPING CODE                                                           #
###############################################################################

# Install pacakges
install.packages(tidyverse)

# Load libraries
library(shiny, tidyverse)

# Clear the console
cat("\014")

###############################################################################
# VARIABLE DECLARATION                                                        #
###############################################################################

 origins <- list("Afghanistan","Albania","Algeria","Angola","Anguilla","Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bermuda","Bhutan","Bolivia","Bonaire Saint Eustatius & Saba","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Cayman Islands","Central African Republic","Chad","Chile","China","Chinese Taipei","Cocos (Keeling) Islands","Colombia","Comoros","Congo","Cook Islands","Costa Rica","Croatia","Cuba","Curacao","Cyprus","Czech Republic","Democratic Republic of the Congo","Denmark","Djibouti","Dominica","Dominican Republic","East Timor","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Falkland Islands","Fiji","Finland","France","French Guiana","French Polynesia","Gabon","Gambia","Georgia","Germany","Ghana","Gibraltar","Greece","Greenland","Grenada and South Grenadines","Guadeloupe","Guatemala","Guinea","Guinea Bissau","Guyana","Haiti","Honduras","Hong Kong (SAR)","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Ivory Coast (Cote d'Ivoire)","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Libya","Lithuania","Luxembourg","Macau (SAR)","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Martinique","Mauritania","Mauritius","Mayotte","Mexico","Micronesia","Moldova","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Caledonia","New Zealand","Nicaragua","Niger","Nigeria","Norfolk Island","North Korea","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Reunion","Romania","Russian Federation","Rwanda","Saint Helena","Saint Kitts and Nevis","Saint Lucia","Saint Pierre and Miquelon","Saint Vincent and Grenadines","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Sint Maarten","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Korea","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Sweden","Switzerland","Syria","Tajikistan","Tanzania","Thailand","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Turks and Caicos Islands","Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Wallis and Futuna Islands","Western Sahara","Western Samoa","Yemen","Zambia","Zimbabwe")

###############################################################################
# APPLICATION CODE                                                            #
###############################################################################

if (interactive()) {
  options(device.ask.default = FALSE)
  
  #############################################################################
  # USER INTERFACE LOGIC                                                      #
  #############################################################################
  
  ui <- fluidPage(
    
    # App title
    titlePanel("SARS-CoV-2 Air Travel Screening Simulator"),
    
    # Layout with input and output definitions
    sidebarLayout(
      
      # Sidebar panel for selecting inputs
      sidebarPanel(
        # Country pair
        h3("Origin characteristics"),
        selectInput(inputId = "OriginState", label = "State of origin", choices = origins),
        selectInput(inputId = "OriginPrevalenceChoice", label = "Disease prevalence at origin", choices = list("Automatic", "Manual")),
        sliderInput(inputId = "OriginPrevalence", label = "", min=0, max=1, value=.1),
        hr(),
        h3("Destination characteristics"),
        selectInput(inputId = "DestinationState", label = "State of destination", choices = origins),
        selectInput(inputId = "DestinationPrevalenceChoice", label = "Disease prevalence at destination", choices = list("Automatic", "Manual")),
        sliderInput(inputId = "DestinationPrevalence", label = "", min=0, max=1, value=.1),
        hr(),
        h3("Traffic volume"),
        selectInput(inputId = "PassengerCountChoice", label = "Passenger count", choices = list("Automatic", "Manual")),
        sliderInput(inputId = "PassengerCount", label = "", min=0, max=2200000000000, value=2200000000000),
        # Test design
        h3("Select a test design:"),
        sliderInput(inputId = "LimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^3, value=10),
        sliderInput(inputId = "SamplePrevalence", label = "Sample prevalence", min=0, max=1, value=.1),
        sliderInput(inputId = "TestSensitivity", label = "Test sensitivity", min=0, max=1, value=.7),
        sliderInput(inputId = "TestSpecificity", label = "Test specificity", min=0, max=1, value=.95)
      ),
      # Main panel for displaying outputs
      mainPanel(
        ls(all.names = TRUE),
        textOutput("ProbabilityPositiveGivenInfected")
      )
    )
  )

  #############################################################################
  # SERVER LOGIC                                                              #
  #############################################################################
  
  server <- function(input, output) {
    
    output$ProbabilityPositiveGivenInfected <- renderText({ input$TestSensitivity })
    # output$ProbabilityPositiveGivenNotInfected <- renderText({ 1- input$TestSpecificity })
    # output$ProbabilityNegativeGivenInfected <- renderText({ 1 - input$TestSensitivity })
    # output$ProbabilityNegativeGivenNotInfected <- renderText({ input$TestSpecitificity })
    
  }

  #############################################################################
  # RUN APPLICATION                                                           #
  #############################################################################
  
  shinyApp(ui, server)
}