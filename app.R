###############################################################################
# HOUSEKEEPING CODE                                                           #
###############################################################################

# Load libraries
library(shiny)
library(tidyverse)

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
                # Departure state characteristics
                h3("Departure state characteristics"),
                selectInput(inputId = "DepartureState", label = "State of origin", choices = origins),
                selectInput(inputId = "DepartureStatePrevalenceChoice", label = "Disease prevalence in the departure state", choices = list("Automatic (assumed from medical data)", "Manual (override with your own)")),
                sliderInput(inputId = "DepartureStatePrevalence", label = "", min=0, max=1, value=.1),

                # Arrival state characteristics
                hr(),
                h3("Arrival state characteristics"),
                selectInput(inputId = "ArrivalState", label = "State of destination", choices = origins),
                selectInput(inputId = "ArrivalStatePrevalenceChoice", label = "Disease prevalence in the arrival state", choices = list("Automatic (assumed from medical data)", "Manual (override with your own)")),
                sliderInput(inputId = "ArrivalStatePrevalence", label = "", min=0, max=1, value=.1),

                # Population characteristics
                hr(),
                h3("Population characteristics"),
                selectInput(inputId = "PopulationCountChoice", label = "Passenger count", choices = list("Automatic (based on AirportIS data)", "Manual (override with your own)")),
                sliderInput(inputId = "PopulationCount", label = "", min=0, max=4.5*10^9, value=2.2*10^9),
                sliderInput(inputId = "PopulationTestingRate", label = "Proportion of passengers being tested", min=0, max=100, value=100),

                # Test characteristics
                hr(),
                h3("Test characteristics"),
                sliderInput(inputId = "TestLimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^4, value=1000),
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