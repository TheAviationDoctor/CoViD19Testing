###############################################################################
# HOUSEKEEPING CODE                                                           #
###############################################################################

# Load libraries
library(shiny)
library(readr)

# Clear the console
cat("\014")

###############################################################################
# VARIABLE DECLARATION                                                        #
###############################################################################

states <- list("Afghanistan","Albania","Algeria","Angola","Anguilla","Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bermuda","Bhutan","Bolivia","Bonaire Saint Eustatius & Saba","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Cayman Islands","Central African Republic","Chad","Chile","China","Chinese Taipei","Cocos (Keeling) Islands","Colombia","Comoros","Congo","Cook Islands","Costa Rica","Croatia","Cuba","Curacao","Cyprus","Czech Republic","Democratic Republic of the Congo","Denmark","Djibouti","Dominica","Dominican Republic","East Timor","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Falkland Islands","Fiji","Finland","France","French Guiana","French Polynesia","Gabon","Gambia","Georgia","Germany","Ghana","Gibraltar","Greece","Greenland","Grenada and South Grenadines","Guadeloupe","Guatemala","Guinea","Guinea Bissau","Guyana","Haiti","Honduras","Hong Kong (SAR)","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Ivory Coast (Cote d'Ivoire)","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Libya","Lithuania","Luxembourg","Macau (SAR)","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Martinique","Mauritania","Mauritius","Mayotte","Mexico","Micronesia","Moldova","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Caledonia","New Zealand","Nicaragua","Niger","Nigeria","Norfolk Island","North Korea","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Reunion","Romania","Russian Federation","Rwanda","Saint Helena","Saint Kitts and Nevis","Saint Lucia","Saint Pierre and Miquelon","Saint Vincent and Grenadines","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Sint Maarten","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Korea","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Sweden","Switzerland","Syria","Tajikistan","Tanzania","Thailand","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Turks and Caicos Islands","Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Wallis and Futuna Islands","Western Sahara","Western Samoa","Yemen","Zambia","Zimbabwe")

###############################################################################
# USER INTERFACE LOGIC                                                        #
###############################################################################

ui <- fluidPage(
    
    # App title
    titlePanel("Air Travel COVID-19 testing simulator"),
    hr(),
    
    # Layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for selecting inputs
        sidebarPanel(

            # Origin characteristics
            h3("Origin characteristics"),
            selectInput(inputId = "DepartureState", label = "Departure state", choices = states),
            selectInput(inputId = "DeparturePrevalenceChoice", label = "Departure disease prevalence", choices = list("Automatic (based on medical data for that state)", "Manual (enter your own)")),
            conditionalPanel(
                condition = "input.DeparturePrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "DeparturePrevalence", label = "Select a disease prevalence at departure", min=0, max=100, value=4),
            ),
            
            # Destination characteristics
            hr(),
            h3("Destination characteristics"),
            selectInput(inputId = "ArrivalState", label = "Arrival state", choices = states),
            selectInput(inputId = "ArrivalPrevalenceChoice", label = "Arrival disease prevalence", choices = list("Automatic (based on medical data for that state)", "Manual (enter your own)")),
            conditionalPanel(
                condition = "input.ArrivalPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "ArrivalPrevalence", label = "Select a disease prevalence at arrival", min=0, max=100, value=4),
            ),
            
            # Population characteristics
            hr(),
            h3("Population characteristics"),
            selectInput(inputId = "PopulationCountChoice", label = "Passenger count", choices = list("Automatic (based on 2019 O&D traffic)", "Manual (enter your own)")),
            conditionalPanel(
                condition = "input.PopulationCountChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "PopulationCount", label = "Select the passenger count", min=0, max=4.5*10^9, value=2.2*10^9),
            ),
            selectInput(inputId = "PopulationTestingChoice", label = "Proportion of passengers being tested", choices = list("Systematic testing (everyone)", "Sample testing (enter a percentage)")),
            conditionalPanel(
                condition = "input.PopulationTestingChoice == 'Sample testing (enter a percentage)'",
                sliderInput(inputId = "PopulationTestingRate", label = "Proportion of passengers being tested", min=0, max=100, value=100),
            ),

            # Pre-departure test characteristics
            hr(),
            h3("Pre-departure test characteristics"),
            selectInput(inputId = "DepartureTestMethod", label = "Select a testing method", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            conditionalPanel(
                condition = "input.DepartureTestMethod == 'Manual (design your own)'",
                sliderInput(inputId = "DepartureTestLimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^4, value=1000),
                sliderInput(inputId = "DepartureTestSensitivity", label = "Clinical sensitivity", min=0, max=100, value=70),
                sliderInput(inputId = "DepartureTestSpecificity", label = "Clinical specificity", min=0, max=100, value=95)
            ),
            conditionalPanel(
                condition = "input.DepartureTestMethod != 'None'",
                sliderInput(inputId = "DaysBeforeDeparture", label = "Days before departure (0 for day of travel)", min=0, max=7, step=1, value=1),
            ),

            # Post-arrival test characteristics
            hr(),
            h3("Post-arrival test characteristics"),
            selectInput(inputId = "ArrivalTestMethod", label = "Select a testing method", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            conditionalPanel(
                condition = "input.ArrivalTestMethod == 'Manual (design your own)'",
                sliderInput(inputId = "ArrivalTestLimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^4, value=1000),
                sliderInput(inputId = "ArrivalTestSensitivity", label = "Clinical sensitivity", min=0, max=100, value=70),
                sliderInput(inputId = "ArrivalTestSpecificity", label = "Clinical specificity", min=0, max=100, value=95)
            ),
            conditionalPanel(
                condition = "input.ArrivalTestMethod != 'None'",
                sliderInput(inputId = "DaysAfterArrival", label = "Days after arrival (0 for day of travel)", min=0, max=7, step=1, value=1),
            ),
            
        ),
        # Main panel for displaying outputs
        mainPanel(
            h3("Pre-departure outcomes"),
            p("Lorem ipsum dolor sit amet"),
            hr(),
            h3("Departure outcomes"),
            fluidRow(
                column(width = 8, "Probability that a traveler tests positive given they are infected (true positive):"),
                column(width = 1, offset = 2, textOutput("DepartureProbabilityPositiveGivenInfected"))
            ),
            fluidRow(
                column(width = 8, "Probability that a traveler tests positive given they are not infected (false positive):"),
                column(width = 1, offset = 2, textOutput("DepartureProbabilityPositiveGivenNotInfected"))
            ),
            fluidRow(
                column(width = 8, "Probability that a traveler tests negative given they are infected (false negative):"),
                column(width = 1, offset = 2, textOutput("DepartureProbabilityNegativeGivenInfected"))
            ),
            fluidRow(
                column(width = 8, "Probability that a traveler tests negative given they are not infected (true negative):"),
                column(width = 1, offset = 2, textOutput("DepartureProbabilityNegativeGivenNotInfected"))
            ),
            hr(),
            h3("On-board outcomes"),
            p("Lorem ipsum dolor sit amet"),
            # We could use load factors here
            hr(),
            h3("Arrival outcomes"),
            p("Lorem ipsum dolor sit amet"),
        )
    )
)

###############################################################################
# SERVER LOGIC                                                                #
###############################################################################

server <- function(input, output) {
    
    # Calculate pre-departure test outcomes
    output$DepartureProbabilityPositiveGivenInfected <- renderText({ input$DepartureTestSensitivity })
    output$DepartureProbabilityPositiveGivenNotInfected <- renderText({ 100 - input$DepartureTestSpecificity })
    output$DepartureProbabilityNegativeGivenInfected <- renderText({ 100 - input$DepartureTestSensitivity })
    output$DepartureProbabilityNegativeGivenNotInfected <- renderText({ input$DepartureTestSpecificity })
    
    DepartureProbabilitiesLabels <- c("Probability that a traveler tests positive given they are infected (true positive):","Probability that a traveler tests positive given they are not infected (false positive):","Probability that a traveler tests negative given they are infected (false negative):","Probability that a traveler tests negative given they are not infected (true negative):")

}

###############################################################################
# RUN THE APP                                                                 #
###############################################################################

shinyApp(ui, server)