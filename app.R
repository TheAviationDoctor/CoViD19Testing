###############################################################################
# HOUSEKEEPING CODE                                                           #
###############################################################################

# Load libraries
library(DT)
library(shiny)
library(tidyverse)

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
    
    # Globally style the app
    tags$head(
        tags$style("* { font-family: 'Aktiv Grotesk', Arial, sans-serif; !important }"),
        tags$style("hr { border: 1px solid #000000 }"),
        tags$style("select { color: #1E32FA; }")
    ),
    
    # App title
    titlePanel("Air travel COVID-19 screening simulator"),
    hr(),
    
    sidebarLayout(

        ###############################################################################
        # SIDEBAR PANEL FOR INPUTS                                                    #
        ###############################################################################
        
        sidebarPanel(

            # Title
            h2("Model inputs"),
            hr(),
            
            # Origin characteristics
            h3("Origin"),
            selectInput(inputId = "DeparturePrevalenceChoice", label = "Disease prevalence at origin", choices = list("Manual (enter your own)", "Automatic (based on state data)")),
            conditionalPanel(
                condition = "input.DeparturePrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "DeparturePrevalence", label = "Select a disease prevalence at origin", min=0, max=1, value=.04),
            ),
            conditionalPanel(
                condition = "input.DeparturePrevalenceChoice == 'Automatic (based on state data)'",
                selectInput(inputId = "DepartureState", label = "Select a departure state", choices = states),
            ),
            
            # Destination characteristics
            hr(),
            h3("Destination"),
            selectInput(inputId = "ArrivalPrevalenceChoice", label = "Disease prevalence at destination", choices = list("Manual (enter your own)", "Automatic (based on state data)")),
            conditionalPanel(
                condition = "input.ArrivalPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "ArrivalPrevalence", label = "Select a disease prevalence at destination", min=0, max=1, value=.12),
            ),
            conditionalPanel(
                condition = "input.ArrivalPrevalenceChoice == 'Automatic (based on state data)'",
                selectInput(inputId = "ArrivalState", label = "Select an arrival state", choices = states),
            ),
            
            # Population characteristics
            hr(),
            h3("Population"),
            selectInput(inputId = "PopulationCountChoice", label = "Air traveler count", choices = list("Manual (enter your own)", "Automatic (based on 2019 O&D traffic for that pair)")),
            conditionalPanel(
                condition = "input.PopulationCountChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "PopulationCount", label = "Select the air traveler count", min=0, max=4.5*10^9, value=2.2*10^9),
            ),

            # Pre-departure test characteristics
            hr(),
            h3("Pre-departure test"),
            selectInput(inputId = "DepartureTestMethod", label = "Select a testing method", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            conditionalPanel(
                condition = "input.DepartureTestMethod == 'Manual (design your own)'",
                sliderInput(inputId = "DepartureTestLimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^4, value=1000),
                sliderInput(inputId = "DepartureTestSensitivity", label = "Clinical sensitivity", min=0, max=1, value=.7),
                sliderInput(inputId = "DepartureTestSpecificity", label = "Clinical specificity", min=0, max=1, value=.95)
            ),
            conditionalPanel(
                condition = "input.DepartureTestMethod != 'None'",
                sliderInput(inputId = "DaysBeforeDeparture", label = "Days before departure (0 for day of travel)", min=0, max=7, step=1, value=1),
            ),
            selectInput(inputId = "DepartureTestSampleChoice", label = "Proportion of departing travelers being tested", choices = list("Systematic testing (all air travelers)", "Sample testing (enter a percentage)")),
            conditionalPanel(
                condition = "input.DepartureTestSampleChoice == 'Sample testing (enter a percentage)'",
                sliderInput(inputId = "DepartureTestSampleSize", label = "Select a proportion of departing travelers to test", min=0, max=100, value=100),
            ),
            

            # Post-arrival test characteristics
            hr(),
            h3("Post-arrival test"),
            selectInput(inputId = "ArrivalTestMethod", label = "Select a testing method", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            conditionalPanel(
                condition = "input.ArrivalTestMethod == 'Manual (design your own)'",
                sliderInput(inputId = "ArrivalTestLimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^4, value=1000),
                sliderInput(inputId = "ArrivalTestSensitivity", label = "Clinical sensitivity", min=0, max=1, value=.7),
                sliderInput(inputId = "ArrivalTestSpecificity", label = "Clinical specificity", min=0, max=1, value=.95)
            ),
            conditionalPanel(
                condition = "input.ArrivalTestMethod != 'None'",
                sliderInput(inputId = "DaysAfterArrival", label = "Days after arrival (0 for day of travel)", min=0, max=7, step=1, value=1),
            ),
            selectInput(inputId = "ArrivalTestTestSampleChoice", label = "Proportion of arriving travelers being tested", choices = list("Systematic testing (all air travelers)", "Sample testing (enter a percentage)")),
            conditionalPanel(
                condition = "input.ArrivalTestTestSampleChoice == 'Sample testing (enter a percentage)'",
                sliderInput(inputId = "ArrivalTestTestSampleSize", label = "Select a proportion of arriving travelers to test", min=0, max=100, value=100),
            ),
            
        ),
        
        #######################################################################
        # MAIN PANEL FOR OUTPUTS                                              #
        #######################################################################
        
        mainPanel(
            
            # Title
            h2("Model outputs"),
            hr(),
            
            ###################################################################
            # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                              #
            ###################################################################
            
            h3("1. Pre-departure, pre-test outcomes"),
            fluidRow(
                column(12, align = "center",
                    em("1.1. Probability that a departing air traveler is infected, based on the disease prevalence at origin."),
                ),
            ),
            fluidRow(
                column(6,
                    dataTableOutput("PreDepartureDiseasePrevalencePercentageTable"),
                ),
                column(6,
                       dataTableOutput("PreDepartureDiseasePrevalenceHeadcountTable"),
                )
            ),
            hr(),

            ###################################################################
            # 2. PRE-DEPARTURE POST-TEST OUTCOMES                             #
            ###################################################################

            h3("2. Pre-departure, post-test outcomes"),
            fluidRow(
                column(12, align = "center",
                       em("2.1. Probability that a departing air traveler tests positive/negative."),
                ),
            ),
            fluidRow(
                column(6,
                       dataTableOutput("PreDepartureTestPercentageTable"),
                ),
                column(6,
                       dataTableOutput("PreDepartureTestHeadcountTable"),
                )
            ),
            br(),
            fluidRow(
                column(12, align = "center",
                       em("2.2. Probability that a departing air traveler tests positive/negative, given that they are infected/uninfected."),
                ),
            ),
            fluidRow(
                column(6,
                       dataTableOutput("PreDepartureTestPriorPercentageTable"),
                ),
                column(6,
                       dataTableOutput("PreDepartureTestPriorHeadcountTable"),
                )
            ),
            br(),
            fluidRow(
                column(12, align = "center",
                       em("2.3. Probability that a departing air traveler is infected/uninfected, given that they test positive/negative."),
                ),
            ),
            fluidRow(
                column(6,
                       dataTableOutput("PreDepartureTestPosteriorPercentageTable"),
                ),
                column(6,
                       dataTableOutput("PreDepartureTestPosteriorHeadcountTable"),
                )
            ),
            hr(),

            ###################################################################
            # 3. POST-ARRIVAL PRE-TEST OUTCOMES                               #
            ###################################################################

            h3("3. Post-arrival, pre-test outcomes"),
            fluidRow(
                column(6,
                       em("3.1. Disease prevalence among air travelers upon arrival (percentage)."),
                ),
                column(6,
                       em("3.2. Disease prevalence among air travelers upon arrival (headcount)"),
                )
            ),
            fluidRow(
                column(6,
                       dataTableOutput("PostArrivalDiseasePrevalencePercentageTable"),
                ),
                column(6,
                       dataTableOutput("PostArrivalDiseasePrevalenceHeadcountTable"),
                )
            ),
            hr(),
            
            ###################################################################
            # 4. POST-ARRIVAL POST-TEST OUTCOMES                              #
            ###################################################################

            h3("4. Post-arrival, post-test outcomes"),
            p("This will show the risk on arrival at destination."),
            hr(),
        )
    )
)

###############################################################################
# SERVER LOGIC                                                                #
###############################################################################

server <- function(input, output) {

    ###########################################################################
    # INPUT VARIABLES                                                         #
    ###########################################################################

    output$DepartureState <- renderText({ input$DepartureState })
        
    ###########################################################################
    # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                                      #
    ###########################################################################

    # 1.1a Build a data frame to store the probability that a departing air traveler is infected (percentage)
    PreDepartureDiseasePrevalencePercentageTable <- reactive({
        PreDepartureDiseasePrevalencePercentageLabels = "Percentage"
        PreDepartureDiseasePrevalencePercentageInfectedValues = input$DeparturePrevalence
        PreDepartureDiseasePrevalencePercentageUninfectedValues = 1 - input$DeparturePrevalence
        PreDepartureDiseasePrevalencePercentageTotal = PreDepartureDiseasePrevalencePercentageInfectedValues + PreDepartureDiseasePrevalencePercentageUninfectedValues
        datatable(data.frame(Prevalence = PreDepartureDiseasePrevalencePercentageLabels, Infected = PreDepartureDiseasePrevalencePercentageInfectedValues, Uninfected = PreDepartureDiseasePrevalencePercentageUninfectedValues, Total = PreDepartureDiseasePrevalencePercentageTotal), rownames = NULL, options = list(dom = 't')) %>% formatPercentage(columns = c("Infected", "Uninfected","Total"), digits = 1) %>% formatStyle(columns = c("Infected", "Uninfected", "Total"),  color = "#1E32FA")
    })
    
    # 1.1b Render a table to display the probability that a departing air traveler is infected (percentage)
    output$PreDepartureDiseasePrevalencePercentageTable <- renderDataTable(
        PreDepartureDiseasePrevalencePercentageTable()
    )
    
    # 1.2a Build a data frame to store the probability that a departing air traveler is infected (headcount)
    PreDepartureDiseasePrevalenceHeadcountTable <- reactive({
        PreDepartureDiseasePrevalenceHeadcountLabels = "Headcount"
        PreDepartureDiseasePrevalenceHeadcountInfectedValues = input$DeparturePrevalence * input$PopulationCount
        PreDepartureDiseasePrevalenceHeadcountUninfectedValues = (1 - input$DeparturePrevalence) * input$PopulationCount
        PreDepartureDiseasePrevalenceHeadcountTotal = PreDepartureDiseasePrevalenceHeadcountInfectedValues + PreDepartureDiseasePrevalenceHeadcountUninfectedValues
        datatable(data.frame(Prevalence = PreDepartureDiseasePrevalenceHeadcountLabels, Infected = PreDepartureDiseasePrevalenceHeadcountInfectedValues, Uninfected = PreDepartureDiseasePrevalenceHeadcountUninfectedValues, Total = PreDepartureDiseasePrevalenceHeadcountTotal), rownames = NULL, options = list(dom = 't')) %>% formatCurrency(columns = c("Infected", "Uninfected", "Total"), currency = "", digits = 0) %>% formatStyle(columns = c("Infected", "Uninfected", "Total"),  color = "#1E32FA")
    })
    
    # 1.2b Render a table to display the probability that a departing air traveler is infected (headcount)
    output$PreDepartureDiseasePrevalenceHeadcountTable <- renderDataTable(
        PreDepartureDiseasePrevalenceHeadcountTable()
    )
    
    ###########################################################################
    # 2. PRE-DEPARTURE POST-TEST OUTCOMES                                     #
    ###########################################################################
    
    # 2.1a Build a data frame to store the probability that a test is positive or negative (percentage)
    PreDepartureTestPercentageTable <- reactive({
        PreDepartureTestPercentageLabels = "Percentage"
        PreDepartureTestPercentagePositiveValues = input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)
        PreDepartureTestPercentageNegativeValues = (1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)
        PreDepartureTestPercentageTotal = PreDepartureTestPercentagePositiveValues + PreDepartureTestPercentageNegativeValues
        datatable(data.frame(Test = PreDepartureTestPercentageLabels, Positive = PreDepartureTestPercentagePositiveValues, Negative = PreDepartureTestPercentageNegativeValues, Total = PreDepartureTestPercentageTotal), rownames = NULL, options = list(dom = 't')) %>% formatPercentage(columns = c("Positive", "Negative","Total"), digits = 1) %>% formatStyle(columns = c("Positive", "Negative", "Total"),  color = "#1E32FA")
    })
    
    # 2.1b Render a table to display the probability that a test is positive or negative (percentage)
    output$PreDepartureTestPercentageTable <- renderDataTable(
        PreDepartureTestPercentageTable()
    )
    
    # 2.1c Build a data frame to store the probability that a test is positive or negative (headcount)
    PreDepartureTestHeadcountTable <- reactive({
        PreDepartureTestHeadcountLabels = "Headcount"
        PreDepartureTestHeadcountPositiveValues = (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)) * input$PopulationCount
        PreDepartureTestHeadcountNegativeValues = ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)) * input$PopulationCount
        PreDepartureTestHeadcountTotal = PreDepartureTestHeadcountPositiveValues + PreDepartureTestHeadcountNegativeValues
        datatable(data.frame(Test = PreDepartureTestHeadcountLabels, Positive = PreDepartureTestHeadcountPositiveValues, Negative = PreDepartureTestHeadcountNegativeValues, Total = PreDepartureTestHeadcountTotal), rownames = NULL, options = list(dom = 't')) %>% formatCurrency(columns = c("Positive", "Negative", "Total"), currency = "", digits = 0) %>% formatStyle(columns = c("Positive", "Negative", "Total"),  color = "#1E32FA")
    })
    
    # 2.1d Render a table to display the probability that a test is positive or negative (headcount)
    output$PreDepartureTestHeadcountTable <- renderDataTable(
        PreDepartureTestHeadcountTable()
    )
    
    # 2.2a Build a data frame to store the pre-departure test prior probabilities (percentage)
    PreDepartureTestPriorPercentage <- reactive({
        PreDepartureTestPriorPercentageTestLabels = c("Positive", "Negative", "Total")
        PreDepartureTestPriorPercentageInfectedValues = c(input$DepartureTestSensitivity, 1 - input$DepartureTestSensitivity, input$DepartureTestSensitivity + 1 - input$DepartureTestSensitivity)
        PreDepartureTestPriorPercentageUninfectedValues = c(1 - input$DepartureTestSpecificity, input$DepartureTestSpecificity, input$DepartureTestSpecificity + 1 - input$DepartureTestSpecificity)
        datatable(data.frame(Test = PreDepartureTestPriorPercentageTestLabels, Infected = PreDepartureTestPriorPercentageInfectedValues,  Uninfected = PreDepartureTestPriorPercentageUninfectedValues), rownames = NULL, options = list(dom = 't')) %>% formatPercentage(columns = c("Infected", "Uninfected"), digits = 1) %>% formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
    })
    
    # 2.2b Render a table to display the pre-departure test prior probabilities (percentage)
    output$PreDepartureTestPriorPercentageTable <- renderDataTable(
        PreDepartureTestPriorPercentage()
    )
    
    # 2.2c Build a data frame to store the pre-departure test probabilities (headcount)
    PreDepartureTestPriorHeadcount <- reactive({
        PreDepartureTestPriorHeadcountTestLabels = c("Positive", "Negative", "Total")
        PreDepartureTestPriorHeadcountInfectedValues = c(input$DepartureTestSensitivity * input$PopulationCount * input$DeparturePrevalence, (1 - input$DepartureTestSensitivity) * input$PopulationCount * input$DeparturePrevalence, input$DepartureTestSensitivity * input$PopulationCount * input$DeparturePrevalence + (1 - input$DepartureTestSensitivity) * input$PopulationCount * input$DeparturePrevalence)
        PreDepartureTestPriorHeadcountUninfectedValues = c((1 - input$DepartureTestSpecificity) * input$PopulationCount * (1 - input$DeparturePrevalence), input$DepartureTestSpecificity * input$PopulationCount * (1 - input$DeparturePrevalence), (1 - input$DepartureTestSpecificity) * input$PopulationCount * (1 - input$DeparturePrevalence) + input$DepartureTestSpecificity * input$PopulationCount * (1 - input$DeparturePrevalence))
        datatable(data.frame(Test = PreDepartureTestPriorHeadcountTestLabels, Infected = PreDepartureTestPriorHeadcountInfectedValues,  Uninfected = PreDepartureTestPriorHeadcountUninfectedValues), rownames = NULL, options = list(dom = 't')) %>% formatCurrency(columns = c("Infected", "Uninfected"), currency = "", digits = 0) %>% formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
    })
    
    # 2.2d Render a table to display the pre-departure test prior probabilities (headcount)
    output$PreDepartureTestPriorHeadcountTable <- renderDataTable(
        PreDepartureTestPriorHeadcount()
    )
    
    # 2.3a Build a data frame to store the pre-departure test posterior probabilities (percentage)
    PreDepartureTestPosteriorPercentage <- reactive({
        PreDepartureTestPosteriorPercentageLabels = c("Infected", "Uninfected", "Total")
        PreDepartureTestPosteriorPercentagePositiveValues = c(input$DepartureTestSensitivity * input$DeparturePrevalence / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)), (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence) / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)), input$DepartureTestSensitivity * input$DeparturePrevalence / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)) + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence) / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)))
        PreDepartureTestPosteriorPercentageNegativeValues = c((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)), input$DepartureTestSpecificity * (1 - input$DeparturePrevalence) / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)), (1 - input$DepartureTestSensitivity) * input$DeparturePrevalence / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)) + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence) / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)))
        datatable(data.frame(Test = PreDepartureTestPosteriorPercentageLabels, Positive = PreDepartureTestPosteriorPercentagePositiveValues,  Negative = PreDepartureTestPosteriorPercentageNegativeValues), rownames = NULL, options = list(dom = 't')) %>% formatPercentage(columns = c("Positive", "Negative"), digits = 1) %>% formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
    })

    # 2.3b Render a table to store the pre-departure test posterior probabilities (percentage)
    output$PreDepartureTestPosteriorPercentageTable <- renderDataTable(
        PreDepartureTestPosteriorPercentage()
    )

    # 2.3c Build a data frame to store the pre-departure test posterior probabilities (headcount)
    PreDepartureTestPosteriorHeadcount <- reactive({
        PreDepartureTestPosteriorHeadcountLabels = c("Infected", "Uninfected", "Total")
        PreDepartureTestPosteriorHeadcountPositiveValues = c(input$DepartureTestSensitivity * input$DeparturePrevalence / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)) * input$PopulationCount, (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence) / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)) * input$PopulationCount, input$DepartureTestSensitivity * input$DeparturePrevalence / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)) * input$PopulationCount + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence) / (input$DepartureTestSensitivity * input$DeparturePrevalence + (1 - input$DepartureTestSpecificity) * (1 - input$DeparturePrevalence)) * input$PopulationCount)
        PreDepartureTestPosteriorHeadcountNegativeValues = c((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)) * input$PopulationCount, input$DepartureTestSpecificity * (1 - input$DeparturePrevalence) / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)) * input$PopulationCount, (1 - input$DepartureTestSensitivity) * input$DeparturePrevalence / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)) * input$PopulationCount + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence) / ((1 - input$DepartureTestSensitivity) * input$DeparturePrevalence + input$DepartureTestSpecificity * (1 - input$DeparturePrevalence)) * input$PopulationCount)
        datatable(data.frame(Test = PreDepartureTestPosteriorHeadcountLabels, Positive = PreDepartureTestPosteriorHeadcountPositiveValues,  Negative = PreDepartureTestPosteriorHeadcountNegativeValues), rownames = NULL, options = list(dom = 't')) %>% formatCurrency(columns = c("Positive", "Negative"), currency = "", digits = 0) %>% formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
    })
    
    # 2.3d Render a table to store the pre-departure test posterior probabilities (headcount)
    output$PreDepartureTestPosteriorHeadcountTable <- renderDataTable(
        PreDepartureTestPosteriorHeadcount()
    )
    
    ###########################################################################
    # 3. POST-ARRIVAL PRE-TEST OUTCOMES                                       #
    ###########################################################################
    

}

###############################################################################
# RUN THE APP                                                                 #
###############################################################################

shinyApp(ui, server)