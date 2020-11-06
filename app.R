###############################################################################
# AIR TRAVEL COVID-19 TESTING SIMULATOR                                       #
# Created by: Thomas D. Pellegrin                                             #
#             contact@theaviationdoctor.com                                   #
#             https://theaviationdoctor.com                                   #
###############################################################################

###############################################################################
# HOUSEKEEPING CODE                                                           #
###############################################################################

# Load libraries
library(DT)
library(pins)
library(shiny)
library(shinycssloaders)
library(tidyverse)

# Clear the console
#cat("\014")

###############################################################################
# VARIABLE DECLARATION                                                        #
###############################################################################

AppTitle <- "Air travel COVID-19 testing simulator"
states <- list("Afghanistan","Albania","Algeria","Angola","Anguilla","Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bermuda","Bhutan","Bolivia","Bonaire Saint Eustatius & Saba","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Cayman Islands","Central African Republic","Chad","Chile","China","Chinese Taipei","Cocos (Keeling) Islands","Colombia","Comoros","Congo","Cook Islands","Costa Rica","Croatia","Cuba","Curacao","Cyprus","Czech Republic","Democratic Republic of the Congo","Denmark","Djibouti","Dominica","Dominican Republic","East Timor","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Falkland Islands","Fiji","Finland","France","French Guiana","French Polynesia","Gabon","Gambia","Georgia","Germany","Ghana","Gibraltar","Greece","Greenland","Grenada and South Grenadines","Guadeloupe","Guatemala","Guinea","Guinea Bissau","Guyana","Haiti","Honduras","Hong Kong (SAR)","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Ivory Coast (Cote d'Ivoire)","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Libya","Lithuania","Luxembourg","Macau (SAR)","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Martinique","Mauritania","Mauritius","Mayotte","Mexico","Micronesia","Moldova","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Caledonia","New Zealand","Nicaragua","Niger","Nigeria","Norfolk Island","North Korea","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Reunion","Romania","Russian Federation","Rwanda","Saint Helena","Saint Kitts and Nevis","Saint Lucia","Saint Pierre and Miquelon","Saint Vincent and Grenadines","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Sint Maarten","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Korea","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Sweden","Switzerland","Syria","Tajikistan","Tanzania","Thailand","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Turks and Caicos Islands","Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Wallis and Futuna Islands","Western Sahara","Western Samoa","Yemen","Zambia","Zimbabwe")
DiseasePrevalenceURL <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

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
    titlePanel(AppTitle),
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
            radioButtons(inputId = "DeparturePrevalenceChoice", label = "Disease point prevalence at origin", choices = list("Manual (enter your own)", "Automatic (based on state data)")),
            conditionalPanel(
                condition = "input.DeparturePrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "DeparturePrevalence", label = "Select a disease point prevalence at origin", min=0, max=1, value=.04)
            ),
            conditionalPanel(
                condition = "input.DeparturePrevalenceChoice == 'Automatic (based on state data)'",
                uiOutput("DepartureStateSelector")
            ),
            
            # Destination characteristics
            hr(),
            h3("Destination"),
            radioButtons(inputId = "ArrivalPrevalenceChoice", label = "Disease point prevalence at destination", choices = list("Manual (enter your own)", "Automatic (based on state data)")),
            conditionalPanel(
                condition = "input.ArrivalPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "ArrivalPrevalence", label = "Select a disease point prevalence at destination", min=0, max=1, value=.12)
            ),
            conditionalPanel(
                condition = "input.ArrivalPrevalenceChoice == 'Automatic (based on state data)'",
                uiOutput("ArrivalStateSelector")
            ),
            
            # Population characteristics
            hr(),
            h3("Population"),
            radioButtons(inputId = "PopulationCountChoice", label = "Air traveler count", choices = list("Manual (enter your own)", "Automatic (based on 2019 O&D traffic for that pair)")),
            conditionalPanel(
                condition = "input.PopulationCountChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "PopulationCount", label = "Select the air traveler count", min=0, max=4.5*10^9, value=2.2*10^9)
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
            radioButtons(inputId = "DepartureTestSampleChoice", label = "Proportion of departing travelers being tested", choices = list("Systematic testing (all air travelers)", "Sample testing (enter a percentage)")),
            conditionalPanel(
                condition = "input.DepartureTestSampleChoice == 'Sample testing (enter a percentage)'",
                sliderInput(inputId = "DepartureTestSampleSize", label = "Select a proportion of departing travelers to test", min=0, max=100, value=100)
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
                sliderInput(inputId = "DaysAfterArrival", label = "Days after arrival (0 for day of travel)", min=0, max=7, step=1, value=1)
            ),
            radioButtons(inputId = "ArrivalTestTestSampleChoice", label = "Proportion of arriving travelers being tested", choices = list("Systematic testing (all air travelers)", "Sample testing (enter a percentage)")),
            conditionalPanel(
                condition = "input.ArrivalTestTestSampleChoice == 'Sample testing (enter a percentage)'",
                sliderInput(inputId = "ArrivalTestTestSampleSize", label = "Select a proportion of arriving travelers to test", min=0, max=100, value=100)
            ),
            
        ),
        
        #######################################################################
        # MAIN PANEL FOR OUTPUTS                                              #
        #######################################################################
        
        mainPanel(
            
            # Title
            hr(),
            
            ###################################################################
            # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                              #
            ###################################################################
            
            h3("1. Pre-departure, pre-test outcomes"),
            tabsetPanel(
                
                # Overview panel
                tabPanel("Overview",
                    br(),
                    p(align = "center", "This model step uses the assumed point prevalence of COVID-19 in the state of origin to calculate the likelihood that any given individual who reports for outbound travel is infected. Click on the Tables tab to see the results."),
                ),

                # Inputs panel
                tabPanel("Inputs",
                    br(),
                    fluidRow(
                        column(6, dataTableOutput("DeparturePrevalence")),
                        column(6, )
                    )
                ),

                # Outputs panel
                tabPanel("Outputs",
                    br(),
                
                    # Table 1.1
                    p(align = "center", "1.1. Probability that a departing air traveler is infected, based on the disease prevalence at origin"),
                    fluidRow(
                        column(6, withSpinner(dataTableOutput("PreDepartureDiseasePrevalencePercentageTable"))),
                        column(6, withSpinner(dataTableOutput("PreDepartureDiseasePrevalenceHeadcountTable")))
                    )
                ),
                
                # Chart panel
                tabPanel("Chart",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                # Data panel
                tabPanel("Data", withSpinner(dataTableOutput("OriginDiseasePrevalenceTable"))),
                
                # Explanation panel
                tabPanel("Explanation",
                    br(),
                    conditionalPanel(
                        condition = "input.DeparturePrevalenceChoice == 'Manual (enter your own)'",
                        p(align = "center", "The point prevalence at origin is calculated using the latest 14-day new cases of COVID-19 per 100K people in the state of origin, as reported daily by the E.U. CDC. Then, the incidence is multiplied by the mean infectious period of COVID-19 of 12 days, as estimated by the U.S. CDC. Next, a coefficient of 40% of all cases is applied to account for non-symptomatic cases, as estimated by the U.S. CDC. Lastly, the result is divided by 100 to express the point prevalence as a percentage. This calculation method conforms to the ICAO CAPSCA guidance.")
                    ),
                )
                
            ),
            hr(),
            
            ###################################################################
            # 2. PRE-DEPARTURE POST-TEST OUTCOMES                             #
            ###################################################################

            h3("2. Pre-departure, post-test outcomes"),
            tabsetPanel(
                
                # Overview panel
                tabPanel("Overview",
                    br(),
                    p(align = "center", "This model step uses the point prevalence at origin and the pre-departure test design characteristics to calculate the likelihood of test results independently of everything else (table 2.1), of test results given an infection status (table 2.2), and of infection status given a test result (table 2.3). Click on the Tables tab to see the results."),
                ),
                
                # Table panel
                tabPanel("Tables",
                    br(),
                    
                    # Table 2.1
                    p(align = "center", "2.1. Probability that a departing air traveler tests positive/negative"),
                    fluidRow(
                        column(6, withSpinner(dataTableOutput("PreDepartureTestPercentageTable"))),
                        column(6, withSpinner(dataTableOutput("PreDepartureTestHeadcountTable")))
                    ),
                    br(),
                    
                    # Table 2.2
                    p(align = "center", "2.2. Probability that a departing air traveler tests positive/negative, given that they are infected/uninfected."),
                    fluidRow(
                        column(6, withSpinner(dataTableOutput("PreDepartureTestPriorPercentageTable"))),
                        column(6, withSpinner(dataTableOutput("PreDepartureTestPriorHeadcountTable")))
                    ),
                    br(),
                    
                    # Table 2.3
                    p(align = "center", "2.3. Probability that a departing air traveler is infected/uninfected, given that they test positive/negative."),
                    fluidRow(
                        column(6, withSpinner(dataTableOutput("PreDepartureTestPosteriorPercentageTable"))),
                        column(6, withSpinner(dataTableOutput("PreDepartureTestPosteriorHeadcountTable")))
                    ),
                ),
                
                # Chart panel
                tabPanel("Chart",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                # Data panel
                tabPanel("Data",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                # Explanation panel
                tabPanel("Explanation",
                     br(),
                     p(align = "center", "Lorem ipsum dolor sit amet.")
                )
            ),
            hr(),

            ###################################################################
            # 3. POST-ARRIVAL PRE-TEST OUTCOMES                               #
            ###################################################################

            h3("3. Post-arrival, pre-test outcomes"),
            tabsetPanel(
                
                # Overview panel
                tabPanel("Overview",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Table panel
                tabPanel("Tables",
                    br(),
                    
                    # Table 3.1
                    p(align = "center", "3.1. Disease prevalence among air travelers upon arrival (percentage)"),
                    fluidRow(
                        column(6, withSpinner(dataTableOutput("PostArrivalDiseasePrevalencePercentageTable"))),
                        column(6, withSpinner(dataTableOutput("PostArrivalDiseasePrevalenceHeadcountTable")))
                    ),
                ),
                
                # Chart panel
                tabPanel("Chart",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Data panel
                tabPanel("Data",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Explanation panel
                tabPanel("Explanation",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                )
                
            ),
            hr(),

            ###################################################################
            # 4. POST-ARRIVAL POST-TEST OUTCOMES                              #
            ###################################################################

            h3("4. Post-arrival, post-test outcomes"),
            tabsetPanel(
            
                # Overview panel
                tabPanel("Overview",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Table panel
                tabPanel("Tables",
                    br(),
                
                    # Table 3.1
                    p(align = "center", "4.1. )"),
                    fluidRow(
                        column(6, withSpinner(dataTableOutput("PostArrivalPostTestPercentageTable"))),
                        column(6, withSpinner(dataTableOutput("PostArrivalPostTestHeadcountTable")))
                    ),
                ),
                
                # Chart panel
                tabPanel("Chart",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Data panel
                tabPanel("Data",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Explanation panel
                tabPanel("Explanation",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                )
                
            )
        )
    )
)

###############################################################################
# SERVER LOGIC                                                                #
###############################################################################

server <- function(input, output) {

    ###############################################################################
    # CALCULATE DISEASE PREVALENCE                                                #
    ###############################################################################
    
    # Import the European Centre for Disease Prevention and Control incidence data and calculate the prevalence by country
    OriginDiseasePrevalenceTable <- pin(DiseasePrevalenceURL) %>%
        read_csv(na = "", col_types = list(col_date(format = "%d/%m/%Y"), col_integer(), col_integer(), col_integer(), col_integer(), col_integer(), col_factor(), col_factor(), col_factor(), col_integer(), col_factor(), col_double())) %>%
        rename("Date" = "dateRep", "Day" = "day", "Month" = "month", "Year" = "year", "Cases" = "cases", "Deaths" = "deaths", "Country" = "countriesAndTerritories", "CountryTwoLetterCode" = "geoId", "CountryThreeLetterCode" = "countryterritoryCode", "Population" = "popData2019", "Continent" = "continentExp", "Incidence" = "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000") %>%
        filter(Continent != "Other") %>%
        mutate(Country = gsub("_", " ", Country)) %>%
        mutate(Prevalence = Incidence * 12 / (1 - .6) / 100) %>%
        group_by(Country) %>% filter(Date == max(Date))
    
    # Render selected columns from the prevalence table
    OriginDiseasePrevalenceColumns <- c("Date","Country", "Population", "Incidence", "Prevalence")
    output$OriginDiseasePrevalenceTable <- renderDataTable(
        datatable(
            OriginDiseasePrevalenceTable %>%
                select(OriginDiseasePrevalenceColumns),
            rownames = NULL,
            options = list(dom = "t", paging = FALSE)
        ) %>%
            formatCurrency(columns = c("Population"), currency = "", digits = 0) %>%
            formatCurrency(columns = c("Incidence", "Prevalence"), currency = "", digits = 2) %>%
            formatStyle(columns = OriginDiseasePrevalenceColumns,  color = "#1E32FA")
    )
    
    # Set origin prevalence to either that of the selected departure state or to that defined manually by the user
    DeparturePrevalence <- reactive({
        ifelse(input$DeparturePrevalenceChoice == "Automatic (based on state data)", OriginDiseasePrevalenceTable[ which(OriginDiseasePrevalenceTable$Country == input$DepartureState), 13, drop = TRUE], input$DeparturePrevalence)
    })
    
    # Render the origin prevalence assumption
    output$DeparturePrevalence <- renderDataTable(
        datatable(
            data.frame("Disease point prevalence at origin", DeparturePrevalence()),
            colnames = c("Assumption", "Value"), rownames = NULL, options = list(dom = "t", paging = FALSE)) %>%
            formatCurrency(columns = 2, currency = "", digits = 2) %>%
            formatStyle(columns = 2,  color = "#1E32FA")
    )
    
    ###########################################################################
    # DEFINE LIST OF STATES                                                   #
    ###########################################################################
    
    output$DepartureStateSelector <- renderUI({
        tagList(
            selectInput(inputId = "DepartureState", label = "Select a departure state", choices = OriginDiseasePrevalenceTable %>% select(Country))
        )
    })

    output$ArrivalStateSelector <- renderUI({
        tagList(
            selectInput(inputId = "ArrivalState", label = "Select an arrival state", choices = OriginDiseasePrevalenceTable %>% select(Country))
        )
    })
    
    ###########################################################################
    # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                                      #
    ###########################################################################

    # 1.1a Build a data frame to store the probability that a departing air traveler is infected (percentage)
    PreDepartureDiseasePrevalencePercentageTable <- reactive({
        PreDepartureDiseasePrevalencePercentageLabels = "Percentage"
        PreDepartureDiseasePrevalencePercentageInfectedValues = input$DeparturePrevalence
        PreDepartureDiseasePrevalencePercentageUninfectedValues = 1 - input$DeparturePrevalence
        PreDepartureDiseasePrevalencePercentageTotal = PreDepartureDiseasePrevalencePercentageInfectedValues + PreDepartureDiseasePrevalencePercentageUninfectedValues
        datatable(data.frame(Prevalence = PreDepartureDiseasePrevalencePercentageLabels, Infected = PreDepartureDiseasePrevalencePercentageInfectedValues, Uninfected = PreDepartureDiseasePrevalencePercentageUninfectedValues, Total = PreDepartureDiseasePrevalencePercentageTotal), rownames = NULL, options = list(dom = "t")) %>% formatPercentage(columns = c("Infected", "Uninfected","Total"), digits = 1) %>% formatStyle(columns = c("Infected", "Uninfected", "Total"),  color = "#1E32FA")
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
        datatable(data.frame(Prevalence = PreDepartureDiseasePrevalenceHeadcountLabels, Infected = PreDepartureDiseasePrevalenceHeadcountInfectedValues, Uninfected = PreDepartureDiseasePrevalenceHeadcountUninfectedValues, Total = PreDepartureDiseasePrevalenceHeadcountTotal), rownames = NULL, options = list(dom = "t")) %>% formatCurrency(columns = c("Infected", "Uninfected", "Total"), currency = "", digits = 0) %>% formatStyle(columns = c("Infected", "Uninfected", "Total"),  color = "#1E32FA")
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
        datatable(data.frame(Test = PreDepartureTestPercentageLabels, Positive = PreDepartureTestPercentagePositiveValues, Negative = PreDepartureTestPercentageNegativeValues, Total = PreDepartureTestPercentageTotal), rownames = NULL, options = list(dom = "t")) %>% formatPercentage(columns = c("Positive", "Negative","Total"), digits = 1) %>% formatStyle(columns = c("Positive", "Negative", "Total"),  color = "#1E32FA")
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
        datatable(data.frame(Test = PreDepartureTestHeadcountLabels, Positive = PreDepartureTestHeadcountPositiveValues, Negative = PreDepartureTestHeadcountNegativeValues, Total = PreDepartureTestHeadcountTotal), rownames = NULL, options = list(dom = "t")) %>% formatCurrency(columns = c("Positive", "Negative", "Total"), currency = "", digits = 0) %>% formatStyle(columns = c("Positive", "Negative", "Total"),  color = "#1E32FA")
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
        datatable(data.frame(Test = PreDepartureTestPriorPercentageTestLabels, Infected = PreDepartureTestPriorPercentageInfectedValues,  Uninfected = PreDepartureTestPriorPercentageUninfectedValues), rownames = NULL, options = list(dom = "t")) %>% formatPercentage(columns = c("Infected", "Uninfected"), digits = 1) %>% formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
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
        datatable(data.frame(Test = PreDepartureTestPriorHeadcountTestLabels, Infected = PreDepartureTestPriorHeadcountInfectedValues,  Uninfected = PreDepartureTestPriorHeadcountUninfectedValues), rownames = NULL, options = list(dom = "t")) %>% formatCurrency(columns = c("Infected", "Uninfected"), currency = "", digits = 0) %>% formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
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
        datatable(data.frame(Test = PreDepartureTestPosteriorPercentageLabels, Positive = PreDepartureTestPosteriorPercentagePositiveValues,  Negative = PreDepartureTestPosteriorPercentageNegativeValues), rownames = NULL, options = list(dom = "t")) %>% formatPercentage(columns = c("Positive", "Negative"), digits = 1) %>% formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
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
        datatable(data.frame(Test = PreDepartureTestPosteriorHeadcountLabels, Positive = PreDepartureTestPosteriorHeadcountPositiveValues,  Negative = PreDepartureTestPosteriorHeadcountNegativeValues), rownames = NULL, options = list(dom = "t")) %>% formatCurrency(columns = c("Positive", "Negative"), currency = "", digits = 0) %>% formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
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