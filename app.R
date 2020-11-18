###############################################################################
# AIR TRAVEL COVID-19 TESTING SIMULATOR                                       #
# Created by: Thomas D. Pellegrin                                             #
#             contact@theaviationdoctor.com                                   #
#             https://theaviationdoctor.com                                   #
#             November 2020                                                   #
###############################################################################
###############################################################################
# ROADMAP FOR FUTURE FEATURES                                                 #
# 1. Likelihood of disease detection based on viral load ramp-up              #
# 2. Transit passengers with mixed disease prevalence                         #
# 3. Post-arrival quarantines to catch pre-symptomatic false negatives        #
# 4. Two tests on departure (to reduce false positives)                       #
###############################################################################
###############################################################################
# HOUSEKEEPING                                                                #
###############################################################################
# Clear the console
#cat("\014")
# Load libraries
library(DT)                 # To better display data tables
library(formattable)        # To format numbers for rendering
library(pins)               # To locally cache downloaded data for performance
library(shiny)              # to build and display the app in a browser
library(shinycssloaders)    # To style the app and spinners in particular
library(tidyverse)          # To wrangle the data
library(zoo)                # To perform rolling means
###############################################################################
# VARIABLE DECLARATION                                                        #
###############################################################################
# Header labels
AppHeader               <- "Air travel COVID-19 testing simulator"
LabelInputPanel1        <- "Model inputs"
LabelOutputHeader1      <- "1. Pre-departure, pre-test outcomes"
LabelOutputHeader2      <- "2. Pre-departure, post-test outcomes"
LabelOutputHeader3      <- "3. Post-arrival, pre-test outcomes"
LabelOutputHeader4      <- "4. Post-arrival, post-test outcomes"
#Summary content
LabelOutputSummary1    <- "This module determines the likelihood that a departing traveler is infected before testing."
LabelOutputSummary2    <- "This module determines the effectiveness of pre-departure testing."
LabelOutputSummary3    <- "This module determines the effectiveness of post-arrival testing."
LabelOutputSummary4    <- "This module determines the likelihood that an arriving traveler is infected after testing."
# Input selectors
States                  <- list("Afghanistan", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antigua and Barbuda", "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia (Plurinational State of)", "Bonaire, Sint Eustatius and Saba", "Bosnia and Herzegovina", "Botswana", "Brazil", "British Virgin Islands", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Cook Islands", "Costa Rica", "Côte d’Ivoire", "Croatia", "Cuba", "Curaçao", "Cyprus", "Czechia", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Falkland Islands (Malvinas)", "Faroe Islands", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Holy See", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia (Federated States of)", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "North Macedonia", "Northern Mariana Islands (Commonwealth of the)", "Norway", "Occupied Palestinian territory, incl. east Jerusalem", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Puerto Rico", "Qatar", "Republic of Korea", "Republic of Moldova", "Réunion", "Romania", "Russian Federation", "Rwanda", "Saint Barthélemy", "Saint Helena", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syrian Arab Republic", "Tajikistan", "Thailand", "The United Kingdom", "Timor-Leste", "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Republic of Tanzania", "United States of America", "United States Virgin Islands", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela (Bolivarian Republic of)", "Viet Nam", "Wallis and Futuna", "Yemen", "Zambia", "Zimbabwe")
DefaultOriginState      <- "United States of America"
DefaultDestinationState <- "France"
# URLs
URLEpidemiology         <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
URLPopulation           <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19Testing/main/data/population.csv"
URLTraffic              <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19Testing/main/data/traffic2019.csv"
###############################################################################
# USER INTERFACE LOGIC                                                        #
###############################################################################
ui <- fluidPage(
    # Globally style the app
    tags$head(
        tags$style("* { font-family: 'Aktiv Grotesk', Arial, sans-serif; !important }"),
        tags$style("hr { border: 1px solid #000000 }")
    ),
    # App header
    titlePanel(AppHeader),
    hr(),
    sidebarLayout(
        #######################################################################
        # SIDEBAR PANEL FOR INPUTS                                            #
        #######################################################################
        sidebarPanel(
            #Title
            h3(LabelInputPanel1),
            # Origin characteristics
            hr(),
            radioButtons(inputId = "OriginPrevalenceChoice", label = "Disease prevalence at origin", choices = list("Manual (enter your own)", "Automatic (based on latest WHO data)")),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "OriginPrevalence", label = "", min = 0, max = 100, step = .1, value = 1.9)
            ),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest WHO data)'",
                selectInput(inputId = "OriginState", label = "", choices = States, selected = DefaultOriginState)
            ),
            # Destination characteristics
            hr(),
            radioButtons(inputId = "DestinationPrevalenceChoice", label = "Disease  prevalence at destination", choices = list("Manual (enter your own)", "Automatic (based on latest WHO data)")),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "DestinationPrevalence", label = "", min = 0, max = 100, step = .1, value = 1.2)
            ),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Automatic (based on latest WHO data)'",
                selectInput(inputId = "DestinationState", label = "", choices = States, selected = DefaultDestinationState)
            ),
            # Prevalence characteristics
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest WHO data)' | input.DestinationPrevalenceChoice == 'Automatic (based on latest WHO data)'",
                hr(),
                strong("Prevalence calculation options"),
                br(),
                br(),
                sliderInput(inputId = "IncidenceMovingAverage", label = "Moving average of new cases (days)", min = 1, max = 30, step = 1, value = 14),
                sliderInput(inputId = "InfectiousPeriod", label = "Infectious period (days)", min = 1, max = 30, step = 1, value = 12),
                sliderInput(inputId = "NonSymptomaticRate", label = "Non-symptomatic rate", min = 0, max = 100, step = 1, value = 40)
            ),
            # Population characteristics
            hr(),
            radioButtons(inputId = "PopulationCountChoice", label = "Passenger traffic from origin to destination", choices = list("Manual (enter your own)", "Automatic (based on 2019 O&D traffic for that pair)")),
            conditionalPanel(
                condition = "input.PopulationCountChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "PopulationCount", label = "", min=0, max=4.5*10^9, value=1*10^6)
            ),
            # Pre-departure test characteristics
            hr(),
            selectInput(inputId = "PreDepartureTestMethod", label = "Pre-departure test", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            conditionalPanel(
                condition = "input.PreDepartureTestMethod != 'None'",
                sliderInput(inputId = "PreDepartureTestSampleSize", label = "Percentage of departing travelers to be tested", min=0, max=100, value=100),
                sliderInput(inputId = "HoursBeforeDeparture", label = "Hours before boarding", min=-96, max=0, step=1, value=-24),
            ),
            conditionalPanel(
                condition = "input.PreDepartureTestMethod == 'Manual (design your own)'",
                sliderInput(inputId = "PreDepartureTestLimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^4, value=1000),
                sliderInput(inputId = "PreDepartureTestSensitivity", label = "Clinical sensitivity", min=0, max=100, value=70),
                sliderInput(inputId = "PreDepartureTestSpecificity", label = "Clinical specificity", min=0, max=100, value=95)
            ),
            # Post-arrival test characteristics
            hr(),
            selectInput(inputId = "PostArrivalTestMethod", label = "Post-arrival test", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            conditionalPanel(
                condition = "input.PostArrivalTestMethod != 'None'",
                sliderInput(inputId = "PostArrivalTestSampleSize", label = "Percentage of arriving travelers to be tested", min=0, max=100, value=100),
                sliderInput(inputId = "HoursAfterArrival", label = "Hours after unboarding", min=0, max=96, step=1, value=24),
            ),
            conditionalPanel(
                condition = "input.PostArrivalTestMethod == 'Manual (design your own)'",
                sliderInput(inputId = "PostArrivalTestLimitOfDetection", label = "Limit of detection (copies/ml)", min=0, max=10^4, value=1000),
                sliderInput(inputId = "PostArrivalTestSensitivity", label = "Clinical sensitivity", min=0, max=100, value=70),
                sliderInput(inputId = "PostArrivalTestSpecificity", label = "Clinical specificity", min=0, max=100, value=95)
            ),
        ),
        #######################################################################
        # MAIN PANEL FOR OUTPUTS                                              #
        #######################################################################
        mainPanel(
            ###################################################################
            # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                              #
            ###################################################################
            h3(LabelOutputHeader1),
            em(LabelOutputSummary1),
            br(),
            br(),
            tabsetPanel(
                ###############################################################
                # 1.1 SUMMARY PANEL                                          #
                ###############################################################
                tabPanel("Summary",
                    br(),
                    uiOutput("PreDepartureOriginSummary")
                ),
                ###############################################################
                # 1.2 DETAILS PANEL                                           #
                ###############################################################
                tabPanel("Details",
                    br(),
                    div(align = "center", em("Table 1.1. Disease prevalence at origin and destination (left), and passenger traffic (right)")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureAssumptionsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureAssumptionsIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 1.2. Likelihood that a departing air traveler is infected, based on the disease prevalence at origin")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 1.3. Latest disease point prevalence and moving average, by country")),
                    fluidRow(
                        column(12, withSpinner(DT::dataTableOutput("PreDeparturePrevalencePercentageTable")))
                    )
                ),
                ###############################################################
                # 1.3 CHARTS PANEL                                            #
                ###############################################################
                tabPanel("Charts",
                    br(),
                    div(align = "center", em("Moving average of the disease prevalence at origin (red) and destination (blue)")),
                    withSpinner(plotOutput("HistoricalPrevalenceChart"))
                )
            ),
            hr(),
            ###################################################################
            # 2. PRE-DEPARTURE POST-TEST OUTCOMES                             #
            ###################################################################
            h3(LabelOutputHeader2),
            em(LabelOutputSummary2),
            br(),
            br(),
            tabsetPanel(
                ###############################################################
                # 2.1 SUMMARY PANEL                                          #
                ###############################################################
                tabPanel("Summary",
                         br(),
                         uiOutput("PreDepartureTestResultsSummary")
                ),
                ###############################################################
                # 2.2 DETAILS PANEL                                           #
                ###############################################################
                tabPanel("Details",
                    br(),
                    div(align = "center", em("Table 2.1. Departing population and test assumptions")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureStartingAssumptionsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureStartingAssumptionsIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 2.2. Likelihood that a departing traveler is infected/uninfected before testing")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDeparturePopulationPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDeparturePopulationIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 2.3. Likelihood that a departing traveler gets tested")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureSamplePercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureSampleIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 2.4. Likelihood that a departing air traveler tests positive/negative")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 2.5. Likelihood that a departing air traveler tests positive/negative, given that they are infected/uninfected")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsGivenInfectionPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsGivenInfectionIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 2.6. Likelihood that a departing air traveler is infected/uninfected, given that they tested positive/negative")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionGivenTestResultsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionGivenTestResultsIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 2.7. Likelihood that a tested departing air traveler is stopped or allowed to depart")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestClearedPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestClearedIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 2.8. Likelihood that a departing traveler is infected/uninfected after testing")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResidualPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResidualIntegerTable")))
                    ),
                ),
                ###############################################################
                # 2.3 CHARTS PANEL                                            #
                ###############################################################
                tabPanel("Charts",
                    br(),
                    em(textOutput("PreDepartureTestChartTitle")),
                )
            ),
            hr(),
            ###################################################################
            # 3. POST-ARRIVAL PRE-TEST OUTCOMES                               #
            ###################################################################
            h3(LabelOutputHeader3),
            em(LabelOutputSummary3),
            br(),
            br(),
            tabsetPanel(
                ###############################################################
                # 3.1 SUMMARY PANEL                                           #
                ###############################################################
                tabPanel("Summary",
                    br(),
                    uiOutput("PostArrivalTestResultsSummary")
                ),
                ###############################################################
                # 3.2 DETAILS PANEL                                           #
                ###############################################################
                tabPanel("Details",
                    br(),
                    div(align = "center", em("Table 3.1. Arriving population and test assumptions")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalStartingAssumptionsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalStartingAssumptionsIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.2. Likelihood that an arriving traveler is infected/uninfected before testing")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPopulationPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPopulationIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.3. Likelihood that an arriving traveler gets tested")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalSamplePercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalSampleIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.4. Likelihood that an arriving air traveler tests positive/negative")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResultsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResultsIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.5. Likelihood that an arriving air traveler tests positive/negative, given that they are infected/uninfected")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResultsGivenInfectionPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResultsGivenInfectionIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.6. Likelihood that an arriving air traveler is infected/uninfected, given that they tested positive/negative")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalInfectionGivenTestResultsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalInfectionGivenTestResultsIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.7. Likelihood that a tested arriving air traveler is stopped or allowed to depart")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestClearedPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestClearedIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.8. Likelihood that an arriving traveler is infected/uninfected after testing")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResidualPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResidualIntegerTable")))
                    ),
                ),
                ###############################################################
                # 3.3 CHARTS PANEL                                            #
                ###############################################################
                tabPanel("Charts",
                         br(),
                         em(textOutput("PostArrivalTestChartTitle")),
                )
            ),
            hr(),
            ###################################################################
            # 4. POST-ARRIVAL POST-TEST OUTCOMES                              #
            ###################################################################
            h3(LabelOutputHeader4),
            em(LabelOutputSummary4),
            br(),
            br(),
            tabsetPanel(
                ###############################################################
                # 4.1 SUMMARY PANEL                                           #
                ###############################################################
                tabPanel("Summary",
                    br(),
                    uiOutput("PostArrivalDestinationSummary")
                ),
                ###############################################################
                # 4.2 DETAILS PANEL                                           #
                ###############################################################
                tabPanel("Details",
                         br(),
                         p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                ###############################################################
                # 4.3 CHARTS PANEL                                            #
                ###############################################################
                tabPanel("Charts",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                )
            )
        )
    )
)
###############################################################################
# SERVER LOGIC                                                                #
###############################################################################
server <- function(input, output) {
    ###########################################################################
    # IMPORT AND WRANGLE DATA                                                 #
    ###########################################################################
    # Import and wrangle the population data
    PopulationTable <- pin(URLPopulation) %>%
        read_csv(na = "", col_types = list(col_factor(), col_integer()))
    # Import and wrangle the 2019 origin-destination passenger traffic
    TrafficTable <- pin(URLTraffic) %>%
        read_csv(na = "", col_types = list(col_factor(), col_factor(), col_integer()))
    # Import and wrangle the WHO epidemiological data, then calculate the disease prevalence by country per 100 people using the CAPSCA formula
    IncidenceTable <- reactive({
        pin(URLEpidemiology) %>%                                                   # Read from the cached file
            read_csv(na = "", col_types = list(col_date(format = "%Y-%m-%d"), col_factor(), col_factor(), col_factor(), col_integer(), col_integer(), col_integer(), col_integer())) %>% # Declare variable types from the data set
            rename("Date" = "Date_reported", "Incidence" = "New_cases") %>%     # Rename the columns
            remove_missing(vars = "Incidence") %>%                              # Remove any blanks (there should not be any, but this is to be safe)
            group_by(Country) %>%                                               # Group by country so we can select the latest cases by country
            arrange(desc(Date)) %>%                                             # Sort by descending date order (should be the default in the data set, but this is to be safe)
            slice(1:(input$IncidenceMovingAverage * 2)) %>%                     # Keep only the latest n rows for each country, based on how long the moving average is set to in the inputs panel (we need twice the moving average length to calculate it)
            inner_join(PopulationTable, by = "Country") %>%                     # Add a population column from the population table to calculate the prevalence
            mutate(PointPrevalence = Incidence / Population * input$InfectiousPeriod / (1 - input$NonSymptomaticRate / 100)) %>% # Apply the CAPSCPA formula to account for infectious period and non-symptomatic / unreported cases
            select(Date, Country, Population, Incidence, PointPrevalence) %>%   # Select only the columns of interest
            group_by(Country) %>%                                               # Needed to calculate the moving average
            arrange(Country, Date) %>%                                          # Needed to calculate the moving average
            mutate(MovingAveragePrevalence = rollmean(x = PointPrevalence, input$IncidenceMovingAverage, align = "right", fill = NA)) %>% # Calculate the moving average of the average
            arrange(Country, desc(Date))%>%                                     # Final sort by latest date first
            remove_missing(vars = "RollingPrevalence")                          # Remove rows beyond the range of the moving average calculation window
    })
    ###########################################################################
    # DECLARE VARIABLES                                                       #
    ###########################################################################
    # Starting assumptions
    OriginPrevalence <- reactive({ ifelse(input$OriginPrevalenceChoice == "Automatic (based on latest WHO data)", IncidenceTable()[which(IncidenceTable()$Country == input$OriginState), 6, drop = TRUE], input$OriginPrevalence / 100) })
    DestinationPrevalence <- reactive({ ifelse(input$DestinationPrevalenceChoice == "Automatic (based on latest WHO data)", IncidenceTable()[which(IncidenceTable()$Country == input$DestinationState), 6, drop = TRUE], input$DestinationPrevalence / 100) })
    # PreDeparture table 2.1
    PreDepartureTestPopulationCount <- reactive({ ifelse(input$PopulationCountChoice == "Automatic (based on 2019 O&D traffic for that pair)", max(TrafficTable[ which(TrafficTable$Origin == input$OriginState & TrafficTable$Destination == input$DestinationState), 3, drop = TRUE],0), input$PopulationCount) })
    PreDepartureTestLimitOfDetection <- reactive({ ifelse(input$PreDepartureTestMethod == "None", NA, input$PreDepartureTestLimitOfDetection) })
    PreDepartureTestSensitivity <- reactive({ ifelse(input$PreDepartureTestMethod == "None", NA, input$PreDepartureTestSensitivity / 100) })
    PreDepartureTestSpecificity <- reactive({ ifelse(input$PreDepartureTestMethod == "None", NA, input$PreDepartureTestSpecificity / 100) })
    # PreDeparture table 2.2
    PreDepartureTestPopulationInfectedPercentage <- reactive({ OriginPrevalence() })
    PreDepartureTestPopulationUninfectedPercentage <- reactive({ 1 - OriginPrevalence() })
    PreDepartureTestPopulationInfectedCount <- reactive({ PreDepartureTestPopulationCount() * PreDepartureTestPopulationInfectedPercentage() })
    PreDepartureTestPopulationUninfectedCount <- reactive({ PreDepartureTestPopulationCount() * (1 - PreDepartureTestPopulationInfectedPercentage()) })
    # PreDeparture table 2.3
    PreDepartureSampleTestedPercentage <- reactive({ ifelse(input$PreDepartureTestMethod == "None", 0, input$PreDepartureTestSampleSize / 100) })
    PreDepartureSampleUntestedPercentage <- reactive({ 1 - PreDepartureSampleTestedPercentage() })
    PreDepartureSampleTestedCount <- reactive({ PreDepartureTestPopulationCount() * PreDepartureSampleTestedPercentage() })
    PreDepartureSampleUntestedCount <- reactive({ PreDepartureTestPopulationCount() - PreDepartureSampleTestedCount() })
    # PreDeparture table 2.4
    PreDepartureTestPositivePercentage <- reactive({ (PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() + (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage())) })
    PreDepartureTestNegativePercentage <- reactive({ ((1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() + PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage())) })
    PreDepartureTestPositiveCount <- reactive({ PreDepartureSampleTestedCount() * PreDepartureTestPositivePercentage() })
    PreDepartureTestNegativeCount <- reactive({ PreDepartureSampleTestedCount() * PreDepartureTestNegativePercentage() })
    # PreDeparture table 2.5
    PreDepartureTestPositiveGivenInfectedPercentage <- reactive({ PreDepartureTestSensitivity() })
    PreDepartureTestPositiveGivenUninfectedPercentage <- reactive({ 1 - PreDepartureTestSpecificity() })
    PreDepartureTestNegativeGivenInfectedPercentage <- reactive({ 1 - PreDepartureTestSensitivity() })
    PreDepartureTestNegativeGivenUninfectedPercentage <- reactive({ PreDepartureTestSpecificity() })
    PreDepartureTestPositiveGivenInfectedCount <- reactive({ PreDepartureTestPopulationInfectedCount() * PreDepartureTestPositiveGivenInfectedPercentage() * PreDepartureSampleTestedPercentage() })
    PreDepartureTestPositiveGivenUninfectedCount <- reactive({ PreDepartureTestPopulationUninfectedCount() * PreDepartureTestPositiveGivenUninfectedPercentage() * PreDepartureSampleTestedPercentage() })
    PreDepartureTestNegativeGivenInfectedCount <- reactive({ PreDepartureTestPopulationInfectedCount() * PreDepartureTestNegativeGivenInfectedPercentage() * PreDepartureSampleTestedPercentage() })
    PreDepartureTestNegativeGivenUninfectedCount <- reactive({ PreDepartureTestPopulationUninfectedCount() * PreDepartureTestNegativeGivenUninfectedPercentage() * PreDepartureSampleTestedPercentage() })
    # PreDeparture table 2.6
    PreDepartureTestInfectedGivenPositivePercentage <- reactive({ PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() / (PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() + (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage())) })
    PreDepartureTestInfectedGivenNegativePercentage <- reactive({ (1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() / ((1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() + PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage())) })
    PreDepartureTestUninfectedGivenPositivePercentage <- reactive({ (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage()) / (PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() + (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage())) })
    PreDepartureTestUninfectedGivenNegativePercentage <- reactive({ PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage()) / ((1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() + PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage())) })
    PreDepartureTestInfectedGivenPositiveCount <- reactive({ PreDepartureTestInfectedGivenPositivePercentage() * PreDepartureTestPositiveCount() })
    PreDepartureTestInfectedGivenNegativeCount <- reactive({ PreDepartureTestInfectedGivenNegativePercentage() * PreDepartureTestNegativeCount() })
    PreDepartureTestUninfectedGivenPositiveCount <- reactive({ PreDepartureTestUninfectedGivenPositivePercentage() * PreDepartureTestPositiveCount() })
    PreDepartureTestUninfectedGivenNegativeCount <- reactive({ PreDepartureTestUninfectedGivenNegativePercentage() * PreDepartureTestNegativeCount() })
    # PreDeparture table 2.7
    PreDepartureTestClearedPercentage <- reactive({ PreDepartureTestClearedCount() / PreDepartureTestPopulationCount() })
    PreDepartureTestStoppedPercentage <- reactive({ 1 - PreDepartureTestClearedPercentage() })
    PreDepartureTestClearedCount <- reactive({ ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationCount(), PreDepartureSampleTestedCount() - PreDepartureTestPositiveGivenInfectedCount()) })
    PreDepartureTestStoppedCount <- reactive({ ifelse(input$PreDepartureTestMethod == "None", 0, PreDepartureTestPositiveGivenInfectedCount()) })
    # PreDeparture table 2.8
    PreDepartureTestResidualInfectedPercentage <- reactive({ ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationInfectedPercentage(), (PreDepartureTestPopulationInfectedCount() - PreDepartureTestPositiveGivenInfectedCount()) / PreDepartureTestPopulationCount()) })
    PreDepartureTestResidualUninfectedPercentage <- reactive({ 1 - PreDepartureTestResidualInfectedPercentage() })
    PreDepartureTestResidualInfectedCount <- reactive({ ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationInfectedCount(), PreDepartureTestPopulationInfectedCount() - PreDepartureTestPositiveGivenInfectedCount()) })
    PreDepartureTestResidualUninfectedCount <- reactive({ PreDepartureTestPopulationUninfectedCount() })
    # PostArrival table 2.1
    PostArrivalTestPopulationCount <- reactive({ ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationCount(), PreDepartureTestPopulationCount() - PreDepartureTestPositiveGivenInfectedCount()) })
    PostArrivalTestLimitOfDetection <- reactive({ ifelse(input$PostArrivalTestMethod == "None", NA, input$PostArrivalTestLimitOfDetection) })
    PostArrivalTestSensitivity <- reactive({ ifelse(input$PostArrivalTestMethod == "None", NA, input$PostArrivalTestSensitivity / 100) })
    PostArrivalTestSpecificity <- reactive({ ifelse(input$PostArrivalTestMethod == "None", NA, input$PostArrivalTestSpecificity / 100) })
    # PostArrival table 2.2
    PostArrivalTestPopulationInfectedPercentage <- reactive({ ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationInfectedPercentage(), PreDepartureTestNegativeGivenInfectedCount() / PostArrivalTestPopulationCount()) })
    PostArrivalTestPopulationUninfectedPercentage <- reactive({ 1 - PostArrivalTestPopulationInfectedPercentage() })
    PostArrivalTestPopulationInfectedCount <- reactive({ PostArrivalTestPopulationCount() * PostArrivalTestPopulationInfectedPercentage() })
    PostArrivalTestPopulationUninfectedCount <- reactive({ PostArrivalTestPopulationCount() * (1 - PostArrivalTestPopulationInfectedPercentage()) })
    # PostArrival table 2.3
    PostArrivalSampleTestedPercentage <- reactive({ ifelse(input$PostArrivalTestMethod == "None", 0, input$PostArrivalTestSampleSize / 100) })
    PostArrivalSampleUntestedPercentage <- reactive({ 1 - PostArrivalSampleTestedPercentage() })
    PostArrivalSampleTestedCount <- reactive({ PostArrivalTestPopulationCount() * PostArrivalSampleTestedPercentage() })
    PostArrivalSampleUntestedCount <- reactive({ PostArrivalTestPopulationCount() - PostArrivalSampleTestedCount() })
    # PostArrival table 2.4
    PostArrivalTestPositivePercentage <- reactive({ (PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() + (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage())) })
    PostArrivalTestNegativePercentage <- reactive({ ((1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() + PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage())) })
    PostArrivalTestPositiveCount <- reactive({ PostArrivalSampleTestedCount() * PostArrivalTestPositivePercentage() })
    PostArrivalTestNegativeCount <- reactive({ PostArrivalSampleTestedCount() * PostArrivalTestNegativePercentage() })
    # PostArrival table 2.5
    PostArrivalTestPositiveGivenInfectedPercentage <- reactive({ PostArrivalTestSensitivity() })
    PostArrivalTestPositiveGivenUninfectedPercentage <- reactive({ 1 - PostArrivalTestSpecificity() })
    PostArrivalTestNegativeGivenInfectedPercentage <- reactive({ 1 - PostArrivalTestSensitivity() })
    PostArrivalTestNegativeGivenUninfectedPercentage <- reactive({ PostArrivalTestSpecificity() })
    PostArrivalTestPositiveGivenInfectedCount <- reactive({ PostArrivalTestPopulationInfectedCount() * PostArrivalTestPositiveGivenInfectedPercentage() * PostArrivalSampleTestedPercentage() })
    PostArrivalTestPositiveGivenUninfectedCount <- reactive({ PostArrivalTestPopulationUninfectedCount() * PostArrivalTestPositiveGivenUninfectedPercentage() * PostArrivalSampleTestedPercentage() })
    PostArrivalTestNegativeGivenInfectedCount <- reactive({ PostArrivalTestPopulationInfectedCount() * PostArrivalTestNegativeGivenInfectedPercentage() * PostArrivalSampleTestedPercentage() })
    PostArrivalTestNegativeGivenUninfectedCount <- reactive({ PostArrivalTestPopulationUninfectedCount() * PostArrivalTestNegativeGivenUninfectedPercentage() * PostArrivalSampleTestedPercentage() })
    # PostArrival table 2.6
    PostArrivalTestInfectedGivenPositivePercentage <- reactive({ PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() / (PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() + (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage())) })
    PostArrivalTestInfectedGivenNegativePercentage <- reactive({ (1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() / ((1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() + PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage())) })
    PostArrivalTestUninfectedGivenPositivePercentage <- reactive({ (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage()) / (PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() + (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage())) })
    PostArrivalTestUninfectedGivenNegativePercentage <- reactive({ PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage()) / ((1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() + PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage())) })
    PostArrivalTestInfectedGivenPositiveCount <- reactive({ PostArrivalTestInfectedGivenPositivePercentage() * PostArrivalTestPositiveCount() })
    PostArrivalTestInfectedGivenNegativeCount <- reactive({ PostArrivalTestInfectedGivenNegativePercentage() * PostArrivalTestNegativeCount() })
    PostArrivalTestUninfectedGivenPositiveCount <- reactive({ PostArrivalTestUninfectedGivenPositivePercentage() * PostArrivalTestPositiveCount() })
    PostArrivalTestUninfectedGivenNegativeCount <- reactive({ PostArrivalTestUninfectedGivenNegativePercentage() * PostArrivalTestNegativeCount() })
    # PostArrival table 2.7
    PostArrivalTestClearedPercentage <- reactive({ PostArrivalTestClearedCount() / PostArrivalTestPopulationCount() })
    PostArrivalTestStoppedPercentage <- reactive({ 1 - PostArrivalTestClearedPercentage() })
    PostArrivalTestClearedCount <- reactive({ ifelse(input$PostArrivalTestMethod == "None", PostArrivalTestPopulationCount(), PostArrivalSampleTestedCount() - PostArrivalTestPositiveGivenInfectedCount()) })
    PostArrivalTestStoppedCount <- reactive({ ifelse(input$PostArrivalTestMethod == "None", 0, PostArrivalTestPositiveGivenInfectedCount()) })
    # PostArrival table 2.8
    PostArrivalTestResidualInfectedPercentage <- reactive({ ifelse(input$PostArrivalTestMethod == "None", PostArrivalTestPopulationInfectedPercentage(), (PostArrivalTestPopulationInfectedCount() - PostArrivalTestPositiveGivenInfectedCount()) / PostArrivalTestPopulationCount()) })
    PostArrivalTestResidualUninfectedPercentage <- reactive({ 1 - PostArrivalTestResidualInfectedPercentage() })
    PostArrivalTestResidualInfectedCount <- reactive({ ifelse(input$PostArrivalTestMethod == "None", PostArrivalTestPopulationInfectedCount(), PostArrivalTestPopulationInfectedCount() - PostArrivalTestPositiveGivenInfectedCount()) })
    PostArrivalTestResidualUninfectedCount <- reactive({ PostArrivalTestPopulationUninfectedCount() })
    ###########################################################################
    # DECLARE FUNCTIONS                                                       #
    ###########################################################################
    # These custom functions standardize table rendering with the DT::renderDataTable function, taking a reactive data frame as input
    MyRenderDataTableInteger <- function(DataFrame) {
        DT::renderDataTable({
            datatable(DataFrame(), rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE)) %>%
                formatRound(columns = 2:length(DataFrame()), digits = 0) %>%
                formatStyle(columns = 2:length(DataFrame()), color = "#1E32FA")
        })
    }
    MyRenderDataTablePercentage <- function(DataFrame) {
        DT::renderDataTable({
            datatable(DataFrame(), rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE)) %>%
                formatPercentage(columns = 2:length(DataFrame()), digits = 1) %>%
                formatStyle(columns = 2:length(DataFrame()), color = "#1E32FA")
        })
    }
    ###########################################################################
    # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                                      #
    ###########################################################################
        #######################################################################
        # 1.1. SUMMARY PANEL                                                  #
        #######################################################################
        output$PreDepartureOriginSummary <- renderUI({
            HTML(
                paste(
                    "<ul>",
                    "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) departing travelers were presumed infected.</li>",
                    "<ul>",
                        "<li>This is <span style=color:#1E32FA>", ifelse(PreDepartureTestPopulationInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the disease prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
                        "<li>It means that there is a <span style=color:#1E32FA>", ifelse(PreDepartureTestPopulationInfectedPercentage() > DestinationPrevalence(), "positive", "negative"), "</span> relative risk of importation of cases prior to any testing.</li>",
                    "</ul>",
                    "</ul>",
                    sep = ""
                )
            )
        })
        #######################################################################
        # 1.2. DETAILS PANEL                                                  #
        #######################################################################
        # Table 1.1. Disease prevalence at origin and destination (left), and passenger traffic (right)
        output$PreDepartureAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Prevalence at origin", "Prevalence at destination"), "Value" = c(OriginPrevalence(), DestinationPrevalence()))))
        output$PreDepartureAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = "Passenger headcount", "Value" = PreDepartureTestPopulationCount())))
        # Table 1.2. Likelihood that a departing air traveler is infected, based on the disease prevalence at origin
        output$PreDepartureInfectionPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Likelihood" = "Percent", "Infected" = PreDepartureTestPopulationInfectedPercentage(), "Uninfected" = PreDepartureTestPopulationUninfectedPercentage())))
        output$PreDepartureInfectionIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Likelihood" = "Count", "Infected" = PreDepartureTestPopulationInfectedCount(), "Uninfected" = PreDepartureTestPopulationUninfectedCount())))
        # Table 1.3. Latest disease point prevalence and moving average, by country
        output$PreDeparturePrevalencePercentageTable <- MyRenderDataTablePercentage(reactive(IncidenceTable() %>% filter(Date == max(Date)) %>% select(Country, PointPrevalence, MovingAveragePrevalence) %>% rename("Latest" = "PointPrevalence", "Moving average" = "MovingAveragePrevalence")))
        output$PreDeparturePrevalenceIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Likelihood" = "Count", "Infected" = PreDepartureTestPopulationInfectedCount(), "Uninfected" = PreDepartureTestPopulationUninfectedCount())))
        #######################################################################
        # 1.3. CHARTS PANEL                                                   #
        #######################################################################
        # Render a line plot to display the historical disease prevalence at origin and destination
        output$HistoricalPrevalenceChart <- renderPlot({
            ggplot(
                data = IncidenceTable() %>%
                    filter(Country == input$OriginState | Country == input$DestinationState) %>%
                    select(Date, Country, MovingAveragePrevalence) %>%
                    pivot_wider(names_from = Country, values_from = MovingAveragePrevalence) %>%
                    rename(DestinationAutomatic = input$DestinationState, OriginAutomatic = input$OriginState) %>%
                    mutate(OriginManual = rep(OriginPrevalence(),length(IncidenceTable))) %>%
                    mutate(DestinationManual = rep(DestinationPrevalence(),length(IncidenceTable))) %>%
                    remove_missing(),
                aes(x = Date)
            ) +
            geom_line(aes(y = if(input$OriginPrevalenceChoice == "Automatic (based on latest WHO data)") { OriginAutomatic } else { OriginManual }), color = "red") +
            geom_line(aes(y = if(input$DestinationPrevalenceChoice == "Automatic (based on latest WHO data)") { DestinationAutomatic } else { DestinationManual }), color = "blue") +
            ylab("Disease prevalence") +
            scale_y_continuous(labels = scales::percent) +
            theme_classic()
        })
    ###########################################################################
    # 2. PRE-DEPARTURE POST-TEST OUTCOMES                                     #
    ###########################################################################
        #######################################################################
        # 2.1. SUMMARY PANEL                                                  #
        #######################################################################
        output$PreDepartureTestResultsSummary <- renderUI({
            HTML(
                ifelse(input$PreDepartureTestMethod != "None",
                    paste(
                       "<ul>",
                           "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) of departing travelers were presumed infected, of which:</li>",
                           "<ul>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPositiveGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPositiveGivenInfectedCount(), format="d", big.mark=","), "</span>) tested positive (true positives) and correctly prevented from boarding.</li>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestNegativeGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenInfectedCount(), format="d", big.mark=","),"</span>) tested negative (false negatives) and incorrectly allowed to board.</li>",
                           "</ul>",
                           "<br>",
                           "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationUninfectedCount(), format="d", big.mark=","), "</span>) of departing travelers were presumed uninfected, of which:</li>",
                           "<ul>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestNegativeGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenUninfectedCount(), format="d", big.mark=","), "</span>) tested negative (true negatives) and are correctly allowed to board.</li>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPositiveGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPositiveGivenUninfectedCount(), format="d", big.mark=","),"</span>) tested positive (false positives) and are only allowed to board after a negative retest.</li>",
                           "</ul>",
                           "<br>",
                           "<li>The likelihood that a traveler is infected or uninfected given a test result is:</li>",
                           "<ul>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestInfectedGivenPositivePercentage(), 1), "</span> chance of being infected in case of a positive test (positive predictive value).</li>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestUninfectedGivenNegativePercentage(), 1), "</span> chance of being uninfected in case of a negative test (negative predictive value).</li>",
                           "</ul>",
                           "<br>",
                           "<li>Pre-departure testing decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(),1),"</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenInfectedCount(), format="d", big.mark=","),"</span>).</li>",
                           "<ul>",
                               "<li>This is <span style=color:#1E32FA>", ifelse(PostArrivalTestPopulationInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the disease prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
                               "<li>It means that there is a <span style=color:#1E32FA>", ifelse(PostArrivalTestPopulationInfectedPercentage() > DestinationPrevalence(), "positive", "negative"), "</span> relative risk of importation of cases after pre-departure testing.</li>",
                           "</ul>",
                       "</ul>",
                       sep = ""
                       ),
                    paste(
                       "<ul>",
                           "<li>No pre-departure test was performed.</li>",
                           "<li>Allll <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) presumed infected travelers boarded their aircraft.</li>",
                       "</ul>",
                       sep = ""
                    )
                )
            )
        })
        #######################################################################
        # 2.2. DETAILS PANEL                                                  #
        #######################################################################
        # 2.1. Starting assumptions
        output$PreDepartureStartingAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Tested passengers (%)", "Clinical sensitivity (%)", "Clinical specificity (%)"), "Value" = c(PreDepartureSampleTestedPercentage(), PreDepartureTestSpecificity(), PreDepartureTestSensitivity()))))
        output$PreDepartureStartingAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = c("Tested passengers (count)", "Limit of detection (copies/ml)"), "Value" = c(PreDepartureSampleTestedCount(), PreDepartureTestLimitOfDetection()))))
        # 2.2 Likelihood that a departing traveler is infected/uninfected before testing
        output$PreDeparturePopulationPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = "Percent", "Infected" = PreDepartureTestPopulationInfectedPercentage(), "Uninfected" = PreDepartureTestPopulationUninfectedPercentage())))
        output$PreDeparturePopulationIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = "Count", "Infected" = PreDepartureTestPopulationInfectedCount(), "Uninfected" = PreDepartureTestPopulationUninfectedCount())))
        # 2.3. Likelihood that a departing traveler gets tested
        output$PreDepartureSamplePercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Tested" = PreDepartureSampleTestedPercentage(), "Untested" = PreDepartureSampleUntestedPercentage())))
        output$PreDepartureSampleIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Tested" = PreDepartureSampleTestedCount(), "Untested" = PreDepartureSampleUntestedCount())))
        # 2.4. Likelihood that a departing air traveler tests positive/negative
        output$PreDepartureTestResultsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Positive" = PreDepartureTestPositivePercentage(), "Negative" = PreDepartureTestNegativePercentage())))
        output$PreDepartureTestResultsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Positive" = PreDepartureTestPositiveCount(), "Negative" = PreDepartureTestNegativeCount())))
        # 2.5. Likelihood that a departing air traveler tests positive/negative, given that they are infected/uninfected
        output$PreDepartureTestResultsGivenInfectionPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Positive", "Negative", "Total"), "Infected" = c(PreDepartureTestPositiveGivenInfectedPercentage(), PreDepartureTestNegativeGivenInfectedPercentage(), PreDepartureTestPositiveGivenInfectedPercentage() + PreDepartureTestNegativeGivenInfectedPercentage()), "Uninfected" = c(PreDepartureTestPositiveGivenUninfectedPercentage(), PreDepartureTestNegativeGivenUninfectedPercentage(), PreDepartureTestPositiveGivenUninfectedPercentage() + PreDepartureTestNegativeGivenUninfectedPercentage()))))
        output$PreDepartureTestResultsGivenInfectionIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Test" = c("Positive", "Negative", "Total"), "Infected" = c(PreDepartureTestPositiveGivenInfectedCount(), PreDepartureTestNegativeGivenInfectedCount(), PreDepartureTestPositiveGivenInfectedCount() + PreDepartureTestNegativeGivenInfectedCount()), "Uninfected" = c(PreDepartureTestPositiveGivenUninfectedCount(), PreDepartureTestNegativeGivenUninfectedCount(), PreDepartureTestPositiveGivenUninfectedCount() + PreDepartureTestNegativeGivenUninfectedCount()))))
        # 2.6. Likelihood that a departing air traveler is infected/uninfected, given that they tested positive/negative
        output$PreDepartureInfectionGivenTestResultsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Infected", "Uninfected", "Total"), "Positive" = c(PreDepartureTestInfectedGivenPositivePercentage(), PreDepartureTestUninfectedGivenPositivePercentage(), PreDepartureTestInfectedGivenPositivePercentage() + PreDepartureTestUninfectedGivenPositivePercentage()), "Negative" = c(PreDepartureTestInfectedGivenNegativePercentage(), PreDepartureTestUninfectedGivenNegativePercentage(), PreDepartureTestInfectedGivenNegativePercentage() + PreDepartureTestUninfectedGivenNegativePercentage()))))
        output$PreDepartureInfectionGivenTestResultsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Test" = c("Infected", "Uninfected", "Total"), "Positive" = c(PreDepartureTestInfectedGivenPositivePercentage() * PreDepartureTestPositiveCount(), PreDepartureTestUninfectedGivenPositivePercentage() * PreDepartureTestPositiveCount(), PreDepartureTestInfectedGivenPositivePercentage() * PreDepartureTestPositiveCount() + PreDepartureTestUninfectedGivenPositivePercentage() * PreDepartureTestPositiveCount()), "Negative" = c(PreDepartureTestInfectedGivenNegativePercentage() * PreDepartureTestNegativeCount(), PreDepartureTestUninfectedGivenNegativePercentage() * PreDepartureTestNegativeCount(), PreDepartureTestInfectedGivenNegativePercentage() * PreDepartureTestNegativeCount() + PreDepartureTestUninfectedGivenNegativePercentage() * PreDepartureTestNegativeCount()))))
        # 2.7. Likelihood that a tested departing air traveler is stopped or allowed to depart
        output$PreDepartureTestClearedPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Stopped" = PreDepartureTestStoppedPercentage(), "Allowed" = PreDepartureTestClearedPercentage())))
        output$PreDepartureTestClearedIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Stopped" = PreDepartureTestStoppedCount(), "Allowed" = PreDepartureTestClearedCount())))
        # 2.8. Likelihood that a departing traveler is infected/uninfected after testing
        output$PreDepartureTestResidualPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Infected" = PreDepartureTestResidualInfectedPercentage(), "Uninfected" = PreDepartureTestResidualUninfectedPercentage())))
        output$PreDepartureTestResidualIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Infected" = PreDepartureTestResidualInfectedCount(), "Uninfected" = PreDepartureTestResidualUninfectedCount())))
        #######################################################################
        # 2.3. CHART PANEL                                                    #
        #######################################################################
        output$PreDepartureTestChartTitle <- renderText("Lorem ipsum dolor sit amet")
    ###########################################################################
    # 3. POST-ARRIVAL PRE-TEST OUTCOMES                                       #
    ###########################################################################
        #######################################################################
        # 3.1. SUMMARY PANEL                                                  #
        #######################################################################
        output$PostArrivalTestResultsSummary <- renderUI({
            HTML(
                ifelse(input$PostArrivalTestMethod != "None",
                    paste(
                        "<ul>",
                            "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) of arriving travelers were presumed infected, of which:</li>",
                            "<ul>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenInfectedCount(), format="d", big.mark=","), "</span>) tested positive (true positives) and correctly allowed to enter the destination.</li>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenInfectedCount(), format="d", big.mark=","),"</span>) tested negative (false negatives) and incorrectly allowed to enter the destination.</li>",
                            "</ul>",
                            "<br>",
                            "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationUninfectedCount(), format="d", big.mark=","), "</span>) of arriving travelers were presumed uninfected, of which:</li>",
                            "<ul>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenUninfectedCount(), format="d", big.mark=","), "</span>) tested negative (true negatives) and are correctly allowed enter the destination.</li>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenUninfectedCount(), format="d", big.mark=","),"</span>) tested positive (false positives) and are only allowed to enter the destination after a negative retest.</li>",
                            "</ul>",
                            "<br>",
                            "<li>The likelihood that a traveler is infected or uninfected given a test result is:</li>",
                            "<ul>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestInfectedGivenPositivePercentage(), 1), "</span> chance of being infected in case of a positive test (positive predictive value).</li>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestUninfectedGivenNegativePercentage(), 1), "</span> chance of being uninfected in case of a negative test (negative predictive value).</li>",
                            "</ul>",
                            "<br>",
                            "<li>Post-arrival testing decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestResidualInfectedPercentage(),1),"</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenInfectedCount(), format="d", big.mark=","),"</span>).</li>",
                            "<ul>",
                                "<li>This is <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the disease prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
                                "<li>It means that there is a <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "positive", "negative"), "</span> relative risk of importation of cases after post-arrival testing.</li>",
                            "</ul>",
                        "</ul>",
                        sep = ""
                       ),
                       paste(
                           "<ul>",
                               "<li>No post-arrival test was performed.</li>",
                               "<li>Allll <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) presumed infected travelers entered the destination.</li>",
                           "</ul>",
                           sep = ""
                       )
                )
            )
        })
        #######################################################################
        # 3.2. DETAILS PANEL                                                  #
        #######################################################################
        # 3.1. Starting assumptions
        output$PostArrivalStartingAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Tested passengers (%)", "Clinical sensitivity (%)", "Clinical specificity (%)"), "Value" = c(PostArrivalSampleTestedPercentage(), PostArrivalTestSpecificity(), PostArrivalTestSensitivity()))))
        output$PostArrivalStartingAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = c("Tested passengers (count)", "Limit of detection (copies/ml)"), "Value" = c(PostArrivalSampleTestedCount(), PostArrivalTestLimitOfDetection()))))
        # 3.2 Likelihood that an arriving traveler is infected/uninfected before testing
        output$PostArrivalPopulationPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = "Percent", "Infected" = PostArrivalTestPopulationInfectedPercentage(), "Uninfected" = PostArrivalTestPopulationUninfectedPercentage())))
        output$PostArrivalPopulationIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = "Count", "Infected" = PostArrivalTestPopulationInfectedCount(), "Uninfected" = PostArrivalTestPopulationUninfectedCount())))
        # 3.3. Likelihood that an arriving traveler gets tested
        output$PostArrivalSamplePercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Tested" = PostArrivalSampleTestedPercentage(), "Untested" = PostArrivalSampleUntestedPercentage())))
        output$PostArrivalSampleIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Tested" = PostArrivalSampleTestedCount(), "Untested" = PostArrivalSampleUntestedCount())))
        # 3.4. Likelihood that an arriving traveler tests positive/negative
        output$PostArrivalTestResultsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Positive" = PostArrivalTestPositivePercentage(), "Negative" = PostArrivalTestNegativePercentage())))
        output$PostArrivalTestResultsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Positive" = PostArrivalTestPositiveCount(), "Negative" = PostArrivalTestNegativeCount())))
        # 3.5. Likelihood that an arriving traveler tests positive/negative, given that they are infected/uninfected
        output$PostArrivalTestResultsGivenInfectionPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Positive", "Negative", "Total"), "Infected" = c(PostArrivalTestPositiveGivenInfectedPercentage(), PostArrivalTestNegativeGivenInfectedPercentage(), PostArrivalTestPositiveGivenInfectedPercentage() + PostArrivalTestNegativeGivenInfectedPercentage()), "Uninfected" = c(PostArrivalTestPositiveGivenUninfectedPercentage(), PostArrivalTestNegativeGivenUninfectedPercentage(), PostArrivalTestPositiveGivenUninfectedPercentage() + PostArrivalTestNegativeGivenUninfectedPercentage()))))
        output$PostArrivalTestResultsGivenInfectionIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Test" = c("Positive", "Negative", "Total"), "Infected" = c(PostArrivalTestPositiveGivenInfectedCount(), PostArrivalTestNegativeGivenInfectedCount(), PostArrivalTestPositiveGivenInfectedCount() + PostArrivalTestNegativeGivenInfectedCount()), "Uninfected" = c(PostArrivalTestPositiveGivenUninfectedCount(), PostArrivalTestNegativeGivenUninfectedCount(), PostArrivalTestPositiveGivenUninfectedCount() + PostArrivalTestNegativeGivenUninfectedCount()))))
        # 3.6. Likelihood that an arriving traveler is infected/uninfected, given that they tested positive/negative
        output$PostArrivalInfectionGivenTestResultsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Infected", "Uninfected", "Total"), "Positive" = c(PostArrivalTestInfectedGivenPositivePercentage(), PostArrivalTestUninfectedGivenPositivePercentage(), PostArrivalTestInfectedGivenPositivePercentage() + PostArrivalTestUninfectedGivenPositivePercentage()), "Negative" = c(PostArrivalTestInfectedGivenNegativePercentage(), PostArrivalTestUninfectedGivenNegativePercentage(), PostArrivalTestInfectedGivenNegativePercentage() + PostArrivalTestUninfectedGivenNegativePercentage()))))
        output$PostArrivalInfectionGivenTestResultsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Test" = c("Infected", "Uninfected", "Total"), "Positive" = c(PostArrivalTestInfectedGivenPositivePercentage() * PostArrivalTestPositiveCount(), PostArrivalTestUninfectedGivenPositivePercentage() * PostArrivalTestPositiveCount(), PostArrivalTestInfectedGivenPositivePercentage() * PostArrivalTestPositiveCount() + PostArrivalTestUninfectedGivenPositivePercentage() * PostArrivalTestPositiveCount()), "Negative" = c(PostArrivalTestInfectedGivenNegativePercentage() * PostArrivalTestNegativeCount(), PostArrivalTestUninfectedGivenNegativePercentage() * PostArrivalTestNegativeCount(), PostArrivalTestInfectedGivenNegativePercentage() * PostArrivalTestNegativeCount() + PostArrivalTestUninfectedGivenNegativePercentage() * PostArrivalTestNegativeCount()))))
        # 3.7. Likelihood that a tested arriving air traveler is stopped or allowed to enter the destination
        output$PostArrivalTestClearedPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Stopped" = PostArrivalTestStoppedPercentage(), "Allowed" = PostArrivalTestClearedPercentage())))
        output$PostArrivalTestClearedIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Stopped" = PostArrivalTestStoppedCount(), "Allowed" = PostArrivalTestClearedCount())))
        # 3.8. Likelihood that an arriving traveler is infected/uninfected after testing
        output$PostArrivalTestResidualPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Infected" = PostArrivalTestResidualInfectedPercentage(), "Uninfected" = PostArrivalTestResidualUninfectedPercentage())))
        output$PostArrivalTestResidualIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Result" = "Count", "Infected" = PostArrivalTestResidualInfectedCount(), "Uninfected" = PostArrivalTestResidualUninfectedCount())))
        #######################################################################
        # 3.3. CHART PANEL                                                    #
        #######################################################################
        output$PostArrivalTestChartTitle <- renderText("Lorem ipsum dolor sit amet")
    ###########################################################################
    # 4. POST-ARRIVAL POST-TEST OUTCOMES                                     #
    ###########################################################################
        #######################################################################
        # 4.1. SUMMARY PANEL                                                  #
        #######################################################################
        output$PostArrivalDestinationSummary <- renderUI({
            HTML(
                if(input$PreDepartureTestMethod == "None" & input$PostArrivalTestMethod == "None") {
                    paste(
                        "<ul>",
                        "<li>No test was performed on departure or arrival, so all <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) presumed infected travelers entered the destination.</li>",
                        "</ul>",
                        sep = ""
                    )
                } else if(input$PreDepartureTestMethod != "None" & input$PostArrivalTestMethod == "None") {
                    paste(
                        "<ul>",
                        "<li>Pre-departure test, but no post-arrival test.</li>",
                        "</ul>",
                        sep = ""
                    )
                } else if(input$PreDepartureTestMethod == "None" & input$PostArrivalTestMethod != "None") {
                    paste(
                        "<ul>",
                        "<li>Only a post-arrival test was performed.</li>",
                        "<li>All <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span>) presumed infected travelers entered the destination.</li>",
                        "</ul>",
                        sep = ""
                    )
                } else if(input$PreDepartureTestMethod != "None" & input$PostArrivalTestMethod != "None") {
                    paste(
                        "<ul>",
                        "<li>Both tests done.</li>",
                        "</ul>",
                        sep = ""
                    )
                }
            )
        })
}
###############################################################################
# RUN THE APP                                                                 #
###############################################################################
shinyApp(ui, server)