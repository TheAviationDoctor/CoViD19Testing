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
library(shinydashboard)     # To add icons
library(shinycssloaders)    # To style the app and spinners in particular
library(shinyWidgets)       # To style the selectors' color
library(tidyverse)          # To wrangle the data
library(zoo)                # To perform rolling means
###############################################################################
# DATA IMPORT/WRANGLING AND VARIABLE DECLARATION                              #
###############################################################################
# URLs for data import
URLEpidemiology         <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
URLPopulation           <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19Testing/main/data/population.csv"
URLTests                <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19Testing/main/data/tests.csv"
URLTraffic              <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19Testing/main/data/traffic2019.csv"
# Import and wrangle the population data
PopulationTable <- pin(URLPopulation) %>% read_csv(na = "", col_types = list(col_factor(), col_integer()))
# Import and wrangle the tests data
TestsTable <- pin(URLTests) %>% read_csv(na = "", col_types = list(col_factor(), col_character(), col_factor(), col_integer(), col_double(), col_double()))
# Import and wrangle the 2019 origin-destination passenger traffic
TrafficTable <- URLTraffic %>% read_csv(na = "", col_types = list(col_factor(), col_factor(), col_integer()))
# Header labels
AppHeader               <- "Air travel COVID-19 testing simulator"
LabelInputPanel1        <- "Disease assumptions"
LabelInputPanel2        <- "Traffic assumptions"
LabelInputPanel3        <- "Test assumptions"
LabelOutputHeader1      <- "1. Pre-departure, pre-test outcomes"
LabelOutputHeader2      <- "2. Pre-departure, post-test outcomes"
LabelOutputHeader3      <- "3. Post-arrival, pre-test outcomes"
LabelOutputHeader4      <- "4. Post-arrival, post-test outcomes"
#Summary content
LabelOutputSummary1    <- "Here, we determine the likelihood that a departing traveler is infected before testing."
LabelOutputSummary2    <- "Here, we determine the effectiveness of pre-departure testing."
LabelOutputSummary3    <- "Here, we determine the effectiveness of post-arrival testing."
LabelOutputSummary4    <- "Here, we determine the residual risk of case importation."
# Input selectors
States                  <- unique(PopulationTable$Country) # We extract the unique states from the states data (they should be unique anyway in the input file, so the use of unique() here is more to be safe)
DefaultOriginState      <- "United States of America"
DefaultDestinationState <- "France"
TestTypes               <- levels(TestsTable$Type) # We extract the unique types of tests from the tests data
###############################################################################
# USER INTERFACE LOGIC                                                        #
###############################################################################
ui <- fluidPage(
    # Globally style the app
    tags$head(
        tags$style("* { font-family: 'Aktiv Grotesk', Arial, sans-serif; !important }"),
        tags$style("hr { border: 1px solid #000000 }")
    ),
    chooseSliderSkin(skin = "Shiny", color = "#1E32FA"),
    # App header
    titlePanel(AppHeader),
    hr(),
    sidebarLayout(
        #######################################################################
        # SIDEBAR PANEL FOR INPUTS                                            #
        #######################################################################
        sidebarPanel(
            hr(),
            # Disease assumptions section
            tags$h4(HTML(paste("<i class='fa fa-virus'></i>", LabelInputPanel1, sep = " "))),
            # Origin characteristics
            radioButtons(inputId = "OriginPrevalenceChoice", label = "Prevalence at origin (%)", choices = list("Manual", "Automatic")),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Manual'",
                sliderInput(inputId = "OriginPrevalence", label = NULL, min = 0, max = 100, step = .1, value = 1.9)
            ),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Automatic'",
                selectInput(inputId = "OriginState", label = NULL, choices = States, selected = DefaultOriginState)
            ),
            # Destination characteristics
            radioButtons(inputId = "DestinationPrevalenceChoice", label = "Prevalence at destination (%)", choices = list("Manual", "Automatic")),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Manual'",
                sliderInput(inputId = "DestinationPrevalence", label = NULL, min = 0, max = 100, step = .1, value = 1.2)
            ),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Automatic'",
                selectInput(inputId = "DestinationState", label = NULL, choices = States, selected = DefaultDestinationState)
            ),
            # Prevalence characteristics
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Automatic' | input.DestinationPrevalenceChoice == 'Automatic'",
                hr(),
                strong("Prevalence calculation options"),
                br(),
                br(),
                sliderInput(inputId = "IncidenceMovingAverage", label = "Moving average of new cases (days)", min = 1, max = 30, step = 1, value = 14),
                sliderInput(inputId = "InfectiousPeriod", label = "Infectious period (days)", min = 1, max = 30, step = 1, value = 12),
                sliderInput(inputId = "NonSymptomaticRate", label = "Non-symptomatic rate", min = 0, max = 100, step = 1, value = 40)
            ),
            hr(),
            # Population assumptions section
            tags$h4(HTML(paste("<i class='fa fa-people-arrows'></i>", LabelInputPanel2, sep = " "))),
            radioButtons(inputId = "PopulationCountChoice", label = "From origin to destination (K pax)", choices = list("Manual", "Automatic")),
            conditionalPanel(
                condition = "input.PopulationCountChoice == 'Manual'",
                sliderInput(inputId = "PopulationCount", label = NULL, min = 0, max = 2.2*10^6, value = 1*10^3)
            ),
            hr(),
            # Test assumptions section
            tags$h4(HTML(paste("<i class='fa fa-microscope'></i>", LabelInputPanel3, sep = " "))),
            # Pre-departure test characteristics
            selectInput(inputId = "PreDepartureTestMethod", label = "Pre-departure method", choices = c("None", TestTypes, "Custom")),
            conditionalPanel(
                condition = "input.PreDepartureTestMethod != 'None'",
                sliderInput(inputId = "PreDepartureTestSampleSize", label = "Sampling of departing travelers (%)", min = 0, max = 100, value = 100),
                sliderInput(inputId = "HoursBeforeDeparture", label = "Hours before boarding", min = -72, max = 0, step = 1, value = -4)
            ),
            conditionalPanel(
                condition = "input.PreDepartureTestMethod == 'Custom'",
                sliderInput(inputId = "PreDepartureTestSensitivity", label = "Clinical sensitivity (%)", min = 0, max = 100, value = 70),
                sliderInput(inputId = "PreDepartureTestSpecificity", label = "Clinical specificity (%)", min = 0, max = 100, value = 95)
            ),
            conditionalPanel(
                condition = "input.PreDepartureTestMethod == 'THIS IS A PLACEHODER FOR FUTURE FEATURE'",
                sliderInput(inputId = "PreDepartureTestLimitOfDetection", label = "Limit of detection (copies/ml)", min = 0, max = 10^4, value = NA)
            ),
            # Post-arrival test characteristics
            selectInput(inputId = "PostArrivalTestMethod", label = "Post-arrival method", choices = c("None", TestTypes, "Custom")),
            conditionalPanel(
                condition = "input.PostArrivalTestMethod != 'None'",
                sliderInput(inputId = "PostArrivalTestSampleSize", label = "Sampling of arriving travelers (%)", min = 0, max = 100, value = 100)
            ),
            conditionalPanel(
                condition = "input.PostArrivalTestMethod == 'Custom'",
                sliderInput(inputId = "PostArrivalTestSensitivity", label = "Clinical sensitivity (%)", min = 0, max = 100, value = 70),
                sliderInput(inputId = "PostArrivalTestSpecificity", label = "Clinical specificity (%)", min = 0, max = 100, value = 95)
            ),
            conditionalPanel(
                condition = "input.PostArrivalTestMethod == 'THIS IS A PLACEHODER FOR FUTURE FEATURE'",
                sliderInput(inputId = "HoursAfterArrival", label = "Hours after unboarding", min = 0, max = 72, step = 1, value = 4),
                sliderInput(inputId = "PostArrivalTestLimitOfDetection", label = "Limit of detection (copies/ml)", min = 0, max = 10^4, value = NA)
            ),
            hr(),
            icon(""),
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
                    )
                )
                ###############################################################
                # 2.3 CHARTS PANEL                                            #
                ###############################################################
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
                    div(align = "center", em("Table 3.7. Likelihood that a tested arriving air traveler is stopped or allowed to enter the destination")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestClearedPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestClearedIntegerTable")))
                    ),
                    br(),
                    div(align = "center", em("Table 3.8. Likelihood that an arriving traveler is infected/uninfected after testing")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResidualPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalTestResidualIntegerTable")))
                    )
                )
                ###############################################################
                # 3.3 CHARTS PANEL                                            #
                ###############################################################
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
                )
                ###############################################################
                # 4.2 DETAILS PANEL                                           #
                ###############################################################
                ###############################################################
                # 4.3 CHARTS PANEL                                            #
                ###############################################################
            )
        )
    )
)
###############################################################################
# SERVER LOGIC                                                                #
###############################################################################
server <- function(input, output) {
    ###########################################################################
    # IMPORT AND WRANGLE DATA REACTIVELY                                      #
    ###########################################################################
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
    OriginPrevalence <- reactive({ ifelse(input$OriginPrevalenceChoice == "Automatic", IncidenceTable()[which(IncidenceTable()$Country == input$OriginState), 6, drop = TRUE], input$OriginPrevalence / 100) })
    DestinationPrevalence <- reactive({ ifelse(input$DestinationPrevalenceChoice == "Automatic", IncidenceTable()[which(IncidenceTable()$Country == input$DestinationState), 6, drop = TRUE], input$DestinationPrevalence / 100) })
    # PreDeparture table 2.1
    PreDepartureTestPopulationCount <- reactive({ ifelse(input$PopulationCountChoice == "Automatic", max(TrafficTable[ which(TrafficTable$Origin == input$OriginState & TrafficTable$Destination == input$DestinationState), 3, drop = TRUE],0), input$PopulationCount * 1000) })
    PreDepartureTestLimitOfDetection <- reactive({ ifelse(input$PreDepartureTestMethod == "None", NA, input$PreDepartureTestLimitOfDetection) })
    PreDepartureTimePenalty <- reactive({ input$HoursBeforeDeparture / 72 + 1 })
    PreDepartureTestSensitivity <- reactive({ if (input$PreDepartureTestMethod == "None") { NA } else if (input$PreDepartureTestMethod == "Custom") { input$PreDepartureTestSensitivity / 100 * PreDepartureTimePenalty() } else { mean(TestsTable$ClinicalSensitivity[TestsTable$Type %in% input$PreDepartureTestMethod]) * PreDepartureTimePenalty() } })
    PreDepartureTestSpecificity <- reactive({ if (input$PreDepartureTestMethod == "None") { NA } else if (input$PreDepartureTestMethod == "Custom") { input$PreDepartureTestSpecificity / 100 } else { mean(TestsTable$ClinicalSpecificity[TestsTable$Type %in% input$PreDepartureTestMethod]) } })
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
    PostArrivalTestSensitivity <- reactive({ if (input$PostArrivalTestMethod == "None") { NA } else if (input$PostArrivalTestMethod == "Custom") { input$PostArrivalTestSensitivity / 100 } else { mean(TestsTable$ClinicalSensitivity[TestsTable$Type %in% input$PostArrivalTestMethod]) } })
    PostArrivalTestSpecificity <- reactive({ if (input$PostArrivalTestMethod == "None") { NA } else if (input$PostArrivalTestMethod == "Custom") { input$PostArrivalTestSpecificity / 100 } else { mean(TestsTable$ClinicalSpecificity[TestsTable$Type %in% input$PostArrivalTestMethod]) } })
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
                    "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) departing travelers are presumed infected.</li>",
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
            geom_line(aes(y = if(input$OriginPrevalenceChoice == "Automatic") { OriginAutomatic } else { OriginManual }), color = "red") +
            geom_line(aes(y = if(input$DestinationPrevalenceChoice == "Automatic") { DestinationAutomatic } else { DestinationManual }), color = "blue") +
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
                            "<li>A <span style=color:#1E32FA>pre-departure</span> test is performed.</li>",
                        "</ul>",
                        "<ul>",
                           "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) of departing travelers are presumed infected, of which:</li>",
                           "<ul>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPositiveGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPositiveGivenInfectedCount(), format="d", big.mark=","), "</span>) test positive (true positives) and are correctly prevented from boarding.</li>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestNegativeGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenInfectedCount(), format="d", big.mark=","),"</span>) test negative (false negatives) and are incorrectly allowed to board.</li>",
                           "</ul>",
                           "<br>",
                           "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationUninfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) of departing travelers are presumed uninfected, of which:</li>",
                           "<ul>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestNegativeGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenUninfectedCount(), format="d", big.mark=","), "</span>) test negative (true negatives) and are correctly allowed to board.</li>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPositiveGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPositiveGivenUninfectedCount(), format="d", big.mark=","),"</span>) test positive (false positives) and must retest (presumably).</li>",
                           "</ul>",
                           "<br>",
                           "<li>The likelihood that a traveler carries the disease given a test result is:</li>",
                           "<ul>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestInfectedGivenPositivePercentage(), 1), "</span> chance of being infected given a positive test (positive predictive value).</li>",
                               "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestUninfectedGivenNegativePercentage(), 1), "</span> chance of being uninfected given a negative test (negative predictive value).</li>",
                           "</ul>",
                       "</ul>",
                       sep = ""
                       ),
                    paste(
                       "<ul>",
                           "<li>Without a pre-departure test, all <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) infected departing travelers will board their aircraft.</li>",
                           "</ul>",
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
        output$PreDepartureStartingAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Tested passengers (%)", "Clinical sensitivity (%)", "Clinical specificity (%)"), "Value" = c(PreDepartureSampleTestedPercentage(), PreDepartureTestSensitivity(), PreDepartureTestSpecificity()))))
        output$PreDepartureStartingAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = c("Tested passengers (count)", "Untested passengers (count)", "Limit of detection (copies/ml)"), "Value" = c(PreDepartureSampleTestedCount(), c(PreDepartureSampleUntestedCount()), PreDepartureTestLimitOfDetection()))))
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
                            "<li>A <span style=color:#1E32FA>post-arrival</span> test is performed.</li>",
                        "</ul>",
                        "<ul>",
                            "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format="d", big.mark=","), "</span>) of arriving travelers are presumed infected, of which:</li>",
                            "<ul>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenInfectedCount(), format="d", big.mark=","), "</span>) test positive (true positives) and are correctly directed to isolate (presumably).</li>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenInfectedCount(), format="d", big.mark=","),"</span>) test negative (false negatives) and are incorrectly allowed to enter the destination.</li>",
                            "</ul>",
                            "<br>",
                            "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationUninfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format="d", big.mark=","), "</span>) of arriving travelers are presumed uninfected, of which:</li>",
                            "<ul>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenUninfectedCount(), format="d", big.mark=","), "</span>) test negative (true negatives) and are correctly allowed enter the destination.</li>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenUninfectedCount(), format="d", big.mark=","),"</span>) test positive (false positives) and must retest (presumably).</li>",
                            "</ul>",
                            "<br>",
                            "<li>The likelihood that a traveler carries the disease given a test result is:</li>",
                            "<ul>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestInfectedGivenPositivePercentage(), 1), "</span> chance of being infected given a positive test (positive predictive value).</li>",
                                "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestUninfectedGivenNegativePercentage(), 1), "</span> chance of being uninfected given a negative test (negative predictive value).</li>",
                            "</ul>",
                        "</ul>",
                        sep = ""
                       ),
                        paste(
                        "<ul>",
                            "<li>Without a post-arrival test, all <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format="d", big.mark=","), "</span>) infected arriving travelers will enter the destination.</li>",
                            "</ul>",
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
        output$PostArrivalStartingAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Tested passengers (%)", "Clinical sensitivity (%)", "Clinical specificity (%)"), "Value" = c(PostArrivalSampleTestedPercentage(), PostArrivalTestSensitivity(), PostArrivalTestSpecificity()))))
        output$PostArrivalStartingAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = c("Tested passengers (count)", "Untested passengers (count)", "Limit of detection (copies/ml)"), "Value" = c(PostArrivalSampleTestedCount(), PostArrivalSampleUntestedCount(), PostArrivalTestLimitOfDetection()))))
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
    # 4. POST-ARRIVAL POST-TEST OUTCOMES                                      #
    ###########################################################################
        #######################################################################
        # 4.1. SUMMARY PANEL                                                  #
        #######################################################################
        output$PostArrivalDestinationSummary <- renderUI({
            HTML(
                # If no test is performed
                if(input$PreDepartureTestMethod == "None" & input$PostArrivalTestMethod == "None") {
                    paste(
                        "<ul>",
                            "<li>Without testing, all <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format="d", big.mark=","), "</span>) infected travelers may become imported cases.</li>",
                        "</ul>",
                        sep = ""
                    )
                # If only a pre-departure test is performed
                } else if(input$PreDepartureTestMethod != "None" & input$PostArrivalTestMethod == "None") {
                    paste(
                        "<ul>",
                            "<li>Pre-departure testing has decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1),"</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenInfectedCount(), format="d", big.mark=","),"</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format="d", big.mark=","), "</span>).</li>",
                            "<ul>",
                                "<li>The residual prevalence is <span style=color:#1E32FA>", ifelse(PostArrivalTestPopulationInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the disease prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
                                "<li>It means that there is a <span style=color:#1E32FA>", ifelse(PostArrivalTestPopulationInfectedPercentage() > DestinationPrevalence(), "positive", "negative"), "</span> relative residual risk of case importation.</li>",
                            "</ul>",
                        "</ul>",
                        sep = ""
                    )
                # If only a post-arrival test is performed
                } else if(input$PreDepartureTestMethod == "None" & input$PostArrivalTestMethod != "None") {
                    paste(
                        "<ul>",
                            "<li>Post-arrival testing has decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestResidualInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format="d", big.mark=","), "</span>).</li>",
                            "<ul>",
                                "<li>The residual prevalence is <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
                                "<li>It means that there is a <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "positive", "negative"), "</span> relative residual risk of case importation.</li>",
                            "</ul>",
                        "</ul>",
                        sep = ""
                    )
                # If both a pre-departure and post-arrival tests are performed
                } else if(input$PreDepartureTestMethod != "None" & input$PostArrivalTestMethod != "None") {
                    paste(
                        "<ul>",
                            "<li>Pre-departure and post-arrival testing have jointly decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format="d", big.mark=","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestResidualInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenInfectedCount(), format="d", big.mark=","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format="d", big.mark=","), "</span>).</li>",
                            "<ul>",
                                "<li>The residual prevalence is <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
                                "<li>It means that there is a <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "positive", "negative"), "</span> relative residual risk of case importation.</li>",
                            "</ul>",
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