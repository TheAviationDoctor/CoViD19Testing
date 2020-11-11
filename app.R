###############################################################################
# AIR TRAVEL COVID-19 TESTING SIMULATOR                                       #
# Created by: Thomas D. Pellegrin                                             #
#             contact@theaviationdoctor.com                                   #
#             https://theaviationdoctor.com                                   #
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
LabelOutputSummary1    <- "This module determines the likelihood that an outbound traveler is infected. Click on the tabs above for details."
LabelOutputSummary2    <- "This module determines the likelihood that an outbound traveler tests positive on departure independently of everything else (table 2.1) and given an infection status (table 2.2), and that they are infected given a test result (table 2.3). Click on the tabs above for details."
LabelOutputSummary3    <- "This module determines the likelihood that an inbound traveler tests positive on arrival independently of everything else (table 3.1) and given an infection status (table 3.2), and that they are infected given a test result (table 3.3). Click on the tabs above for details."
LabelOutputSummary4    <- "This module determines the net importation risk at destination based on all the preceding steps. Click on the tabs above for details."

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
            radioButtons(inputId = "OriginPrevalenceChoice", label = "Disease prevalence at origin", choices = list("Manual (enter your own)", "Automatic (based on latest state data)")),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "OriginPrevalence", label = "", min = 0, max = 100, step = .1, value = 1.9)
            ),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest state data)'",
                selectInput(inputId = "OriginState", label = "", choices = States, selected = DefaultOriginState)
            ),
            
            # Destination characteristics
            hr(),
            radioButtons(inputId = "DestinationPrevalenceChoice", label = "Disease  prevalence at destination", choices = list("Manual (enter your own)", "Automatic (based on latest state data)")),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "DestinationPrevalence", label = "", min = 0, max = 100, step = .1, value = 1.2)
            ),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Automatic (based on latest state data)'",
                selectInput(inputId = "DestinationState", label = "", choices = States, selected = DefaultDestinationState)
            ),
            
            # Prevalence characteristics
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest state data)' | input.DestinationPrevalenceChoice == 'Automatic (based on latest state data)'",
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
                sliderInput(inputId = "PopulationCount", label = "", min=0, max=4.5*10^9, value=2.5875*10^9) # AdJ spoke about 2021 seeing 55-60% of 2019 traffic, so we use 57.5% here
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
                sliderInput(inputId = "PreDepartureTestSensitivity", label = "Clinical sensitivity", min=0, max=1, value=.7),
                sliderInput(inputId = "PreDepartureTestSpecificity", label = "Clinical specificity", min=0, max=1, value=.95)
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
                sliderInput(inputId = "PostArrivalTestSensitivity", label = "Clinical sensitivity", min=0, max=1, value=.7),
                sliderInput(inputId = "PostArrivalTestSpecificity", label = "Clinical specificity", min=0, max=1, value=.95)
            ),
        ),
        
        #######################################################################
        # MAIN PANEL FOR OUTPUTS                                              #
        #######################################################################
        
        mainPanel(
            
            ###################################################################
            # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                              #
            ###################################################################

            DT::dataTableOutput("PrevalenceChartTable"),
            
            h3(LabelOutputHeader1),
            tabsetPanel(
                
                ###############################################################
                # 1.1 SUMMARY PANEL                                          #
                ###############################################################
                
                tabPanel("Summary",
                    br(),
                    uiOutput("PreDepartureInfectionSummary")
                ),

                ###############################################################
                # 1.2 ASSUMPTIONS PANEL                                       #
                ###############################################################
                tabPanel("Assumptions",
                    br(),
                    em(align = "center", textOutput("PrevalenceAssumptionsTitle")),
                    fluidRow(
                        column(6, DT::dataTableOutput("PrevalenceAssumptionsTable")),
                        column(6, DT::dataTableOutput("TrafficAssumptionsTable"))
                    )
                ),

                ###############################################################
                # 1.3 OUTPUTS PANEL                                           #
                ###############################################################
                tabPanel("Outputs",
                    br(),
                    em(align = "center", textOutput("PreDepartureInfectionTitle")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionCountTable")))
                    )
                ),
                
                ###############################################################
                # 1.4 CHARTS PANEL                                            #
                ###############################################################
                tabPanel("Charts",
                    br(),
                    em(align = "center", textOutput("PrevalenceChartTitle")),
                    withSpinner(plotOutput("HistoricalPrevalenceChart"))
                ),
                
                ###############################################################
                # 1.5 DATA PANEL                                              #
                ###############################################################
                tabPanel("Data",
                    br(),
                    em(align = "center", textOutput("IncidenceTableTitle")),
                    withSpinner(DT::dataTableOutput("IncidenceTable"))
                ),

                ###############################################################
                # 1.6 METHOD PANEL                                            #
                ###############################################################
                tabPanel("Method",
                    br(),
                    conditionalPanel(
                        condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest state data)'",
                        tags$ol(
                            tags$li("We assume that the proportion of infected outbound air travelers is identical to that of the general population in the country of origin (the so-called point prevalence of the disease)."),
                            tags$li("The point prevalence is unknown but we can estimate it from the 14-day cumulative count of daily new cases published daily by the European CDC (the so-called incidence of the disease)."),
                            tags$li("We convert that incidence to a daily average and then apply the CAPSCPA formula to account for infectious period (mean 12 days) and non-symptomatic and unreported cases (40% of total cases)."), 
                            tags$li("With the point prevalence now available as a percentage for every country, we can compare the prevalence at origin and destination, and, therefore, the risk of disease importation before testing.")
                        )
                    ),
                    conditionalPanel(
                        condition = "input.OriginPrevalenceChoice == 'Manual (enter your own)'",
                        tags$ol(
                            tags$li("The model applies the disease prevalence at origin to the passenger traffic from origin to destination to calculate the likelihood that a departing traveler is infected.") 
                        )
                    ),
                )
                
            ),
            hr(),
            
            ###################################################################
            # 2. PRE-DEPARTURE POST-TEST OUTCOMES                             #
            ###################################################################

            h3(LabelOutputHeader2),
            tabsetPanel(
                
                ###############################################################
                # 2.1 SUMMARY PANEL                                          #
                ###############################################################
                
                tabPanel("Summary",
                         br(),
                         uiOutput("PreDepartureTestResultsSummary")
                ),
                
                ###############################################################
                # 2.2 ASSUMPTIONS PANEL                                       #
                ###############################################################
                
                tabPanel("Assumptions",
                    br(),
                    em(align = "center", textOutput("PreDepartureTestAssumptionsTitle")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestCharacteristicsAssumptionsTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestPopulationAssumptionsTable")))
                    ),
                ),
                
                ###############################################################
                # 2.3 OUTPUTS PANEL                                           #
                ###############################################################
                tabPanel("Outputs",
                    br(),
                    
                    # Table 2.1
                    em(align = "center", textOutput("PreDepartureTestResultsTitle")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsCountTable")))
                    ),
                    br(),
                    
                    # Table 2.2
                    em(align = "center", textOutput("PreDepartureTestResultsGivenInfectionTitle")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsGivenInfectionPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestResultsGivenInfectionCountTable")))
                    ),
                    br(),
                    
                    # Table 2.3
                    em(align = "center", textOutput("PreDepartureInfectionGivenTestResultsTitle")),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionGivenTestResultsPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionGivenTestResultsCountTable")))
                    ),
                ),
                
                ###############################################################
                # 2.4 CHARTS PANEL                                            #
                ###############################################################
                tabPanel("Chart",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                ###############################################################
                # 2.5 DATA PANEL                                              #
                ###############################################################
                tabPanel("Data",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                ###############################################################
                # 2.6 METHOD PANEL                                            #
                ###############################################################
                tabPanel("Method",
                     br(),
                     p(align = "center", "Lorem ipsum dolor sit amet.")
                )
            ),
            hr(),

            ###################################################################
            # 3. POST-ARRIVAL PRE-TEST OUTCOMES                               #
            ###################################################################

            h3(LabelOutputHeader3),
            tabsetPanel(
                
                ###############################################################
                # 3.1 SUMMARY PANEL                                          #
                ###############################################################
                tabPanel("Summary",
                    br(),
                    p(align = "center", LabelOutputSummary3)
                ),
                
                ###############################################################
                # 3.2 ASSUMPTIONS PANEL                                       #
                ###############################################################
                tabPanel("Assumptions",
                         br(),
                         p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                ###############################################################
                # 3.3 OUTPUTS PANEL                                           #
                ###############################################################
                tabPanel("Outputs",
                    br(),
                    
                    # Table 3.1
                    p(align = "center", "3.1. Disease prevalence among air travelers upon arrival (percentage)"),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPrevalencePercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPrevalenceCountTable")))
                    ),
                ),
                
                ###############################################################
                # 3.4 CHART PANEL                                             #
                ###############################################################
                tabPanel("Chart",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                ###############################################################
                # 3.5 DATA PANEL                                              #
                ###############################################################
                tabPanel("Data",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                ###############################################################
                # 3.6 METHOD PANEL                                            #
                ###############################################################
                tabPanel("Method",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                )
                
            ),
            hr(),

            ###################################################################
            # 4. POST-ARRIVAL POST-TEST OUTCOMES                              #
            ###################################################################

            h3(LabelOutputHeader4),
            tabsetPanel(
            
                ###############################################################
                # 4.1 SUMMARY PANEL                                          #
                ###############################################################
                tabPanel("Summary",
                    br(),
                    p(align = "center", LabelOutputSummary4),
                ),
                
                ###############################################################
                # 4.2 ASSUMPTIONS PANEL                                       #
                ###############################################################
                tabPanel("Assumptions",
                         br(),
                         p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                ###############################################################
                # 4.3 OUTPUTS PANEL                                           #
                ###############################################################
                tabPanel("Outputs",
                    br(),
                
                    # Table 3.1
                    p(align = "center", "4.1. )"),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPostTestPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPostTestCountTable")))
                    ),
                ),
                
                ###############################################################
                # 4.4 CHARTS PANEL                                            #
                ###############################################################
                tabPanel("Chart",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                ###############################################################
                # 4.5 DATA PANEL                                              #
                ###############################################################
                tabPanel("Data",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                ###############################################################
                # 4.6 METHOD PANEL                                            #
                ###############################################################
                tabPanel("Method",
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
    
    # Section 1
    OriginPrevalence <- reactive({ ifelse(input$OriginPrevalenceChoice == "Automatic (based on latest state data)", IncidenceTable()[which(IncidenceTable()$Country == input$OriginState), 6, drop = TRUE], input$OriginPrevalence / 100) })
    DestinationPrevalence <- reactive({ ifelse(input$DestinationPrevalenceChoice == "Automatic (based on latest state data)", IncidenceTable()[which(IncidenceTable()$Country == input$DestinationState), 6, drop = TRUE], input$DestinationPrevalence / 100) })
    PreDepartureCount <- reactive({ ifelse(input$PopulationCountChoice == "Automatic (based on 2019 O&D traffic for that pair)", max(TrafficTable[ which(TrafficTable$Origin == input$OriginState & TrafficTable$Destination == input$DestinationState), 3, drop = TRUE],0), input$PopulationCount) })
    PreDepartureInfectedPercentage <- reactive({ OriginPrevalence() })
    PreDepartureUninfectedPercentage <- reactive({ 1 - OriginPrevalence() })
    DestinationInfectedPercentage <- reactive({ DestinationPrevalence() })
    PreDepartureInfectedCount <- reactive({ OriginPrevalence() * PreDepartureCount() })
    PreDepartureUninfectedCount <- reactive({ (1 - OriginPrevalence()) * PreDepartureCount() })
    # Section 2
    PreDepartureTestLimitOfDetection <- reactive({ ifelse(input$PreDepartureTestMethod != "None", input$PreDepartureTestLimitOfDetection, 0) })
    PreDepartureTestSensitivity <- reactive({ ifelse(input$PreDepartureTestMethod != "None", input$PreDepartureTestSensitivity, 0) })
    PreDepartureTestSpecificity <- reactive({ ifelse(input$PreDepartureTestMethod != "None", input$PreDepartureTestSpecificity, 0) })
    PreDepartureTestPopulationPercentage <- 1
    PreDepartureTestResultsPositivePercentage <- reactive({ PreDepartureTestSensitivity() * OriginPrevalence() + (1 - PreDepartureTestSpecificity()) * (1 - OriginPrevalence()) })
    PreDepartureTestResultsNegativePercentage <- reactive({ (1 - PreDepartureTestSensitivity()) * OriginPrevalence() + PreDepartureTestSpecificity() * (1 - OriginPrevalence()) })
    PreDepartureTestResultsPositiveCount <- reactive({ PreDepartureTestResultsPositivePercentage() * PreDepartureCount() })
    PreDepartureTestResultsNegativeCount <- reactive({ PreDepartureTestResultsNegativePercentage() * PreDepartureCount() })

    ###########################################################################
    # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                                      #
    ###########################################################################

        #######################################################################
        # 1.1. SUMMARY PANEL                                                  #
        #######################################################################

        output$PreDepartureInfectionSummary <- renderUI({ HTML(paste(
            "<ul><li><span style=color:#1E32FA>",
            formattable::percent(PreDepartureInfectedPercentage()),
            "</span> or <span style=color:#1E32FA>",
            formatC(PreDepartureInfectedCount(), format="d", big.mark=','),
            "</span> departing travelers are presumed infected, from the assumed disease prevalence in the general population at origin.</li>",
            "<li>This is <span style=color:#1E32FA>",
            ifelse(PreDepartureInfectedPercentage() > DestinationInfectedPercentage(), "higher", "lower"),
            "</span> than the disease prevalence at destination, which means there is a <span style=color:#1E32FA>",
            ifelse(PreDepartureInfectedPercentage() > DestinationInfectedPercentage(), "positive", "negative"),
            "</span> relative risk of importation of cases before testing.</li></ul>"))
        })
        
        #######################################################################
        # 1.2. ASSUMPTIONS PANEL                                              #
        #######################################################################

        # Render the prevalence assumptions
        output$PrevalenceAssumptionsTitle <- renderText("Disease prevalence at origin and destination (left), and passenger traffic (right)")
        output$PrevalenceAssumptionsTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Assumption" = c("Disease prevalence at origin", "Disease prevalence at destination"),
                    "Value" = c(OriginPrevalence(), DestinationPrevalence())
                ), rownames = NULL, options = list(dom = "t", ordering = F, paging = FALSE)
            ) %>%
                formatPercentage(columns = 2, digits = 1) %>%
                formatStyle(columns = 2,  color = "#1E32FA")
        )
        # Render the traffic assumptions
        output$TrafficAssumptionsTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Assumption" = "Passenger headcount",
                    "Value" = PreDepartureCount()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatCurrency(columns = "Value", currency = "", digits = 0) %>% formatStyle(columns = "Value",  color = "#1E32FA")
        )

        #######################################################################
        # 1.3. OUTPUTS PANEL                                                  #
        #######################################################################

        # Render the pre-departure likelihood of infection
        output$PreDepartureInfectionTitle <- renderText("Likelihood that a departing air traveler is infected, based on the disease prevalence at origin")
        output$PreDepartureInfectionPercentageTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Likelihood" = "Percent",
                    "Infected" = PreDepartureInfectedPercentage(),
                    "Uninfected" = PreDepartureUninfectedPercentage()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatPercentage(columns = c("Infected", "Uninfected"), digits = 1) %>%
                formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
        )
        output$PreDepartureInfectionCountTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Likelihood" = "Count",
                    "Infected" = PreDepartureInfectedCount(),
                    "Uninfected" = PreDepartureUninfectedCount()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatCurrency(columns = c("Infected", "Uninfected"), currency = "", digits = 0) %>%
                formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
        )

        #######################################################################
        # 1.4. CHARTS PANEL                                                   #
        #######################################################################
        
        # Render a line plot to display the historical disease prevalence at origin and destination
        output$PrevalenceChartTitle <- renderText(paste(input$IncidenceMovingAverage, "-day disease prevalence at origin (red) and destination (blue)"))
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
            geom_line(aes(y = if(input$OriginPrevalenceChoice == "Automatic (based on latest state data)") { OriginAutomatic } else { OriginManual }), color = "red") +
            geom_line(aes(y = if(input$DestinationPrevalenceChoice == "Automatic (based on latest state data)") { DestinationAutomatic } else { DestinationManual }), color = "blue") +
            ylab("Disease prevalence") +
            scale_y_continuous(labels = scales::percent) +
            theme_classic()
        })
        
        #######################################################################
        # 1.5. DATA PANEL                                                     #
        #######################################################################

        # Render the prevalence at origin and destination
        output$IncidenceTableOriginDestinationTitle <- renderText(paste("Latest disease point prevalence and ", input$IncidenceMovingAverage, "-day moving average, by origin and destination"))
        output$IncidenceTableOriginDestination <- DT::renderDataTable(
            datatable(
                IncidenceTable() %>%
                    filter(Country == input$OriginState | Country == input$DestinationState) %>%
                    select(Date, Country, MovingAveragePrevalence) %>%
                    pivot_wider(names_from = Country, values_from = MovingAveragePrevalence) %>%
                    rename(Destination = input$DestinationState, Origin = input$OriginState) %>%
                    remove_missing(),
                rownames = NULL, options = list(dom = "t", ordering = F, paging = FALSE)
            ) %>%
                formatPercentage(columns = c(2:3), digits = 3) %>%
                formatStyle(columns = c(2:3), color = "#1E32FA")
        )
        
        # Render the prevalence for all countries
        output$IncidenceTableTitle <- renderText(paste("Latest disease point prevalence and ", input$IncidenceMovingAverage, "-day moving average, by country"))
        output$IncidenceTable <- DT::renderDataTable(
            datatable(
                IncidenceTable() %>%
                    filter(Date == max(Date)) %>%
                    select(Country, PointPrevalence, MovingAveragePrevalence) %>%
                    rename("Latest" = "PointPrevalence", "Moving average" = "MovingAveragePrevalence"
                ), rownames = NULL, options = list(dom = "t", paging = FALSE)
            ) %>%
                formatPercentage(columns = c("Latest", "Moving average"), digits = 3) %>%
                formatStyle(columns = c(2:3), color = "#1E32FA")
        )

    ###########################################################################
    # 2. PRE-DEPARTURE POST-TEST OUTCOMES                                     #
    ###########################################################################

        #######################################################################
        # 2.1. SUMMARY PANEL                                                  #
        #######################################################################
        
        output$PreDepartureTestResultsSummary <- renderUI({ HTML(paste(
            "<ul><li><span style=color:#1E32FA>",
            formattable::percent(PreDepartureTestResultsPositivePercentage()),
            "</span> or <span style=color:#1E32FA>",
            formatC(PreDepartureTestResultsPositiveCount(), format="d", big.mark=','),
            "</span> departing travelers will test positive, based on their disease prevalence and pre-departure test characteristics.</li></ul>"))
        })
        
        #######################################################################
        # 2.2. ASSUMPTIONS PANEL                                              #
        #######################################################################
        
        # Render the test assumptions
        output$PreDepartureTestAssumptionsTitle <- renderText("Pre-departure test characteristics")
        output$PreDepartureTestCharacteristicsAssumptionsTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Assumption" = c("Limit of detection (copies/ml)", "Clinical sensitivity", "Clinical specificity"),
                    "Value" = c(PreDepartureTestSensitivity(), PreDepartureTestSpecificity(), PreDepartureTestLimitOfDetection()
                )
            ), rownames = NULL, options = list(dom = "t", ordering = F, paging = FALSE)) %>%
                formatPercentage(columns = 2, digits = 1) %>%
                formatStyle(columns = 2,  color = "#1E32FA")
        )
        # Render the traffic assumptions
        output$PreDepartureTestPopulationAssumptionsTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Assumption" = c("Tested passengers (%)", "Tested passengers (count)"),
                    "Value" = PreDepartureCount()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>% formatCurrency(columns = "Value", currency = "", digits = 0) %>% formatStyle(columns = "Value",  color = "#1E32FA")
        )
        
        #######################################################################
        # 2.3. OUTPUTS PANEL                                                  #
        #######################################################################
        
        # Render the pre-departure test results
        output$PreDepartureTestResultsTitle <- renderText("Likelihood that a departing air traveler tests positive/negative")
        output$PreDepartureTestResultsPercentageTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Result" = "Percent",
                    "Positive" = PreDepartureTestResultsPositivePercentage(),
                    "Negative" = PreDepartureTestResultsNegativePercentage()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatPercentage(columns = c("Positive", "Negative"), digits = 1) %>%
                formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
        )
        output$PreDepartureTestResultsCountTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Result" = "Count",
                    "Positive" = PreDepartureTestResultsPositiveCount(),
                    "Negative" = PreDepartureTestResultsNegativeCount()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatCurrency(columns = c("Positive", "Negative"), currency = "", digits = 0) %>%
                formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
        )
    
        # Set and render the pre-departure test results given infection    
        PreDepartureTestResultsPositiveGivenInfected <- reactive({ input$PreDepartureTestSensitivity })
        PreDepartureTestResultsPositiveGivenUninfected <- reactive({ 1 - input$PreDepartureTestSpecificity })
        PreDepartureNegativeTestResultsGivenInfected <- reactive({ 1 - input$PreDepartureTestSensitivity })
        PreDepartureTestResultsNegativeGivenUninfected <- reactive({ input$PreDepartureTestSpecificity })
        output$PreDepartureTestResultsGivenInfectionTitle <- renderText("Likelihood that a departing air traveler tests positive/negative, given that they are infected/uninfected")
        output$PreDepartureTestResultsGivenInfectionPercentageTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Test" = c("Positive", "Negative", "Total"),
                    "Infected" = c(PreDepartureTestResultsPositiveGivenInfected(), PreDepartureNegativeTestResultsGivenInfected(), PreDepartureTestResultsPositiveGivenInfected() + PreDepartureNegativeTestResultsGivenInfected()),
                    "Uninfected" = c(PreDepartureTestResultsPositiveGivenUninfected(), PreDepartureTestResultsNegativeGivenUninfected(), PreDepartureTestResultsPositiveGivenUninfected() + PreDepartureTestResultsNegativeGivenUninfected())
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatPercentage(columns = c("Infected", "Uninfected"), digits = 1) %>%
                formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
        )
        output$PreDepartureTestResultsGivenInfectionCountTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Test" = c("Positive", "Negative", "Total"),
                    "Infected" = c(PreDepartureTestResultsPositiveGivenInfected() * PreDepartureInfectedCount(), PreDepartureNegativeTestResultsGivenInfected() * PreDepartureInfectedCount(),PreDepartureTestResultsPositiveGivenInfected() * PreDepartureInfectedCount() + PreDepartureNegativeTestResultsGivenInfected() * PreDepartureInfectedCount()),
                    "Uninfected" = c(PreDepartureTestResultsPositiveGivenUninfected() * PreDepartureUninfectedCount(), PreDepartureTestResultsNegativeGivenUninfected() * PreDepartureUninfectedCount(), PreDepartureTestResultsPositiveGivenUninfected() * PreDepartureUninfectedCount() + PreDepartureTestResultsNegativeGivenUninfected() * PreDepartureUninfectedCount())
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatCurrency(columns = c("Infected", "Uninfected"), currency = "", digits = 0) %>%
                formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
        )
        
        # Set and render the pre-departure infection likelihood given test results (posterior probabilities)
        PreDepartureInfectedGivenPositiveTestResult <- reactive({ input$PreDepartureTestSensitivity * OriginPrevalence() / (input$PreDepartureTestSensitivity * OriginPrevalence() + (1 - input$PreDepartureTestSpecificity) * (1 - OriginPrevalence())) })
        PreDepartureInfectedGivenNegativeTestResult <- reactive({ (1 - input$PreDepartureTestSensitivity) * OriginPrevalence() / ((1 - input$PreDepartureTestSensitivity) * OriginPrevalence() + input$PreDepartureTestSpecificity * (1 - OriginPrevalence())) })
        PreDepartureUninfectedGivenPositiveTestResults <- reactive({ (1 - input$PreDepartureTestSpecificity) * (1 - OriginPrevalence()) / (input$PreDepartureTestSensitivity * OriginPrevalence() + (1 - input$PreDepartureTestSpecificity) * (1 - OriginPrevalence())) })
        PreDepartureUninfectedGivenNegativeTestResults <- reactive({ input$PreDepartureTestSpecificity * (1 - OriginPrevalence()) / ((1 - input$PreDepartureTestSensitivity) * OriginPrevalence() + input$PreDepartureTestSpecificity * (1 - OriginPrevalence())) })
        output$PreDepartureInfectionGivenTestResultsTitle <- renderText("Likelihood that a departing air traveler is infected/uninfected, given that they tested positive/negative")
        output$PreDepartureInfectionGivenTestResultsPercentageTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Test" = c("Infected", "Uninfected", "Total"),
                    "Positive" = c(PreDepartureInfectedGivenPositiveTestResult(), PreDepartureUninfectedGivenPositiveTestResults(), PreDepartureInfectedGivenPositiveTestResult() + PreDepartureUninfectedGivenPositiveTestResults()),
                    "Negative" = c(PreDepartureInfectedGivenNegativeTestResult(), PreDepartureUninfectedGivenNegativeTestResults(), PreDepartureInfectedGivenNegativeTestResult() + PreDepartureUninfectedGivenNegativeTestResults())
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatPercentage(columns = c("Positive", "Negative"), digits = 1) %>%
                formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
        )
        output$PreDepartureInfectionGivenTestResultsCountTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Test" = c("Infected", "Uninfected", "Total"),
                    "Positive" = c(PreDepartureInfectedGivenPositiveTestResult() * PreDepartureTestResultsPositiveCount(), PreDepartureUninfectedGivenPositiveTestResults() * PreDepartureTestResultsPositiveCount(), PreDepartureInfectedGivenPositiveTestResult() * PreDepartureTestResultsPositiveCount() + PreDepartureUninfectedGivenPositiveTestResults() * PreDepartureTestResultsPositiveCount()),
                    "Negative" = c(PreDepartureInfectedGivenNegativeTestResult() * PreDepartureTestResultsNegativeCount(), PreDepartureUninfectedGivenNegativeTestResults() * PreDepartureTestResultsNegativeCount(), PreDepartureInfectedGivenNegativeTestResult() * PreDepartureTestResultsNegativeCount() + PreDepartureUninfectedGivenNegativeTestResults() * PreDepartureTestResultsNegativeCount())
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatCurrency(columns = c("Positive", "Negative"), currency = "", digits = 0) %>%
                formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
        )

        #######################################################################
        # 2.4. CHART PANEL                                                    #
        #######################################################################
        
        #######################################################################
        # 2.5. DATA PANEL                                                     #
        #######################################################################
        
        #######################################################################
        # 2.6. METHOD PANEL                                                   #
        #######################################################################
        
    ###########################################################################
    # 3. POST-ARRIVAL PRE-TEST OUTCOMES                                       #
    ###########################################################################
    
}

###############################################################################
# RUN THE APP                                                                 #
###############################################################################

shinyApp(ui, server)