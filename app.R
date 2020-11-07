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
library(DT)                 # To better display datatables
library(pins)               # To locally cache downloaded data for performance
library(shiny)              # to build and display the app in a browser
library(shinycssloaders)    # To style the app and spinners in particular
library(tidyverse)          # To wrangle the data

###############################################################################
# VARIABLE DECLARATION                                                        #
###############################################################################

# Headers
AppHeader <- "Air travel COVID-19 testing simulator"
LabelPanelInputHeader1 <- "Model inputs"
LabelPanelOutputHeader1 <- "1. Pre-departure, pre-test outcomes"
LabelPanelOutputHeader2 <- "2. Pre-departure, post-test outcomes"
LabelPanelOutputHeader3 <- "3. Post-arrival, pre-test outcomes"
LabelPanelOutputHeader4 <- "4. Post-arrival, post-test outcomes"

# URLs
URLPrevalence <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

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

        ###############################################################################
        # SIDEBAR PANEL FOR INPUTS                                                    #
        ###############################################################################
        
        sidebarPanel(

            #Title
            h3(LabelPanelInputHeader1),
            hr(),
            
            # Origin characteristics
            radioButtons(inputId = "OriginPrevalenceChoice", label = "Percentage of disease prevalence at origin", choices = list("Manual (enter your own)", "Automatic (based on latest state data)")),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "OriginPrevalence", label = "", min = 0, max = 100, step = .1, value = 1.9)
            ),
            conditionalPanel(
                condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest state data)'",
                uiOutput("OriginStateSelector")
            ),
            
            # Destination characteristics
            hr(),
            radioButtons(inputId = "DestinationPrevalenceChoice", label = "Percentage of disease  prevalence at destination", choices = list("Manual (enter your own)", "Automatic (based on latest state data)")),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "DestinationPrevalence", label = "", min = 0, max = 100, step = .1, value = 1.2)
            ),
            conditionalPanel(
                condition = "input.DestinationPrevalenceChoice == 'Automatic (based on latest state data)'",
                uiOutput("DestinationStateSelector")
            ),
            
            # Population characteristics
            hr(),
            radioButtons(inputId = "PopulationCountChoice", label = "Number of air travelers", choices = list("Manual (enter your own)", "Automatic (based on 2019 O&D traffic for that pair)")),
            conditionalPanel(
                condition = "input.PopulationCountChoice == 'Manual (enter your own)'",
                sliderInput(inputId = "PopulationCount", label = "", min=0, max=4.5*10^9, value=2.2*10^9)
            ),

            # Pre-departure test characteristics
            hr(),
            radioButtons(inputId = "DepartureTestSampleChoice", label = "Pre-departure test", choices = list("None", "Sample-based (enter a percentage)", "Systematic (all air travelers)")),
            conditionalPanel(
                condition = "input.DepartureTestSampleChoice == 'Sample-based (enter a percentage)'",
                sliderInput(inputId = "DepartureTestSampleSize", label = "Sample size", min=0, max=100, value=100)
            ),
            conditionalPanel(
                condition = "input.DepartureTestSampleChoice != 'None'",
                selectInput(inputId = "DepartureTestMethod", label = "Method", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            ),
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
            

            # Post-arrival test characteristics
            hr(),
            radioButtons(inputId = "ArrivalTestSampleChoice", label = "Post-arrival test", choices = list("None", "Sample-based (enter a percentage)", "Systematic (all air travelers)")),
            conditionalPanel(
                condition = "input.ArrivalTestSampleChoice == 'Sample-based (enter a percentage)'",
                sliderInput(inputId = "ArrivalTestSampleSize", label = "Sample size", min=0, max=100, value=100)
            ),
            conditionalPanel(
                condition = "input.ArrivalTestSampleChoice != 'None'",
                selectInput(inputId = "ArrivalTestMethod", label = "Method", choices = list("None", "Typical RT-PCR", "Typical RT-LAMP", "Typical RT-RPA", "Typical CRISPR-CaS", "Typical RAT", "Manual (design your own)")),
            ),
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
            
        ),
        
        #######################################################################
        # MAIN PANEL FOR OUTPUTS                                              #
        #######################################################################
        
        mainPanel(
            
            ###################################################################
            # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                              #
            ###################################################################
            
            h3(LabelPanelOutputHeader1),
            tabsetPanel(
                
                # Overview panel
                tabPanel("1.1. Overview",
                    br(),
                    p(align = "center", "This module assumes a COVID-19 prevalence at origin to determine the likelihood that any given individual who reports for outbound travel is infected. Click on Inputs for assumptions and Outputs for results."),
                ),

                # Inputs panel
                tabPanel("1.2. Inputs",
                    br(),
                    fluidRow(
                        column(6, DT::dataTableOutput("PrevalenceAssumptionsTable")),
                        column(6, )
                    )
                ),

                # Outputs panel
                tabPanel("1.3. Outputs",
                    br(),
                
                    # Table 1.3.1.
                    p(align = "center", "1.3.1. Probability that a departing air traveler is infected, based on the disease prevalence at origin"),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDeparturePrevalencePercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDeparturePrevalenceHeadcountTable")))
                    )
                ),
                
                # Chart panel
                tabPanel("1.4. Charts",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet.")
                ),
                
                # Data panel
                tabPanel("1.5. Data", withSpinner(DT::dataTableOutput("EUCDCTable"))),
                
                # Method panel
                tabPanel("1.6. Method",
                    br(),
                    conditionalPanel(
                        condition = "input.OriginPrevalenceChoice == 'Manual (enter your own)'",
                        tags$ol(
                            tags$li("We assume that the proportion of infected outbound air travelers is identical to that of the general population in the country of origin (the so-called point prevalence of the disease)."),
                            tags$li("The point prevalence is unknown but we can estimate it from the 14-day cumulative count of daily new cases published daily by the European CDC (the so-called incidence of the disease)."),
                            tags$li("We convert that incidence to a daily average and then apply the CAPSCPA formula to account for infectious period (mean 12 days) and non-symptomatic and unreported cases (40% of total cases)."), 
                            tags$li("With the point prevalence now available as a percentage for every country, we can compare the prevalence at origin and destination, and, therefore, the risk of disease importation before testing."), 
                        )
                    ),
                )
                
            ),
            hr(),
            
            ###################################################################
            # 2. PRE-DEPARTURE POST-TEST OUTCOMES                             #
            ###################################################################

            h3(LabelPanelOutputHeader2),
            tabsetPanel(
                
                # Overview panel
                tabPanel("Overview",
                    br(),
                    p(align = "center", "This module applies the pre-departure test design characteristics to calculate the likelihood of that a traveler tests positive or negative independently of everything else (table 2.1) and given an infection status (table 2.2), and that they are infected or not given a test result (table 2.3). Click on Inputs for assumptions and Outputs for results."),
                ),
                
                # Outputs panel
                tabPanel("Outputs",
                    br(),
                    
                    # Table 2.1
                    p(align = "center", "2.1. Probability that a departing air traveler tests positive/negative"),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestHeadcountTable")))
                    ),
                    br(),
                    
                    # Table 2.2
                    p(align = "center", "2.2. Probability that a departing air traveler tests positive/negative, given that they are infected/uninfected."),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestPriorPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestPriorHeadcountTable")))
                    ),
                    br(),
                    
                    # Table 2.3
                    p(align = "center", "2.3. Probability that a departing air traveler is infected/uninfected, given that they test positive/negative."),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestPosteriorPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDepartureTestPosteriorHeadcountTable")))
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

            h3(LabelPanelOutputHeader3),
            tabsetPanel(
                
                # Overview panel
                tabPanel("Overview",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Outputs panel
                tabPanel("Outputs",
                    br(),
                    
                    # Table 3.1
                    p(align = "center", "3.1. Disease prevalence among air travelers upon arrival (percentage)"),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPrevalencePercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPrevalenceHeadcountTable")))
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

            h3(LabelPanelOutputHeader4),
            tabsetPanel(
            
                # Overview panel
                tabPanel("Overview",
                    br(),
                    p(align = "center", "Lorem ipsum dolor sit amet."),
                ),
                
                # Outputs panel
                tabPanel("Outputs",
                    br(),
                
                    # Table 3.1
                    p(align = "center", "4.1. )"),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPostTestPercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PostArrivalPostTestHeadcountTable")))
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

    ###########################################################################
    # IMPORT AND WRANGLE DATA                                                 #
    ###########################################################################
    
    # Import and wrangle the European Center for Disease Prevention and Control (E.U. CDC) epidemiological data, then calculate the disease prevalence by country per 100 people using the CAPSCA formula
    EUCDCTable <- pin(URLPrevalence) %>%
        read_csv(na = "", col_types = list(col_date(format = "%d/%m/%Y"), col_integer(), col_integer(), col_integer(), col_integer(), col_integer(), col_factor(), col_factor(), col_factor(), col_integer(), col_factor(), col_double())) %>%
        rename("Date" = "dateRep", "Day" = "day", "Month" = "month", "Year" = "year", "Cases" = "cases", "Deaths" = "deaths", "Country" = "countriesAndTerritories", "CountryTwoLetterCode" = "geoId", "CountryThreeLetterCode" = "countryterritoryCode", "Population" = "popData2019", "Continent" = "continentExp", "Incidence" = "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000") %>%
        filter(Continent != "Other") %>%                                      # Filter out an odd entry in the list
        mutate(Country = gsub("_", " ", Country)) %>%                         # Replace underscores with spaces in country names for nicer display
        mutate(Incidence = Incidence / 14) %>%                                # Calculate a daily incidence (new cases per 100,000 people) from the last 14-day cumulative incidence
        mutate(Prevalence = Incidence * 12 / (1 - .6) / 100000) %>%           # Apply the CAPSCPA formula to account for infectious period (mean 12 days) and non-symptomatic and unreported cases (40% of total cases), then convert to a percentage
        group_by(Country) %>% filter(Date == max(Date))                       # Display only the latest row for each country

    ###########################################################################
    # 0. INPUTS PANEL                                                         #
    ###########################################################################
    
    # Render the origin state selector in the inputs panel
    output$OriginStateSelector <- renderUI({
        tagList(
            selectInput(inputId = "OriginState", label = "", choices = EUCDCTable %>% select(Country))
        )
    })

    # Render the destination state selector in the inputs panel (same as origin, except that we exclude the selected origin country since we only model international traffic and not domestic)
    output$DestinationStateSelector <- renderUI({
        tagList(
            selectInput(inputId = "DestinationState", label = "", choices = EUCDCTable[which(EUCDCTable$Country != input$OriginState), 7])
        )
    })

    ###########################################################################
    # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                                      #
    ###########################################################################

    # Set origin prevalence to either that of the selected departure state or to that defined manually by the user
    OriginPrevalence <- reactive({
        ifelse(input$OriginPrevalenceChoice == "Automatic (based on latest state data)", EUCDCTable[ which(EUCDCTable$Country == input$OriginState), 13, drop = TRUE], input$OriginPrevalence / 100)
    })
    
    # Set destination prevalence to either that of the selected arrival state or to that defined manually by the user
    DestinationPrevalence <- reactive({
        ifelse(input$DestinationPrevalenceChoice == "Automatic (based on latest state data)", EUCDCTable[ which(EUCDCTable$Country == input$DestinationState), 13, drop = TRUE], input$DestinationPrevalence / 100)
    })
    
        #######################################################################
        # 1.2. INPUTS PANEL                                                   #
        #######################################################################

        # 1.2.1. Render the active prevalence assumptions in the Inputs panel
        output$PrevalenceAssumptionsTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    c("Disease prevalence at origin", "Disease prevalence at destination"),
                    c(OriginPrevalence(), DestinationPrevalence())
                ),
                colnames = c("Assumption", "Value"), rownames = NULL, options = list(dom = "t", paging = FALSE)) %>%
                formatPercentage(columns = 2, digits = 1) %>%
                formatStyle(columns = 2,  color = "#1E32FA")
        )

        #######################################################################
        # 1.3. OUTPUTS PANEL                                                  #
        #######################################################################

        # 1.3.1. Render the probability (in percentage) that a departing air traveler is infected
        output$PreDeparturePrevalencePercentageTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Prevalence" = "Percentage",
                    "Infected" = OriginPrevalence(),
                    "Uninfected" = 1 - OriginPrevalence(),
                    "Total" = 1
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>% formatPercentage(columns = c("Infected", "Uninfected","Total"), digits = 1) %>% formatStyle(columns = c("Infected", "Uninfected", "Total"),  color = "#1E32FA")
        )
    
        # 1.3.1. Render the probability (in headcount) that a departing air traveler is infected
        output$PreDeparturePrevalenceHeadcountTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Prevalence" = "Headcount",
                    "Infected" = OriginPrevalence() * input$PopulationCount,
                    "Uninfected" = (1 - OriginPrevalence()) * input$PopulationCount,
                    "Total" = input$PopulationCount
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>% formatCurrency(columns = c("Infected", "Uninfected", "Total"), currency = "", digits = 0) %>% formatStyle(columns = c("Infected", "Uninfected", "Total"),  color = "#1E32FA")
        )

        #######################################################################
        # 1.4. DATA PANEL                                                     #
        #######################################################################
    
        # Render the prevalence by country for display in the Data panel
        output$EUCDCTable <- DT::renderDataTable(
            datatable(
                EUCDCTable %>%
                    select(c("Date","Country", "Population", "Incidence", "Prevalence")),
                rownames = NULL,
                options = list(dom = "t", paging = FALSE)
            ) %>%
                formatCurrency(columns = c("Population"), currency = "", digits = 0) %>%
                formatCurrency(columns = "Incidence", currency = "", digits = 2) %>%
                formatPercentage(columns = "Prevalence", digits = 2) %>%
                formatStyle(columns = c("Date","Country", "Population", "Incidence", "Prevalence"),  color = "#1E32FA")
        )

    ###########################################################################
    # 2. PRE-DEPARTURE POST-TEST OUTCOMES                                     #
    ###########################################################################
    
    # 2.1a Build a data frame to store the probability that a test is positive or negative (percentage)
    PreDepartureTestPercentageTable <- reactive({
        PreDepartureTestPercentageLabels = "Percentage"
        PreDepartureTestPercentagePositiveValues = input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())
        PreDepartureTestPercentageNegativeValues = (1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())
        PreDepartureTestPercentageTotal = PreDepartureTestPercentagePositiveValues + PreDepartureTestPercentageNegativeValues
        datatable(data.frame(Test = PreDepartureTestPercentageLabels, Positive = PreDepartureTestPercentagePositiveValues, Negative = PreDepartureTestPercentageNegativeValues, Total = PreDepartureTestPercentageTotal), rownames = NULL, options = list(dom = "t")) %>% formatPercentage(columns = c("Positive", "Negative","Total"), digits = 1) %>% formatStyle(columns = c("Positive", "Negative", "Total"),  color = "#1E32FA")
    })
    
    # 2.1b Render a table to display the probability that a test is positive or negative (percentage)
    output$PreDepartureTestPercentageTable <- DT::renderDataTable(
        PreDepartureTestPercentageTable()
    )
    
    # 2.1c Build a data frame to store the probability that a test is positive or negative (headcount)
    PreDepartureTestHeadcountTable <- reactive({
        PreDepartureTestHeadcountLabels = "Headcount"
        PreDepartureTestHeadcountPositiveValues = (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())) * input$PopulationCount
        PreDepartureTestHeadcountNegativeValues = ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())) * input$PopulationCount
        PreDepartureTestHeadcountTotal = PreDepartureTestHeadcountPositiveValues + PreDepartureTestHeadcountNegativeValues
        datatable(data.frame(Test = PreDepartureTestHeadcountLabels, Positive = PreDepartureTestHeadcountPositiveValues, Negative = PreDepartureTestHeadcountNegativeValues, Total = PreDepartureTestHeadcountTotal), rownames = NULL, options = list(dom = "t")) %>% formatCurrency(columns = c("Positive", "Negative", "Total"), currency = "", digits = 0) %>% formatStyle(columns = c("Positive", "Negative", "Total"),  color = "#1E32FA")
    })
    
    # 2.1d Render a table to display the probability that a test is positive or negative (headcount)
    output$PreDepartureTestHeadcountTable <- DT::renderDataTable(
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
    output$PreDepartureTestPriorPercentageTable <- DT::renderDataTable(
        PreDepartureTestPriorPercentage()
    )
    
    # 2.2c Build a data frame to store the pre-departure test probabilities (headcount)
    PreDepartureTestPriorHeadcount <- reactive({
        PreDepartureTestPriorHeadcountTestLabels = c("Positive", "Negative", "Total")
        PreDepartureTestPriorHeadcountInfectedValues = c(input$DepartureTestSensitivity * input$PopulationCount * OriginPrevalence(), (1 - input$DepartureTestSensitivity) * input$PopulationCount * OriginPrevalence(), input$DepartureTestSensitivity * input$PopulationCount * OriginPrevalence() + (1 - input$DepartureTestSensitivity) * input$PopulationCount * OriginPrevalence())
        PreDepartureTestPriorHeadcountUninfectedValues = c((1 - input$DepartureTestSpecificity) * input$PopulationCount * (1 - OriginPrevalence()), input$DepartureTestSpecificity * input$PopulationCount * (1 - OriginPrevalence()), (1 - input$DepartureTestSpecificity) * input$PopulationCount * (1 - OriginPrevalence()) + input$DepartureTestSpecificity * input$PopulationCount * (1 - OriginPrevalence()))
        datatable(data.frame(Test = PreDepartureTestPriorHeadcountTestLabels, Infected = PreDepartureTestPriorHeadcountInfectedValues,  Uninfected = PreDepartureTestPriorHeadcountUninfectedValues), rownames = NULL, options = list(dom = "t")) %>% formatCurrency(columns = c("Infected", "Uninfected"), currency = "", digits = 0) %>% formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
    })
    
    # 2.2d Render a table to display the pre-departure test prior probabilities (headcount)
    output$PreDepartureTestPriorHeadcountTable <- DT::renderDataTable(
        PreDepartureTestPriorHeadcount()
    )
    
    # 2.3a Build a data frame to store the pre-departure test posterior probabilities (percentage)
    PreDepartureTestPosteriorPercentage <- reactive({
        PreDepartureTestPosteriorPercentageLabels = c("Infected", "Uninfected", "Total")
        PreDepartureTestPosteriorPercentagePositiveValues = c(input$DepartureTestSensitivity * OriginPrevalence() / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())), (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence()) / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())), input$DepartureTestSensitivity * OriginPrevalence() / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())) + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence()) / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())))
        PreDepartureTestPosteriorPercentageNegativeValues = c((1 - input$DepartureTestSensitivity) * OriginPrevalence() / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())), input$DepartureTestSpecificity * (1 - OriginPrevalence()) / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())), (1 - input$DepartureTestSensitivity) * OriginPrevalence() / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())) + input$DepartureTestSpecificity * (1 - OriginPrevalence()) / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())))
        datatable(data.frame(Test = PreDepartureTestPosteriorPercentageLabels, Positive = PreDepartureTestPosteriorPercentagePositiveValues,  Negative = PreDepartureTestPosteriorPercentageNegativeValues), rownames = NULL, options = list(dom = "t")) %>% formatPercentage(columns = c("Positive", "Negative"), digits = 1) %>% formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
    })

    # 2.3b Render a table to store the pre-departure test posterior probabilities (percentage)
    output$PreDepartureTestPosteriorPercentageTable <- DT::renderDataTable(
        PreDepartureTestPosteriorPercentage()
    )

    # 2.3c Build a data frame to store the pre-departure test posterior probabilities (headcount)
    PreDepartureTestPosteriorHeadcount <- reactive({
        PreDepartureTestPosteriorHeadcountLabels = c("Infected", "Uninfected", "Total")
        PreDepartureTestPosteriorHeadcountPositiveValues = c(input$DepartureTestSensitivity * OriginPrevalence() / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())) * input$PopulationCount, (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence()) / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())) * input$PopulationCount, input$DepartureTestSensitivity * OriginPrevalence() / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())) * input$PopulationCount + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence()) / (input$DepartureTestSensitivity * OriginPrevalence() + (1 - input$DepartureTestSpecificity) * (1 - OriginPrevalence())) * input$PopulationCount)
        PreDepartureTestPosteriorHeadcountNegativeValues = c((1 - input$DepartureTestSensitivity) * OriginPrevalence() / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())) * input$PopulationCount, input$DepartureTestSpecificity * (1 - OriginPrevalence()) / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())) * input$PopulationCount, (1 - input$DepartureTestSensitivity) * OriginPrevalence() / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())) * input$PopulationCount + input$DepartureTestSpecificity * (1 - OriginPrevalence()) / ((1 - input$DepartureTestSensitivity) * OriginPrevalence() + input$DepartureTestSpecificity * (1 - OriginPrevalence())) * input$PopulationCount)
        datatable(data.frame(Test = PreDepartureTestPosteriorHeadcountLabels, Positive = PreDepartureTestPosteriorHeadcountPositiveValues,  Negative = PreDepartureTestPosteriorHeadcountNegativeValues), rownames = NULL, options = list(dom = "t")) %>% formatCurrency(columns = c("Positive", "Negative"), currency = "", digits = 0) %>% formatStyle(columns = c("Positive", "Negative"),  color = "#1E32FA")
    })
    
    # 2.3d Render a table to store the pre-departure test posterior probabilities (headcount)
    output$PreDepartureTestPosteriorHeadcountTable <- DT::renderDataTable(
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