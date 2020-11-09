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
LabelPanelInputHeader1  <- "Model inputs"
LabelPanelOutputHeader1 <- "1. Pre-departure, pre-test outcomes"
LabelPanelOutputHeader2 <- "2. Pre-departure, post-test outcomes"
LabelPanelOutputHeader3 <- "3. Post-arrival, pre-test outcomes"
LabelPanelOutputHeader4 <- "4. Post-arrival, post-test outcomes"

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

        ###############################################################################
        # SIDEBAR PANEL FOR INPUTS                                                    #
        ###############################################################################
        
        sidebarPanel(

            #Title
            h3(LabelPanelInputHeader1),

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
                hr(),
                strong("Prevalence calculation options"),
                br(),
                br(),
                condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest state data)' | input.DestinationPrevalenceChoice == 'Automatic (based on latest state data)'",
                sliderInput(inputId = "IncidenceMovingAverage", label = "Moving average of new cases (days)", min = 1, max = 30, step = 1, value = 14),
                sliderInput(inputId = "InfectiousPeriod", label = "Infectious period (days)", min = 1, max = 30, step = 1, value = 12),
                sliderInput(inputId = "NonSymptomaticRate", label = "Non-symptomatic rate", min = 0, max = 100, step = 1, value = 40)
            ),

            # Population characteristics
            hr(),
            radioButtons(inputId = "PopulationCountChoice", label = "Passenger traffic from origin to destination", choices = list("Manual (enter your own)", "Automatic (based on 2019 O&D traffic for that pair)")),
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
                tabPanel("Overview",
                    br(),
                    p(align = "center", "This module assumes a COVID-19 prevalence at origin to determine the likelihood that any given individual who reports for outbound travel is infected. Click on Inputs for assumptions and Outputs for results."),
                ),

                # Assumptions panel
                tabPanel("Assumptions",
                    br(),
                    fluidRow(
                        column(6, DT::dataTableOutput("PrevalenceAssumptionsTable")),
                        column(6, DT::dataTableOutput("TrafficAssumptionsTable"))
                    )
                ),

                # Outputs panel
                tabPanel("Outputs",
                    br(),
                
                    # Table 1.3.1.
                    strong(align = "center", "Probability that a departing air traveler is infected, based on the disease prevalence at origin"),
                    fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("PreDeparturePrevalencePercentageTable"))),
                        column(6, withSpinner(DT::dataTableOutput("PreDeparturePrevalenceHeadcountTable")))
                    )
                ),
                
                # Chart panel
                tabPanel("Charts",
                    br(),
                    strong(align = "center", "Point prevalence at origin and destination"),
                    plotOutput("PrevalenceChart"),
                    strong(align = "center", "Historical prevalence at origin and destination"),
                    plotOutput("HistoricalPrevalenceChart")
                ),
                
                # Data panel
                tabPanel("Data",
                    conditionalPanel(
                        condition = "input.OriginPrevalenceChoice == 'Automatic (based on latest state data)' | input.DestinationPrevalenceChoice == 'Automatic (based on latest state data)'",
                        br(),
                        strong(align = "center", textOutput("IncidenceTableOriginDestinationTitle")),
                        withSpinner(DT::dataTableOutput("IncidenceTableOriginDestination")),
                    ),
                    br(),
                    strong(align = "center", textOutput("IncidenceTableTitle")),
                    withSpinner(DT::dataTableOutput("IncidenceTable"))
                ),

                # Method panel
                tabPanel("Method",
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
                
                # Assumptions panel
                tabPanel("Assumptions",
                         br(),
                         p(align = "center", "Lorem ipsum dolor sit amet.")
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
                
                # Method panel
                tabPanel("Method",
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
                
                # Assumptions panel
                tabPanel("Assumptions",
                         br(),
                         p(align = "center", "Lorem ipsum dolor sit amet.")
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
                
                # Method panel
                tabPanel("Method",
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
                
                # Assumptions panel
                tabPanel("Assumptions",
                         br(),
                         p(align = "center", "Lorem ipsum dolor sit amet.")
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
                
                # Method panel
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
            mutate(MovingAveragePrevalence = rollsum(x = PointPrevalence, input$IncidenceMovingAverage, align = "right", fill = NA) / input$IncidenceMovingAverage) %>% # Calculate the moving average of the average
            arrange(Country, desc(Date))%>%                                     # Final sort by latest date first
            remove_missing(vars = "RollingPrevalence")                          # Remove rows beyond the range of the moving average calculation window
    })

    ###########################################################################
    # VARIABLE DECLARATION                                                    #
    ###########################################################################

    # Set origin prevalence to either that of the selected origin state or to that defined manually by the user
    OriginPrevalence <- reactive({
        ifelse(input$OriginPrevalenceChoice == "Automatic (based on latest state data)", IncidenceTable()[which(IncidenceTable()$Country == input$OriginState), 6, drop = TRUE], input$OriginPrevalence / 100)
    })
    
    # Set destination prevalence to either that of the selected destination state or to that defined manually by the user
    DestinationPrevalence <- reactive({
        ifelse(input$DestinationPrevalenceChoice == "Automatic (based on latest state data)", IncidenceTable()[which(IncidenceTable()$Country == input$DestinationState), 6, drop = TRUE], input$DestinationPrevalence / 100)
    })
    
    # Set traffic to either that of the origin-destination pair for 2019 or to that defined manually by the user
    Traffic <- reactive({
        ifelse(input$PopulationCountChoice == "Automatic (based on 2019 O&D traffic for that pair)", max(TrafficTable[ which(TrafficTable$Origin == input$OriginState & TrafficTable$Destination == input$DestinationState), 3, drop = TRUE],0), input$PopulationCount)
    })

    ###########################################################################
    # 1. PRE-DEPARTURE PRE-TEST OUTCOMES                                      #
    ###########################################################################
    
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
                colnames = c("Assumption", "Value"), rownames = NULL, options = list(dom = "t", ordering = F, paging = FALSE)) %>%
                formatPercentage(columns = 2, digits = 1) %>%
                formatStyle(columns = 2,  color = "#1E32FA")
        )

        output$TrafficAssumptionsTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Assumption" = "Passenger headcount",
                    "Value" = Traffic()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>% formatCurrency(columns = "Value", currency = "", digits = 0) %>% formatStyle(columns = "Value",  color = "#1E32FA")
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
                    "Uninfected" = 1 - OriginPrevalence()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>% formatPercentage(columns = c("Infected", "Uninfected"), digits = 1) %>% formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
        )
    
        # 1.3.1. Render the probability (in headcount) that a departing air traveler is infected
        output$PreDeparturePrevalenceHeadcountTable <- DT::renderDataTable(
            datatable(
                data.frame(
                    "Prevalence" = "Headcount",
                    "Infected" = OriginPrevalence() * Traffic(),
                    "Uninfected" = (1 - OriginPrevalence()) * Traffic()
                ), rownames = NULL, options = list(dom = "t", ordering = F)
            ) %>%
                formatCurrency(columns = c("Infected", "Uninfected"), currency = "", digits = 0) %>%
                formatStyle(columns = c("Infected", "Uninfected"),  color = "#1E32FA")
        )

        #######################################################################
        # 1.4. CHARTS PANEL                                                   #
        #######################################################################
        
        # Render a bar plot to display the disease prevalence at origin and destination
        output$PrevalenceChart <- renderPlot({
            ggplot(
                data = data.frame(
                    "Location" = c(
                        ifelse(input$OriginPrevalenceChoice == "Automatic (based on latest state data)", input$OriginState, "Disease prevalence at origin"),
                        ifelse(input$DestinationPrevalenceChoice == "Automatic (based on latest state data)", input$DestinationState, "Disease prevalence at destination")
                    ),
                    "Prevalence" = c(OriginPrevalence(), DestinationPrevalence())
                ),
                aes(x = reorder(Location, desc(Location)), y = Prevalence)
            ) +
            geom_bar(stat="identity") +
            theme_classic() +
            theme(axis.title.x=element_blank()) +
            scale_y_continuous(labels = scales::percent)
        })
        
        # Render a line plot to display the historical disease prevalence at origin and destination
        output$HistoricalPrevalenceChart <- renderPlot({
            ggplot(
                data = IncidenceTable() %>%
                    filter(Country == input$OriginState | Country == input$DestinationState) %>%
                    select(Date, Country, MovingAveragePrevalence) %>%
                    pivot_wider(names_from = Country, values_from = MovingAveragePrevalence) %>%
                    rename(Destination = input$DestinationState, Origin = input$OriginState) %>%
                    remove_missing(),
                aes(x = Date)
            ) +
            geom_line(aes(y = Origin), color = "blue") +
            geom_line(aes(y = Destination), color = "red") +
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
                    rename("Latest" = "PointPrevalence", "Moving average" = "MovingAveragePrevalence"),
                rownames = NULL, options = list(dom = "t", paging = FALSE)
            )
            %>%
                formatPercentage(columns = c("Latest", "Moving average"), digits = 3) %>%
                formatStyle(columns = c(2:3), color = "#1E32FA")
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