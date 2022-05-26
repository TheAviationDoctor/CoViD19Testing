# ==============================================================================
#    NAME: app.R
#   INPUT: None
# ACTIONS: Creates a Shiny app
#  OUTPUT: Set of global variables loaded into R's environment
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Clear the console
cat("\014")

# Load required libraries
library(data.table)
library(DT)
library(formattable)
library(pins)
library(plyr)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(styler)
library(tidyverse)
library(zoo)

# ==============================================================================
# 1 Import and wrangle the epidemiological data
# ==============================================================================

# Declare data sources
url_www <- "https://raw.githubusercontent.com"
url_dir <- "owid/covid-19-data/master/public/data/jhu"
url_csv <- "COVID-19%20-%20Johns%20Hopkins%20University.csv"

# Assemble data source
url_epi <- paste(url_www, url_dir, url_csv, sep = "/")

# Cache the data
pin_epi <- pin(x = url_epi)

# Load the data
dt_epi <- fread(
  file   = pin_epi,
  header = TRUE,
  key    = c("Country", "Year"),
  select = list(
    factor  = 1L,
    integer = 2L,
    numeric = 17L
  )
)

# Rename the columns
setnames(
  x = dt_epi,
  1:3,
  c("location", "date", "prevalence")
)

# Set the date of each observation based on the lag value
set(
  x = dt_epi,
  j = "date",
  value = as.Date(x = "2020-01-21", format = "%Y-%m-%d") + dt_epi[, date]
)

# Remove rows with blank prevalence
dt_epi <- na.omit(object = dt_epi, cols = "prevalence")
fwrite(x = dt_epi, file = "temp.csv", append = FALSE)

# Keep only the last observation by location
dt_epi <- dt_epi[, .SD[.N], by = location]

# Convert the prevalence from per million to percent
set(
  x = dt_epi,
  j = "prevalence",
  value = dt_epi[, prevalence] / 10^4
)

# Find and round up the maximum prevalence value for the slider scale
MaximumPrevalence <- ceiling(max(dt_epi[, prevalence]))

# ==============================================================================
# 1 Define test types
# ==============================================================================

TestTypes <- data.table(
  "Manufacturer"          = "ACME",
  "Test"                  = "SuperTest",
  "Type"                  = "PCR",
  "AnalyticalSensitivity" = .7,
  "ClinicalSensitivity"   = .8,
  "ClinicalSpecificity"   = .9
)

# ==============================================================================
# 2 Define the app interface labels
# ==============================================================================

# Header labels
LabelInputPanel2    <- "Traffic assumptions"

# Input selectors
InputLocations      <- unique(dt_epi[, "location"])
InputOrigin         <- "Singapore"
InputDestination    <- "China"

# ==============================================================================
# 3 Client-side interface
# ==============================================================================

ui <- fluidPage(

  # ============================================================================
  # 3.1 Style and header
  # ============================================================================

  tags$head(
    tags$style(
      "* { font-family: 'Aktiv Grotesk', Arial, sans-serif; !important }"
    ),
    tags$style(
      "hr { border: 1px solid #000000 }"
    )
  ),

  chooseSliderSkin(skin = "Shiny", color = "#1E32FA"),

  titlePanel(
      fluidRow(
        column(6, h2("COVID-19 case importation risk assessment tool")),
        column(6, tags$img(
          src = "logo.png",
          height = "90px",
          align = "right"
        )
      )
    )
  ),

  hr(),

  sidebarLayout(

    # ==========================================================================
    # 3.2 Side panel
    # ==========================================================================

    sidebarPanel(

      tabsetPanel(

        # ======================================================================
        # 3.2.1 Assumptions tab
        # ======================================================================

        tabPanel("Assumptions",

          # Header
          tags$h4(
            HTML(
              paste(
                "<i class='fa fa-virus'></i>",
                "Disease assumptions"
              )
            )
          ),

          # Origin selector
          radioButtons(
            inputId    = "OriginPrevalenceChoice",
            label      = "Prevalence at origin (%)",
            choices    = list("Automatic", "Manual")
          ),

          # Origin manual slider
          conditionalPanel(
            condition = "input.OriginPrevalenceChoice == 'Manual'",
            sliderInput(
              inputId  = "OriginPrevalence",
              label    = NULL,
              min      = 0L,
              max      = MaximumPrevalence,
              step     = MaximumPrevalence / 100,
              value    = dt_epi[location == InputOrigin, prevalence]
            )
          ),

          # Origin automatic dropdown
          conditionalPanel(
            condition = "input.OriginPrevalenceChoice == 'Automatic'",
            selectInput(
              inputId  = "OriginLocation",
              label    = NULL,
              choices  = InputLocations,
              selected = InputOrigin
            )
          ),

          # Destination selector
          radioButtons(
            inputId    = "DestinationPrevalenceChoice",
            label      = "Prevalence at destination (%)",
            choices    = list("Automatic", "Manual")
          ),

          # Destination manual slider
          conditionalPanel(
            condition = "input.DestinationPrevalenceChoice == 'Manual'",
            sliderInput(
              inputId  = "DestinationPrevalence",
              label    = NULL,
              min      = 0L,
              max      = MaximumPrevalence,
              step     = MaximumPrevalence / 100,
              value    = dt_epi[location == InputDestination, prevalence]
            )
          ),

          # Destination automatic dropdown
          conditionalPanel(
            condition = "input.DestinationPrevalenceChoice == 'Automatic'",
            selectInput(
              inputId  = "DestinationLocation",
              label    = NULL,
              choices  = InputLocations,
              selected = InputDestination
            )
          ),
    
          # Prevalence assumptions
          conditionalPanel(
            condition = "input.OriginPrevalenceChoice == 'Automatic' | input.DestinationPrevalenceChoice == 'Automatic'",
            hr(),
            strong("Prevalence calculation options"),
            br(),
            br(),
            sliderInput(
              inputId  = "IncidenceMovingAverage",
              label    = "Moving average of new cases (days)",
              min      = 1L,
              max      = 30L,
              step     = 1L,
              value    = 14L
            ),
            sliderInput(
              inputId  = "InfectiousPeriod",
              label    = "Infectious period (days)",
              min      = 1L,
              max      = 30L,
              step     = 1L,
              value    = 12L
            ),
            sliderInput(
              inputId  = "NonSymptomaticRate",
              label    = "Non-symptomatic rate",
              min      = 0L,
              max      = 100L,
              step     = 1L,
              value    = 40L
            )
          ),
          hr(),
    
          # Header
          tags$h4(
            HTML(
              paste(
                "<i class='fa fa-microscope'></i>",
                "Test assumptions"
              )
            )
          ),
    
          # Pre-departure test assumptions
          selectInput(
            inputId    = "PreDepartureTestMethod",
            label      = "Pre-departure method",
            choices    = c("None", TestTypes, "Custom")
          ),
          conditionalPanel(
            condition = "input.PreDepartureTestMethod != 'None'",
            sliderInput(
              inputId  = "PreDepartureTestSampleSize",
              label    = "Sampling of departing travelers (%)",
              min      = 0L,
              max      = 100L,
              value    = 100L
            ),
            sliderInput(
              inputId  = "HoursBeforeDeparture",
              label    = "Hours before boarding",
              min      = -72L,
              max      = 0L,
              step     = 1L,
              value    = -4L
            )
          ),
          conditionalPanel(
            condition = "input.PreDepartureTestMethod == 'Custom'",
            sliderInput(
              inputId  = "PreDepartureTestSensitivity",
              label    = "Clinical sensitivity (%)",
              min      = 0L,
              max      = 100L,
              value    = 70L
            ),
            sliderInput(
              inputId  = "PreDepartureTestSpecificity",
              label    = "Clinical specificity (%)",
              min      = 0L,
              max      = 100L,
              value    = 95L
            )
          ),
          conditionalPanel(
            condition = "input.PreDepartureTestMethod == 'THIS IS A PLACEHOLDER FOR FUTURE FEATURE'",
            sliderInput(
              inputId  = "PreDepartureTestLimitOfDetection",
              label    = "Limit of detection (copies/ml)",
              min      = 0L,
              max      = 10^4,
              value    = 10^3
            )
          ),
    
          # Post-arrival test assumptions
          selectInput(
            inputId = "PostArrivalTestMethod",
            label   = "Post-arrival method",
            choices = c("None", TestTypes, "Custom")
          ),
          conditionalPanel(
            condition = "input.PostArrivalTestMethod != 'None'",
            sliderInput(
              inputId = "PostArrivalTestSampleSize",
              label   = "Sampling of arriving travelers (%)",
              min     = 0L,
              max     = 100L,
              value   = 100L
            )
          ),
          conditionalPanel(
            condition = "input.PostArrivalTestMethod == 'Custom'",
            sliderInput(
              inputId = "PostArrivalTestSensitivity",
              label   = "Clinical sensitivity (%)",
              min     = 0L,
              max     = 100L,
              value   = 70L
            ),
            sliderInput(
              inputId = "PostArrivalTestSpecificity",
              label   = "Clinical specificity (%)",
              min     = 0L,
              max     = 100L,
              value   = 95L
            )
          ),
          conditionalPanel(
            condition = "input.PostArrivalTestMethod == 'THIS IS A PLACEHOLDER FOR FUTURE FEATURE'",
            sliderInput(
              inputId = "HoursAfterArrival",
              label   = "Hours after unboarding",
              min     = 0L,
              max     = 72L,
              step    = 1L,
              value   = 4L
            ),
            sliderInput(
              inputId = "PostArrivalTestLimitOfDetection",
              label   = "Limit of detection (copies/ml)",
              min     = 0L,
              max     = 10^4,
              value   = 10^3
            )
          ),
          icon(name = "", verify_fa = FALSE), # Apparently it's necessary to declare this function somewhere to have the icon classes show up

        ), # End of side panel assumptions tab 

        # ==============================================================================
        # 3.2.2 Data tab
        # ==============================================================================

        tabPanel("Data",

                 tableOutput("PrevalenceTable")

        ) # End of side panel data tab

      ), # End of side panel tab set

    ), # End of side panel

    # ==============================================================================
    # 3.4 Main panel
    # ==============================================================================

    mainPanel(

      # ==============================================================================
      # 3.4.1 Pre-departure, pre-test probabilities
      # ==============================================================================

      # Section header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-home'></i>",
            "Pre-departure, pre-test outcomes"
          )
        )
      ),
      em("Likelihood that a pre-departing traveler is infected."),
      br(),
      br(),

      tabsetPanel(

        # Summary panel
        tabPanel(
          "Summary",
          br(),
          uiOutput("PreDepartureOriginSummary")
        ),

        # Details panel
        tabPanel(
          "Details",
          br(),
          div(
            align = "center",
            em("Table 1.1. Disease prevalence at origin and destination (left), and traveler count (right)")
          ),
          fluidRow(
            column(6, withSpinner(DT::dataTableOutput("PreDepartureAssumptionsPercentageTable"))),
            column(6, withSpinner(DT::dataTableOutput("PreDepartureAssumptionsIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 1.2. Likelihood that a departing air traveler is infected, based on the disease prevalence at origin")
          ),
          fluidRow(
            column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionPercentageTable"))),
            column(6, withSpinner(DT::dataTableOutput("PreDepartureInfectionIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 1.3. Latest disease point prevalence and moving average, by country")
          ),
          fluidRow(
            column(12, withSpinner(DT::dataTableOutput("PreDeparturePrevalencePercentageTable")))
          )
        ),

        # Charts panel
        tabPanel(
          "Charts",
          br(),
          div(
            align = "center", em("Moving average of the disease prevalence at origin (red) and destination (blue)")
          ),
          withSpinner(plotOutput("HistoricalPrevalenceChart"))
        )
      ),
      hr(),

      # ==============================================================================
      # 3.4.2 Pre-departure, post-test probabilities
      # ==============================================================================

      # Section header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-plane-departure'></i>",
            "Pre-departure, post-test outcomes"
          )
        )
      ),
      em("Effectiveness of pre-departure testing."),
      br(),
      br(),

      tabsetPanel(

        # Summary panel
        tabPanel(
          "Summary",
          br(),
          uiOutput("PreDepartureTestResultsSummary")
        ),

        # Details panel
        tabPanel(
          "Details",
          br(),
          div(
            align = "center",
            em("Table 2.1. Departing population and test assumptions")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureStartingAssumptionsPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureStartingAssumptionsIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.2. Likelihood that a departing traveler is infected/uninfected before testing")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDeparturePopulationPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDeparturePopulationIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.3. Likelihood that a departing traveler gets tested")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureSamplePercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureSampleIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.4. Likelihood that a departing air traveler tests positive/negative")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestResultsPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestResultsIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.5. Likelihood that a departing air traveler tests positive/negative, given that they are infected/uninfected")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestResultsGivenInfectionPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestResultsGivenInfectionIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.6. Likelihood that a departing air traveler is infected/uninfected, given that they tested positive/negative")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureInfectionGivenTestResultsPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureInfectionGivenTestResultsIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.7. Likelihood that a tested departing air traveler is stopped or allowed to depart")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestClearedPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestClearedIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.8. Likelihood that a departing traveler is infected/uninfected after testing")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestResidualPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepartureTestResidualIntegerTable")))
          )
        )
      ),
      hr(),

      # ==============================================================================
      # 3.4.3 Post-arrival, pre-test probabilities
      # ==============================================================================

    # Section header
    tags$h4(
      HTML(
        paste(
          "<i class='fa fa-plane-arrival'></i>",
          "Post-arrival, pre-test outcomes"
        )
      )
    ),
    em("Effectiveness of post-arrival testing."),
    br(),
    br(),

    tabsetPanel(
    
        # Summary panel
        tabPanel(
          "Summary",
          br(),
          uiOutput("PostArrivalTestResultsSummary")
        ),

        # Details panel
        tabPanel(
          "Details",
          br(),
          div(
            align = "center",
            em("Table 3.1. Arriving population and test assumptions")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalStartingAssumptionsPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalStartingAssumptionsIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 3.2. Likelihood that an arriving traveler is infected/uninfected before testing")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalPopulationPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalPopulationIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 3.3. Likelihood that an arriving traveler gets tested")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalSamplePercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalSampleIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 3.4. Likelihood that an arriving air traveler tests positive/negative")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResultsPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResultsIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 3.5. Likelihood that an arriving air traveler tests positive/negative, given that they are infected/uninfected")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResultsGivenInfectionPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResultsGivenInfectionIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 3.6. Likelihood that an arriving air traveler is infected/uninfected, given that they tested positive/negative")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalInfectionGivenTestResultsPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalInfectionGivenTestResultsIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 3.7. Likelihood that a tested arriving air traveler is stopped or allowed to enter the destination")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestClearedPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestClearedIntegerTable")))
          ),
          br(),
          div(
            align = "center",
            em("Table 3.8. Likelihood that an arriving traveler is infected/uninfected after testing")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResidualPercentageTable"))),
            column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResidualIntegerTable")))
          )
        )
      ),
      hr(),

      # ==============================================================================
      # 3.4.4 Post-arrival, post-test probabilities
      # ==============================================================================

      # Section header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-hotel'></i>",
            "Post-arrival, post-test outcomes"
          )
        )
      ),
      em("Residual risk of case importation."),
      br(),
      br(),

      tabsetPanel(

        # Summary panel
        tabPanel(
          "Summary",
          br(),
          uiOutput("PostArrivalDestinationSummary")
        )
      )
    )
  )
)

# ==============================================================================
# 4 Server-side logic
# ==============================================================================

server <- function(input, output) {

  # ============================================================================
  # 4.1 Logo
  # ============================================================================

  output$Logo <- renderImage(
    {
      list(
        src = "logo.png",
        height = 110,
        width  = 297.5,
        alt = "Logo"
      )
    },
    deleteFile = FALSE
  )

  IncidenceTable <- reactive({dt_epi})

  PrevalenceTable <- reactive({dt_epi})

  # Starting assumptions
  OriginPrevalence <- reactive({
    ifelse(
      input$OriginPrevalenceChoice == "Automatic",
      IncidenceTable()[which(IncidenceTable()$location == input$DestinationLocation), prevalence],
      input$OriginPrevalence / 100
    )
  })
  DestinationPrevalence <- reactive({
    ifelse(input$DestinationPrevalenceChoice == "Automatic", IncidenceTable()[which(IncidenceTable()$Country == input$DestinationLocation), 6, drop = TRUE], input$DestinationPrevalence / 100)
  })

  # PreDeparture table 2.1
  PreDepartureTestPopulationCount <- reactive({
    ifelse(input$PopulationCountChoice == "Automatic", max(TrafficTable[which(TrafficTable$Origin == input$OriginLocation & TrafficTable$Destination == input$DestinationLocation), 3, drop = TRUE], 0), input$PopulationCount * 1000)
  })
  PreDepartureTestLimitOfDetection <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", NA, input$PreDepartureTestLimitOfDetection)
  })
  PreDepartureTimePenalty <- reactive({
    input$HoursBeforeDeparture / 72 + 1
  })
  PreDepartureTestSensitivity <- reactive({
    if (input$PreDepartureTestMethod == "None") {
      NA
    } else if (input$PreDepartureTestMethod == "Custom") {
      input$PreDepartureTestSensitivity / 100 * PreDepartureTimePenalty()
    } else {
      mean(TestsTable$ClinicalSensitivity[TestsTable$Type %in% input$PreDepartureTestMethod]) * PreDepartureTimePenalty()
    }
  })
  PreDepartureTestSpecificity <- reactive({
    if (input$PreDepartureTestMethod == "None") {
      NA
    } else if (input$PreDepartureTestMethod == "Custom") {
      input$PreDepartureTestSpecificity / 100
    } else {
      mean(TestsTable$ClinicalSpecificity[TestsTable$Type %in% input$PreDepartureTestMethod])
    }
  })
  # PreDeparture table 2.2
  PreDepartureTestPopulationInfectedPercentage <- reactive({
    OriginPrevalence()
  })
  PreDepartureTestPopulationUninfectedPercentage <- reactive({
    1 - OriginPrevalence()
  })
  PreDepartureTestPopulationInfectedCount <- reactive({
    PreDepartureTestPopulationCount() * PreDepartureTestPopulationInfectedPercentage()
  })
  PreDepartureTestPopulationUninfectedCount <- reactive({
    PreDepartureTestPopulationCount() * (1 - PreDepartureTestPopulationInfectedPercentage())
  })
  # PreDeparture table 2.3
  PreDepartureSampleTestedPercentage <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", 0, input$PreDepartureTestSampleSize / 100)
  })
  PreDepartureSampleUntestedPercentage <- reactive({
    1 - PreDepartureSampleTestedPercentage()
  })
  PreDepartureSampleTestedCount <- reactive({
    PreDepartureTestPopulationCount() * PreDepartureSampleTestedPercentage()
  })
  PreDepartureSampleUntestedCount <- reactive({
    PreDepartureTestPopulationCount() - PreDepartureSampleTestedCount()
  })
  # PreDeparture table 2.4
  PreDepartureTestPositivePercentage <- reactive({
    (PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() + (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage()))
  })
  PreDepartureTestNegativePercentage <- reactive({
    ((1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() + PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage()))
  })
  PreDepartureTestPositiveCount <- reactive({
    PreDepartureSampleTestedCount() * PreDepartureTestPositivePercentage()
  })
  PreDepartureTestNegativeCount <- reactive({
    PreDepartureSampleTestedCount() * PreDepartureTestNegativePercentage()
  })
  # PreDeparture table 2.5
  PreDepartureTestPositiveGivenInfectedPercentage <- reactive({
    PreDepartureTestSensitivity() * PreDepartureSampleTestedPercentage()
  })
  PreDepartureTestPositiveGivenUninfectedPercentage <- reactive({
    (1 - PreDepartureTestSpecificity()) * PreDepartureSampleTestedPercentage()
  })
  PreDepartureTestNegativeGivenInfectedPercentage <- reactive({
    (1 - PreDepartureTestSensitivity()) * PreDepartureSampleTestedPercentage()
  })
  PreDepartureTestNegativeGivenUninfectedPercentage <- reactive({
    PreDepartureTestSpecificity() * PreDepartureSampleTestedPercentage()
  })
  PreDepartureUntestedGivenInfectedPercentage <- reactive({
    1 - PreDepartureTestPositiveGivenInfectedPercentage() - PreDepartureTestNegativeGivenInfectedPercentage()
  })
  PreDepartureUntestedGivenUninfectedPercentage <- reactive({
    1 - PreDepartureTestPositiveGivenUninfectedPercentage() - PreDepartureTestNegativeGivenUninfectedPercentage()
  })
  PreDepartureTestPositiveGivenInfectedCount <- reactive({
    PreDepartureTestPopulationInfectedCount() * PreDepartureTestPositiveGivenInfectedPercentage()
  })
  PreDepartureTestPositiveGivenUninfectedCount <- reactive({
    PreDepartureTestPopulationUninfectedCount() * PreDepartureTestPositiveGivenUninfectedPercentage()
  })
  PreDepartureTestNegativeGivenInfectedCount <- reactive({
    PreDepartureTestPopulationInfectedCount() * PreDepartureTestNegativeGivenInfectedPercentage()
  })
  PreDepartureTestNegativeGivenUninfectedCount <- reactive({
    PreDepartureTestPopulationUninfectedCount() * PreDepartureTestNegativeGivenUninfectedPercentage()
  })
  PreDepartureUntestedGivenInfectedCount <- reactive({
    PreDepartureTestPopulationInfectedCount() * PreDepartureUntestedGivenInfectedPercentage()
  })
  PreDepartureUntestedGivenUninfectedCount <- reactive({
    PreDepartureTestPopulationUninfectedCount() * PreDepartureUntestedGivenUninfectedPercentage()
  })
  # PreDeparture table 2.6
  PreDepartureTestInfectedGivenPositivePercentage <- reactive({
    PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() / (PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() + (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage()))
  })
  PreDepartureTestInfectedGivenNegativePercentage <- reactive({
    (1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() / ((1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() + PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage()))
  })
  PreDepartureTestUninfectedGivenPositivePercentage <- reactive({
    (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage()) / (PreDepartureTestSensitivity() * PreDepartureTestPopulationInfectedPercentage() + (1 - PreDepartureTestSpecificity()) * (1 - PreDepartureTestPopulationInfectedPercentage()))
  })
  PreDepartureTestUninfectedGivenNegativePercentage <- reactive({
    PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage()) / ((1 - PreDepartureTestSensitivity()) * PreDepartureTestPopulationInfectedPercentage() + PreDepartureTestSpecificity() * (1 - PreDepartureTestPopulationInfectedPercentage()))
  })
  PreDepartureTestInfectedGivenPositiveCount <- reactive({
    PreDepartureTestInfectedGivenPositivePercentage() * PreDepartureTestPositiveCount()
  })
  PreDepartureTestInfectedGivenNegativeCount <- reactive({
    PreDepartureTestInfectedGivenNegativePercentage() * PreDepartureTestNegativeCount()
  })
  PreDepartureTestUninfectedGivenPositiveCount <- reactive({
    PreDepartureTestUninfectedGivenPositivePercentage() * PreDepartureTestPositiveCount()
  })
  PreDepartureTestUninfectedGivenNegativeCount <- reactive({
    PreDepartureTestUninfectedGivenNegativePercentage() * PreDepartureTestNegativeCount()
  })
  # PreDeparture table 2.7
  PreDepartureTestClearedPercentage <- reactive({
    PreDepartureTestClearedCount() / PreDepartureTestPopulationCount()
  })
  PreDepartureTestStoppedPercentage <- reactive({
    1 - PreDepartureTestClearedPercentage()
  })
  PreDepartureTestClearedCount <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationCount(), PreDepartureTestPopulationCount() - PreDepartureTestPositiveGivenInfectedCount())
  })
  PreDepartureTestStoppedCount <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", 0, PreDepartureTestPositiveGivenInfectedCount())
  })
  # PreDeparture table 2.8
  PreDepartureTestResidualInfectedPercentage <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationInfectedPercentage(), (PreDepartureTestPopulationInfectedCount() - PreDepartureTestPositiveGivenInfectedCount()) / PreDepartureTestPopulationCount())
  })
  PreDepartureTestResidualUninfectedPercentage <- reactive({
    1 - PreDepartureTestResidualInfectedPercentage()
  })
  PreDepartureTestResidualInfectedCount <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationInfectedCount(), PreDepartureTestPopulationInfectedCount() - PreDepartureTestPositiveGivenInfectedCount())
  })
  PreDepartureTestResidualUninfectedCount <- reactive({
    PreDepartureTestPopulationUninfectedCount()
  })
  # PostArrival table 2.1
  PostArrivalTestPopulationCount <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationCount(), PreDepartureTestPopulationCount() - PreDepartureTestPositiveGivenInfectedCount())
  })
  PostArrivalTestLimitOfDetection <- reactive({
    ifelse(input$PostArrivalTestMethod == "None", NA, input$PostArrivalTestLimitOfDetection)
  })
  PostArrivalTestSensitivity <- reactive({
    if (input$PostArrivalTestMethod == "None") {
      NA
    } else if (input$PostArrivalTestMethod == "Custom") {
      input$PostArrivalTestSensitivity / 100
    } else {
      mean(TestsTable$ClinicalSensitivity[TestsTable$Type %in% input$PostArrivalTestMethod])
    }
  })
  PostArrivalTestSpecificity <- reactive({
    if (input$PostArrivalTestMethod == "None") {
      NA
    } else if (input$PostArrivalTestMethod == "Custom") {
      input$PostArrivalTestSpecificity / 100
    } else {
      mean(TestsTable$ClinicalSpecificity[TestsTable$Type %in% input$PostArrivalTestMethod])
    }
  })
  # PostArrival table 2.2
  PostArrivalTestPopulationInfectedPercentage <- reactive({
    ifelse(input$PreDepartureTestMethod == "None", PreDepartureTestPopulationInfectedPercentage(), (PreDepartureTestNegativeGivenInfectedCount() + PreDepartureUntestedGivenInfectedCount()) / PostArrivalTestPopulationCount())
  })
  PostArrivalTestPopulationUninfectedPercentage <- reactive({
    1 - PostArrivalTestPopulationInfectedPercentage()
  })
  PostArrivalTestPopulationInfectedCount <- reactive({
    PostArrivalTestPopulationCount() * PostArrivalTestPopulationInfectedPercentage()
  })
  PostArrivalTestPopulationUninfectedCount <- reactive({
    PostArrivalTestPopulationCount() * (1 - PostArrivalTestPopulationInfectedPercentage())
  })
  # PostArrival table 2.3
  PostArrivalSampleTestedPercentage <- reactive({
    ifelse(input$PostArrivalTestMethod == "None", 0, input$PostArrivalTestSampleSize / 100)
  })
  PostArrivalSampleUntestedPercentage <- reactive({
    1 - PostArrivalSampleTestedPercentage()
  })
  PostArrivalSampleTestedCount <- reactive({
    PostArrivalTestPopulationCount() * PostArrivalSampleTestedPercentage()
  })
  PostArrivalSampleUntestedCount <- reactive({
    PostArrivalTestPopulationCount() - PostArrivalSampleTestedCount()
  })
  # PostArrival table 2.4
  PostArrivalTestPositivePercentage <- reactive({
    (PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() + (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage()))
  })
  PostArrivalTestNegativePercentage <- reactive({
    ((1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() + PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage()))
  })
  PostArrivalTestPositiveCount <- reactive({
    PostArrivalSampleTestedCount() * PostArrivalTestPositivePercentage()
  })
  PostArrivalTestNegativeCount <- reactive({
    PostArrivalSampleTestedCount() * PostArrivalTestNegativePercentage()
  })
  # PostArrival table 2.5
  PostArrivalTestPositiveGivenInfectedPercentage <- reactive({
    PostArrivalTestSensitivity() * PostArrivalSampleTestedPercentage()
  })
  PostArrivalTestPositiveGivenUninfectedPercentage <- reactive({
    (1 - PostArrivalTestSpecificity()) * PostArrivalSampleTestedPercentage()
  })
  PostArrivalTestNegativeGivenInfectedPercentage <- reactive({
    (1 - PostArrivalTestSensitivity()) * PostArrivalSampleTestedPercentage()
  })
  PostArrivalTestNegativeGivenUninfectedPercentage <- reactive({
    PostArrivalTestSpecificity() * PostArrivalSampleTestedPercentage()
  })
  PostArrivalUntestedGivenInfectedPercentage <- reactive({
    1 - PostArrivalTestPositiveGivenInfectedPercentage() - PostArrivalTestNegativeGivenInfectedPercentage()
  })
  PostArrivalUntestedGivenUninfectedPercentage <- reactive({
    1 - PostArrivalTestPositiveGivenUninfectedPercentage() - PostArrivalTestNegativeGivenUninfectedPercentage()
  })
  PostArrivalTestPositiveGivenInfectedCount <- reactive({
    PostArrivalTestPopulationInfectedCount() * PostArrivalTestPositiveGivenInfectedPercentage()
  })
  PostArrivalTestPositiveGivenUninfectedCount <- reactive({
    PostArrivalTestPopulationUninfectedCount() * PostArrivalTestPositiveGivenUninfectedPercentage()
  })
  PostArrivalTestNegativeGivenInfectedCount <- reactive({
    PostArrivalTestPopulationInfectedCount() * PostArrivalTestNegativeGivenInfectedPercentage()
  })
  PostArrivalTestNegativeGivenUninfectedCount <- reactive({
    PostArrivalTestPopulationUninfectedCount() * PostArrivalTestNegativeGivenUninfectedPercentage()
  })
  PostArrivalUntestedGivenInfectedCount <- reactive({
    PostArrivalTestPopulationInfectedCount() * PostArrivalUntestedGivenInfectedPercentage()
  })
  PostArrivalUntestedGivenUninfectedCount <- reactive({
    PostArrivalTestPopulationUninfectedCount() * PostArrivalUntestedGivenUninfectedPercentage()
  })
  # PostArrival table 2.6
  PostArrivalTestInfectedGivenPositivePercentage <- reactive({
    PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() / (PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() + (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage()))
  })
  PostArrivalTestInfectedGivenNegativePercentage <- reactive({
    (1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() / ((1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() + PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage()))
  })
  PostArrivalTestUninfectedGivenPositivePercentage <- reactive({
    (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage()) / (PostArrivalTestSensitivity() * PostArrivalTestPopulationInfectedPercentage() + (1 - PostArrivalTestSpecificity()) * (1 - PostArrivalTestPopulationInfectedPercentage()))
  })
  PostArrivalTestUninfectedGivenNegativePercentage <- reactive({
    PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage()) / ((1 - PostArrivalTestSensitivity()) * PostArrivalTestPopulationInfectedPercentage() + PostArrivalTestSpecificity() * (1 - PostArrivalTestPopulationInfectedPercentage()))
  })
  PostArrivalTestInfectedGivenPositiveCount <- reactive({
    PostArrivalTestInfectedGivenPositivePercentage() * PostArrivalTestPositiveCount()
  })
  PostArrivalTestInfectedGivenNegativeCount <- reactive({
    PostArrivalTestInfectedGivenNegativePercentage() * PostArrivalTestNegativeCount()
  })
  PostArrivalTestUninfectedGivenPositiveCount <- reactive({
    PostArrivalTestUninfectedGivenPositivePercentage() * PostArrivalTestPositiveCount()
  })
  PostArrivalTestUninfectedGivenNegativeCount <- reactive({
    PostArrivalTestUninfectedGivenNegativePercentage() * PostArrivalTestNegativeCount()
  })
  # PostArrival table 2.7
  PostArrivalTestClearedPercentage <- reactive({
    PostArrivalTestClearedCount() / PostArrivalTestPopulationCount()
  })
  PostArrivalTestStoppedPercentage <- reactive({
    1 - PostArrivalTestClearedPercentage()
  })
  PostArrivalTestClearedCount <- reactive({
    ifelse(input$PostArrivalTestMethod == "None", PostArrivalTestPopulationCount(), PostArrivalSampleTestedCount() - PostArrivalTestPositiveGivenInfectedCount())
  })
  PostArrivalTestStoppedCount <- reactive({
    ifelse(input$PostArrivalTestMethod == "None", 0, PostArrivalTestPositiveGivenInfectedCount())
  })
  # PostArrival table 2.8
  PostArrivalTestResidualInfectedPercentage <- reactive({
    ifelse(input$PostArrivalTestMethod == "None", PostArrivalTestPopulationInfectedPercentage(), (PostArrivalTestNegativeGivenInfectedCount() + PostArrivalUntestedGivenInfectedCount()) / PostArrivalTestPopulationCount())
  })
  PostArrivalTestResidualUninfectedPercentage <- reactive({
    1 - PostArrivalTestResidualInfectedPercentage()
  })
  PostArrivalTestResidualInfectedCount <- reactive({
    ifelse(input$PostArrivalTestMethod == "None", PostArrivalTestPopulationInfectedCount(), PostArrivalTestNegativeGivenInfectedCount() + PostArrivalUntestedGivenInfectedCount())
  })
  PostArrivalTestResidualUninfectedCount <- reactive({
    PostArrivalTestPopulationUninfectedCount()
  })
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
        "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format = "d", big.mark = ","), "</span>) departing travelers are presumed infected.</li>",
        "<ul>",
        "<li>This is <span style=color:#1E32FA>", ifelse(PreDepartureTestPopulationInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the disease prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
        "</ul>",
        "</ul>",
        sep = ""
      )
    )
  })
  #######################################################################
  # 1.2. DETAILS PANEL                                                  #
  #######################################################################
  # Table 1.1. Disease prevalence at origin and destination (left), and traveler count (right)
  output$PreDepartureAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Prevalence at origin", "Prevalence at destination"), "Value" = c(OriginPrevalence(), DestinationPrevalence()))))
  output$PreDepartureAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = "Traveler count", "Value" = PreDepartureTestPopulationCount())))
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
        filter(Country == input$OriginLocation | Country == input$DestinationLocation) %>%
        select(Date, Country, MovingAveragePrevalence) %>%
        pivot_wider(names_from = Country, values_from = MovingAveragePrevalence) %>%
        rename(DestinationAutomatic = input$DestinationLocation, OriginAutomatic = input$OriginLocation) %>%
        mutate(OriginManual = rep(OriginPrevalence(), length(IncidenceTable))) %>%
        mutate(DestinationManual = rep(DestinationPrevalence(), length(IncidenceTable))) %>%
        remove_missing(),
      aes(x = Date)
    ) +
      geom_line(aes(y = if (input$OriginPrevalenceChoice == "Automatic") {
        OriginAutomatic
      } else {
        OriginManual
      }), color = "red") +
      geom_line(aes(y = if (input$DestinationPrevalenceChoice == "Automatic") {
        DestinationAutomatic
      } else {
        DestinationManual
      }), color = "blue") +
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
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format = "d", big.mark = ","), "</span>) of departing travelers are presumed infected, of which:</li>",
          "<ul>",
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPositiveGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPositiveGivenInfectedCount(), format = "d", big.mark = ","), "</span>) test positive (true positives) and are correctly prevented from boarding.</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestNegativeGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenInfectedCount(), format = "d", big.mark = ","), "</span>) test negative (false negatives) and are incorrectly allowed to board.</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureUntestedGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureUntestedGivenInfectedCount(), format = "d", big.mark = ","), "</span>) are untested and incorrectly allowed to board.</li>",
          "</ul>",
          "<br>",
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationUninfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format = "d", big.mark = ","), "</span>) of departing travelers are presumed uninfected, of which:</li>",
          "<ul>",
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestNegativeGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestNegativeGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) test negative (true negatives) and are correctly allowed to board.</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureTestPositiveGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPositiveGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) test positive (false positives) and are correctly allowed to board after a retest.</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PreDepartureUntestedGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureUntestedGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) are untested and correctly allowed to board.</li>",
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
          "<li>Without a pre-departure test, all <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format = "d", big.mark = ","), "</span>) infected departing travelers will board their aircraft.</li>",
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
  output$PreDepartureStartingAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Tested travelers (%)", "Clinical sensitivity (%)", "Clinical specificity (%)"), "Value" = c(PreDepartureSampleTestedPercentage(), PreDepartureTestSensitivity(), PreDepartureTestSpecificity()))))
  output$PreDepartureStartingAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = c("Tested travelers (count)", "Untested travelers (count)"), "Value" = c(PreDepartureSampleTestedCount(), c(PreDepartureSampleUntestedCount())))))
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
  output$PreDepartureTestResultsGivenInfectionPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Positive", "Negative", "Untested", "Total"), "Infected" = c(PreDepartureTestPositiveGivenInfectedPercentage(), PreDepartureTestNegativeGivenInfectedPercentage(), PreDepartureUntestedGivenInfectedPercentage(), PreDepartureTestPositiveGivenInfectedPercentage() + PreDepartureTestNegativeGivenInfectedPercentage() + PreDepartureUntestedGivenInfectedPercentage()), "Uninfected" = c(PreDepartureTestPositiveGivenUninfectedPercentage(), PreDepartureTestNegativeGivenUninfectedPercentage(), PreDepartureUntestedGivenUninfectedPercentage(), PreDepartureTestPositiveGivenUninfectedPercentage() + PreDepartureTestNegativeGivenUninfectedPercentage() + PreDepartureUntestedGivenUninfectedPercentage()))))
  output$PreDepartureTestResultsGivenInfectionIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Test" = c("Positive", "Negative", "Untested", "Total"), "Infected" = c(PreDepartureTestPositiveGivenInfectedCount(), PreDepartureTestNegativeGivenInfectedCount(), PreDepartureUntestedGivenInfectedCount(), PreDepartureTestPositiveGivenInfectedCount() + PreDepartureTestNegativeGivenInfectedCount() + PreDepartureUntestedGivenInfectedCount()), "Uninfected" = c(PreDepartureTestPositiveGivenUninfectedCount(), PreDepartureTestNegativeGivenUninfectedCount(), PreDepartureUntestedGivenUninfectedCount(), PreDepartureTestPositiveGivenUninfectedCount() + PreDepartureTestNegativeGivenUninfectedCount() + PreDepartureUntestedGivenUninfectedCount()))))
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
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>) of arriving travelers are presumed infected, of which:</li>",
          "<ul>",
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenInfectedCount(), format = "d", big.mark = ","), "</span>) test positive (true positives) and are correctly directed to isolate (presumably).</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenInfectedCount(), format = "d", big.mark = ","), "</span>) test negative (false negatives) and are incorrectly allowed to enter the destination.</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalUntestedGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalUntestedGivenInfectedCount(), format = "d", big.mark = ","), "</span>) are untested and incorrectly allowed to enter the destination.</li>",
          "</ul>",
          "<br>",
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationUninfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>) of arriving travelers are presumed uninfected, of which:</li>",
          "<ul>",
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) test negative (true negatives) and are correctly allowed enter the destination.</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) test positive (false positives) and are correctly allowed enter the destination after a retest (presumably).</li>",
          "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalUntestedGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalUntestedGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) are untested and correctly allowed to enter the destination.</li>",
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
          "<li>Without a post-arrival test, all <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>) infected arriving travelers will enter the destination.</li>",
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
  output$PostArrivalStartingAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Tested travelers (%)", "Clinical sensitivity (%)", "Clinical specificity (%)"), "Value" = c(PostArrivalSampleTestedPercentage(), PostArrivalTestSensitivity(), PostArrivalTestSpecificity()))))
  output$PostArrivalStartingAssumptionsIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Assumption" = c("Tested travelers (count)", "Untested travelers (count)"), "Value" = c(PostArrivalSampleTestedCount(), PostArrivalSampleUntestedCount()))))
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
  output$PostArrivalTestResultsGivenInfectionPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Positive", "Negative", "Untested", "Total"), "Infected" = c(PostArrivalTestPositiveGivenInfectedPercentage(), PostArrivalTestNegativeGivenInfectedPercentage(), PostArrivalUntestedGivenInfectedPercentage(), PostArrivalTestPositiveGivenInfectedPercentage() + PostArrivalTestNegativeGivenInfectedPercentage() + PostArrivalUntestedGivenInfectedPercentage()), "Uninfected" = c(PostArrivalTestPositiveGivenUninfectedPercentage(), PostArrivalTestNegativeGivenUninfectedPercentage(), PostArrivalUntestedGivenUninfectedPercentage(), PostArrivalTestPositiveGivenUninfectedPercentage() + PostArrivalTestNegativeGivenUninfectedPercentage() + PostArrivalUntestedGivenUninfectedPercentage()))))
  output$PostArrivalTestResultsGivenInfectionIntegerTable <- MyRenderDataTableInteger(reactive(data.frame("Test" = c("Positive", "Negative", "Untested", "Total"), "Infected" = c(PostArrivalTestPositiveGivenInfectedCount(), PostArrivalTestNegativeGivenInfectedCount(), PostArrivalUntestedGivenInfectedCount(), PostArrivalTestPositiveGivenInfectedCount() + PostArrivalTestNegativeGivenInfectedCount() + PostArrivalUntestedGivenInfectedCount()), "Uninfected" = c(PostArrivalTestPositiveGivenUninfectedCount(), PostArrivalTestNegativeGivenUninfectedCount(), PostArrivalUntestedGivenUninfectedCount(), PostArrivalTestPositiveGivenUninfectedCount() + PostArrivalTestNegativeGivenUninfectedCount() + PostArrivalUntestedGivenUninfectedCount()))))
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
      if (input$PreDepartureTestMethod == "None" & input$PostArrivalTestMethod == "None") {
        paste(
          "<ul>",
          "<li>Without testing, all <span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>) infected travelers may become imported cases.</li>",
          "</ul>",
          sep = ""
        )
        # If only a pre-departure test is performed
      } else if (input$PreDepartureTestMethod != "None" & input$PostArrivalTestMethod == "None") {
        paste(
          "<ul>",
          "<li>Pre-departure testing has decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format = "d", big.mark = ","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestResidualInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestResidualInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>).</li>",
          "<ul>",
          "<li>The residual prevalence is <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the disease prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
          "</ul>",
          "</ul>",
          sep = ""
        )
        # If only a post-arrival test is performed
      } else if (input$PreDepartureTestMethod == "None" & input$PostArrivalTestMethod != "None") {
        paste(
          "<ul>",
          "<li>Post-arrival testing has decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format = "d", big.mark = ","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestResidualInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestResidualInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>).</li>",
          "<ul>",
          "<li>The residual prevalence is <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
          "</ul>",
          "</ul>",
          sep = ""
        )
        # If both a pre-departure and post-arrival tests are performed
      } else if (input$PreDepartureTestMethod != "None" & input$PostArrivalTestMethod != "None") {
        paste(
          "<ul>",
          "<li>Pre-departure and post-arrival testing have jointly decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(PreDepartureTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepartureTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepartureTestPopulationCount(), format = "d", big.mark = ","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestResidualInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestResidualInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>).</li>",
          "<ul>",
          "<li>The residual prevalence is <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DestinationPrevalence(), "higher", "lower"), "</span> than the prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DestinationPrevalence(), 1), "</span>).</li>",
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
