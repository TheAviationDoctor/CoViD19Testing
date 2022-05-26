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
# 1 Client-side interface
# ==============================================================================

ui <- fluidPage(

  # ============================================================================
  # 1.1 Style and header
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
    # 1.2 Side panel
    # ==========================================================================

    sidebarPanel(

      # Header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-virus'></i>",
            "Disease assumptions"
          )
        )
      ),

      # Origin assumptions
      sliderInput(
        inputId  = "OriPrv",
        label    = "Prevalence at origin (%)",
        min      = 0L,
        max      = 100L,
        step     = .5,
        value    = 5L
      ),

      # Destination assumptions
      sliderInput(
        inputId  = "DesPrv",
        label    = "Prevalence at destination (%)",
        min      = 0L,
        max      = 100L,
        step     = .5,
        value    = 5L
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

      # Test sensitivity assumptions
      sliderInput(
        inputId  = "TstSen",
        label    = "Clinical sensitivity (%)",
        min      = 0L,
        max      = 100L,
        value    = 70L
      ),

      # Test specificity assumptions
      sliderInput(
        inputId  = "TstSpe",
        label    = "Clinical specificity (%)",
        min      = 0L,
        max      = 100L,
        value    = 95L
      ),

      icon(name = "", verify_fa = FALSE),

    ), # End of side panel

    # ==========================================================================
    # 1.3 Main panel
    # ==========================================================================

    mainPanel(

      # ========================================================================
      # 1.3.1 Pre-departure, pre-test probabilities
      # ========================================================================

      # Section header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-home'></i>",
            "1. Pre-departure, pre-test outcomes"
          )
        )
      ),
      em("Likelihood that a pre-departing traveler is infected."),
      br(),
      br(),

      tabsetPanel(

        # Summary tab
        tabPanel(
          "Summary",
          br(),
          uiOutput("PreDepOriSum")
        ),

        # Details tab
        tabPanel(
          "Details",
          br(),
          fluidRow(
            column(6L,
              div(
                align = "center",
                em("Disease prevalence at origin and destination")
              )
            ),
            column(6L,
              div(
                align = "center",
                em("Likelihood that a pre-departing air traveler is infected or uninfected")
              )
            )
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepAssPerTbl"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepInfPerTbl")))
          )
        ),

      ),

      hr(),
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-microscope'></i>",
            "2. Pre-departure test"
          )
        )
      ),
      em("100% of passengers are presumed tested."),
      hr(),

      # ==============================================================================
      # 1.3.2 Pre-departure, post-test probabilities
      # ==============================================================================

      # Section header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-plane-departure'></i>",
            "3. Pre-departure, post-test outcomes"
          )
        )
      ),
      em("Effectiveness of pre-departure testing."),
      br(),
      br(),

      tabsetPanel(

        # Summary tab
        tabPanel(
          "Summary",
          br(),
          uiOutput("PreDepTstResSum")
        ),

        # Details tab
        tabPanel(
          "Details",
          br(),
          fluidRow(
            column(6L,
              div(
                align = "center",
                em("Likelihood that a pre-departing air traveler is infected or uninfected")
              )
            ),
            column(6L,
            div(
              align = "center",
              em("Likelihood that a departing air traveler tests positive or negative")
              )
            )
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepInfPreTstTbl"))),
            column(6L, withSpinner(DT::dataTableOutput("PreDepTstResPerTbl")))
          ),
          br(),
          div(
            align = "center",
            em("Table 2.5. Likelihood that a departing air traveler tests positive/negative, given that they are infected/uninfected")
          ),
          fluidRow(
            column(6L, withSpinner(DT::dataTableOutput("PreDepTstResGivInfPerTbl"))),
          ),
          # br(),
          # div(
          #   align = "center",
          #   em("Table 2.6. Likelihood that a departing air traveler is infected/uninfected, given that they tested positive/negative")
          # ),
          # fluidRow(
          #   column(6L, withSpinner(DT::dataTableOutput("PreDepInfGivTstPerTbl"))),
          # ),
          # br(),
          # div(
          #   align = "center",
          #   em("Table 2.7. Likelihood that a tested departing air traveler is stopped or allowed to depart")
          # ),
          # fluidRow(
          #   column(6L, withSpinner(DT::dataTableOutput("PreDepTstNegTbl"))),
          # ),
          # br(),
          # div(
          #   align = "center",
          #   em("Table 2.8. Likelihood that a departing traveler is infected/uninfected after testing")
          # ),
          # fluidRow(
          #   column(6L, withSpinner(DT::dataTableOutput("PreDepTstResPerTbl"))),
          # )
       )
    #   ),
    #   hr(),
    # 
    #   # ==============================================================================
    #   # 1.3.3 Post-arrival, pre-test probabilities
    #   # ==============================================================================
    # 
    # # Section header
    # tags$h4(
    #   HTML(
    #     paste(
    #       "<i class='fa fa-plane-arrival'></i>",
    #       "Post-arrival, pre-test outcomes"
    #     )
    #   )
    # ),
    # em("Effectiveness of post-arrival testing."),
    # br(),
    # br(),
    # 
    # tabsetPanel(
    # 
    #     # Summary panel
    #     tabPanel(
    #       "Summary",
    #       br(),
    #       uiOutput("PstArrTstResSum")
    #     ),
    # 
    #     # Details panel
    #     tabPanel(
    #       "Details",
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.1. Arriving population and test assumptions")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalStartingAssumptionsPercentageTable"))),
    #       ),
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.2. Likelihood that an arriving traveler is infected/uninfected before testing")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalPopulationPercentageTable"))),
    #       ),
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.3. Likelihood that an arriving traveler gets tested")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalSamplePercentageTable"))),
    #       ),
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.4. Likelihood that an arriving air traveler tests positive/negative")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResultsPercentageTable"))),
    #       ),
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.5. Likelihood that an arriving air traveler tests positive/negative, given that they are infected/uninfected")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResultsGivenInfectionPercentageTable"))),
    #       ),
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.6. Likelihood that an arriving air traveler is infected/uninfected, given that they tested positive/negative")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalInfectionGivenTestResultsPercentageTable"))),
    #       ),
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.7. Likelihood that a tested arriving air traveler is stopped or allowed to enter the destination")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestClearedPercentageTable"))),
    #       ),
    #       br(),
    #       div(
    #         align = "center",
    #         em("Table 3.8. Likelihood that an arriving traveler is infected/uninfected after testing")
    #       ),
    #       fluidRow(
    #         column(6L, withSpinner(DT::dataTableOutput("PostArrivalTestResidualPercentageTable"))),
    #       )
    #     )
    #   ),
    #   hr(),
    # 
    #   # ==============================================================================
    #   # 1.3.4 Post-arrival, post-test probabilities
    #   # ==============================================================================
    # 
    #   # Section header
    #   tags$h4(
    #     HTML(
    #       paste(
    #         "<i class='fa fa-hotel'></i>",
    #         "Post-arrival, post-test outcomes"
    #       )
    #     )
    #   ),
    #   em("Residual risk of case importation."),
    #   br(),
    #   br(),
    # 
    #   tabsetPanel(
    # 
    #     # Summary panel
    #     tabPanel(
    #       "Summary",
    #       br(),
    #       uiOutput("PostArrivalDestinationSummary")
    #     )
     )
    )
  )
)

# ==============================================================================
# 2 Server-side logic
# ==============================================================================

server <- function(input, output) {

  # ============================================================================
  # 2.0 Helper functions
  # ============================================================================

  MyRenderDataTablePercentage <- function(DataFrame) {
    DT::renderDataTable({
      datatable(
        DataFrame(),
        rownames = NULL,
        options = list(dom = "t", ordering = FALSE, paging = FALSE)
      ) %>%
        formatPercentage(columns = 2L:length(DataFrame()), digits = 1L) %>%
        formatStyle(columns = 2L:length(DataFrame()), color = "#1E32FA")
    })
  }

  # ============================================================================
  # 2.1 Variables definition
  # ============================================================================

  # Origin prevalence
  OriPrv <- reactive({input$OriPrv / 100})

  # Destination prevalence
  DesPrv <- reactive({input$DesPrv / 100})

  # Test sensitivity
  TstSen <- reactive({input$TstSen / 100})

  # Test specificity
  TstSpe <- reactive({input$TstSpe / 100})

  # Pre-departure probability of a positive test
  PreDepTstPos <- reactive({
    TstSen() * OriPrv() + (1 - TstSpe()) * (1 - OriPrv())
  })

  # Pre-departure probability of a negative test
  PreDepTstNeg <- reactive({
    (1 - TstSen()) * OriPrv() + TstSpe() * (1 - OriPrv())
  })

  # Pre-departure probability of being infected given a positive test
  PreDepTstInfGivPos <- reactive({
    TstSen() * OriPrv() / (TstSen() * OriPrv() + (1 - TstSpe()) * (1 - OriPrv()))
  })

  # Pre-departure probability of being infected given a negative test
  PreDepTstInfGivNeg <- reactive({
    (1 - TstSen()) * OriPrv() / ((1 - TstSen()) * OriPrv() + TstSpe() * (1 - OriPrv()))
  })

  # Pre-departure probability of being uninfected given a positive test
  PreDepTstUniGivPos <- reactive({
    (1 - TstSpe()) * (1 - OriPrv()) / (TstSen() * OriPrv() + (1 - TstSpe()) * (1 - OriPrv()))
  })

  # Pre-departure probability of being uninfected given a negative test
  PreDepTstUniGivNeg <- reactive({
    TstSpe() * (1 - OriPrv()) / ((1 - TstSen()) * OriPrv() + TstSpe() * (1 - OriPrv()))
  })

  # ============================================================================
  # 2.2 Pre-departure, pre-test outcomes
  # ============================================================================

  # Summary tab
  output$PreDepOriSum <- renderUI({
    HTML(
      paste(
        "<ul><li><span style=color:#1E32FA>",
        formattable::percent(OriPrv(), 1),
        "</span> of departing travelers are presumed infected.</li>",
        "<li>This is <span style=color:#1E32FA>",
        ifelse(OriPrv() > DesPrv(), "higher", "lower"),
        "</span> than the disease prevalence at destination (<span style=color:#1E32FA>",
        formattable::percent(DesPrv(), 1), "</span>).</li></ul>",
        sep = ""
      )
    )
  })

  # Details tab
  output$PreDepAssPerTbl <- MyRenderDataTablePercentage(
    reactive(
      data.frame(
        "Prevalence" = c("At origin", "At destination"),
        "Value"      = c(OriPrv(), DesPrv())
      )
    )
  )
  output$PreDepInfPerTbl <- MyRenderDataTablePercentage(
    reactive(
      data.frame(
        "Status"     = c("Infected", "Uninfected"),
        "Value"      = c(OriPrv(), 1L - OriPrv())
      )
    )
  )
  output$PreDepPrvPerTbl <- MyRenderDataTablePercentage(
    reactive(
      IncidenceTable() %>%
      filter(Date == max(Date)) %>%
      select(Country, PointPrevalence, MovingAveragePrevalence) %>%
      rename(
        "Latest"         = "PointPrevalence",
        "Moving average" = "MovingAveragePrevalence"
      )
    )
  )

  # ============================================================================
  # 2.3 Pre-departure, post-test outcomes
  # ============================================================================
  
  # Summary tab
  output$PreDepTstResSum <- renderUI({
    HTML(
      paste(
        "<ul>",
        "<li>A <span style=color:#1E32FA>pre-departure</span> test is performed.</li>",
        "</ul>",
        "<ul>",
        "<li><span style=color:#1E32FA>", formattable::percent(OriPrv(), 1),
        "</span> of departing travelers are presumed infected, of which:</li>",
        "<ul>",
        "<li><span style=color:#1E32FA>", formattable::percent(TstSen() * OriPrv(), 1), "</span> test positive (true positives) and are correctly prevented from boarding.</li>",
        "<li><span style=color:#1E32FA>", formattable::percent((1 - TstSen()) * OriPrv(), 1), "</span> test negative (false negatives) and are incorrectly allowed to board.</li>",
        "</ul>",
        "<br>",
        "<li><span style=color:#1E32FA>", formattable::percent(1L - OriPrv(), 1), "</span> of departing travelers are presumed uninfected, of which:</li>",
        "<ul>",
        "<li><span style=color:#1E32FA>", formattable::percent(TstSpe() * (1L - OriPrv()), 1), "</span> test negative (true negatives) and are correctly allowed to board.</li>",
        "<li><span style=color:#1E32FA>", formattable::percent((1 - TstSpe()) * (1L - OriPrv()), 1), "</span> test positive (false positives) and are correctly allowed to board after a retest.</li>",
        "</ul>",
        "<br>",
        "<li>The likelihood that a traveler carries the disease given a test result is:</li>",
        "<ul>",
        "<li><span style=color:#1E32FA>", formattable::percent(PreDepTstInfGivPos(), 1), "</span> chance of being infected given a positive test (positive predictive value).</li>",
        "<li><span style=color:#1E32FA>", formattable::percent(PreDepTstUniGivNeg(), 1), "</span> chance of being uninfected given a negative test (negative predictive value).</li>",
        "</ul>",
        "</ul>",
        sep = ""
      )
    )
  })

  # Details tab
  output$PreDepInfPreTstTbl <- MyRenderDataTablePercentage(
    reactive(
      data.frame(
        "Status" = c("Infected", "Uninfected"),
        "Value"  = c(OriPrv(), 1L - OriPrv())
      )
    )
  )
  output$PreDepTstResPerTbl <- MyRenderDataTablePercentage(
    reactive(
      data.frame(
        "Test"  = c("Positive", "Negative"),
        "Value" = c((TstSen() * OriPrv()) + (1L - TstSpe()) * (1L - OriPrv()), (1L - TstSen()) * OriPrv() + TstSpe() * (1L - OriPrv()))
      )
    )
  )
  output$PreDepTstResGivInfPerTbl <- MyRenderDataTablePercentage(
    reactive(
      data.frame(
        "Test"  = c("Positive", "Negative"),
        "Infected" = c(TstSen() * OriPrv(), (1 - TstSen()) * OriPrv()),
        "Uninfected" = c((1 - TstSpe()) * (1L - OriPrv()), TstSpe() * (1L - OriPrv()))
      )
    )
  )
  # output$PreDepTstResGivInfPerTbl <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Positive", "Negative", "Untested", "Total"), "Infected" = c(TstSen(), (1 - TstSen())(), PreDepartureUntestedGivenInfectedPercentage(), TstSen() + (1 - TstSen())() + PreDepartureUntestedGivenInfectedPercentage()), "Uninfected" = c((1 - TstSpe())(), TstSpe()(), PreDepartureUntestedGivenUninfectedPercentage(), (1 - TstSpe())() + TstSpe()() + PreDepartureUntestedGivenUninfectedPercentage()))))
  # output$PreDepInfGivTstPerTbl    <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Infected", "Uninfected", "Total"), "Positive" = c(PreDepTstInfGivPos(), PreDepTstUniGivPos(), PreDepTstInfGivPos() + PreDepTstUniGivPos()), "Negative" = c(PreDepTstInfGivNeg(), PreDepTstUniGivNeg(), PreDepTstInfGivNeg() + PreDepTstUniGivNeg()))))
  # output$PreDepTstNegTbl          <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Stopped" = PreDepTstPos(), "Allowed" = PreDepTstNeg())))
  # output$PreDepTstResPerTbl       <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Infected" = PreDepartureTestResidualInfectedPercentage(), "Uninfected" = PreDepartureTestResidualUninfectedPercentage())))
# 
#   # ============================================================================
#   # 2.4 Post-arrival, pre-test outcomes
#   # ============================================================================
#   
#   # Summary tab
#   output$PstArrTstResSum <- renderUI({
#     HTML(
#       paste(
#         "<ul>",
#         "<li>A <span style=color:#1E32FA>post-arrival</span> test is performed.</li>",
#         "</ul>",
#         "<ul>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>) of arriving travelers are presumed infected, of which:</li>",
#         "<ul>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenInfectedCount(), format = "d", big.mark = ","), "</span>) test positive (true positives) and are correctly directed to isolate (presumably).</li>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenInfectedCount(), format = "d", big.mark = ","), "</span>) test negative (false negatives) and are incorrectly allowed to enter the destination.</li>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalUntestedGivenInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalUntestedGivenInfectedCount(), format = "d", big.mark = ","), "</span>) are untested and incorrectly allowed to enter the destination.</li>",
#         "</ul>",
#         "<br>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPopulationUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPopulationUninfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>) of arriving travelers are presumed uninfected, of which:</li>",
#         "<ul>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestNegativeGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestNegativeGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) test negative (true negatives) and are correctly allowed enter the destination.</li>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestPositiveGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestPositiveGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) test positive (false positives) and are correctly allowed enter the destination after a retest (presumably).</li>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalUntestedGivenUninfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalUntestedGivenUninfectedCount(), format = "d", big.mark = ","), "</span>) are untested and correctly allowed to enter the destination.</li>",
#         "</ul>",
#         "<br>",
#         "<li>The likelihood that a traveler carries the disease given a test result is:</li>",
#         "<ul>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestInfectedGivenPositivePercentage(), 1), "</span> chance of being infected given a positive test (positive predictive value).</li>",
#         "<li><span style=color:#1E32FA>", formattable::percent(PostArrivalTestUninfectedGivenNegativePercentage(), 1), "</span> chance of being uninfected given a negative test (negative predictive value).</li>",
#         "</ul>",
#         "</ul>",
#         sep = ""
#       ),
#     )
#   })
# 
#   # Details panel
#   output$PostArrivalStartingAssumptionsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = c("Tested travelers (%)", "Clinical sensitivity (%)", "Clinical specificity (%)"), "Value" = c(PostArrivalSampleTestedPercentage(), PostArrivalTestSensitivity(), PostArrivalTestSpecificity()))))
#   output$PostArrivalPopulationPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Assumption" = "Percent", "Infected" = PostArrivalTestPopulationInfectedPercentage(), "Uninfected" = PostArrivalTestPopulationUninfectedPercentage())))
#   output$PostArrivalSamplePercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Tested" = PostArrivalSampleTestedPercentage(), "Untested" = PostArrivalSampleUntestedPercentage())))
#   output$PostArrivalTestResultsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Positive" = PostArrivalTestPositivePercentage(), "Negative" = PostArrivalTestNegativePercentage())))
#   output$PostArrivalTestResultsGivenInfectionPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Positive", "Negative", "Untested", "Total"), "Infected" = c(PostArrivalTestPositiveGivenInfectedPercentage(), PostArrivalTestNegativeGivenInfectedPercentage(), PostArrivalUntestedGivenInfectedPercentage(), PostArrivalTestPositiveGivenInfectedPercentage() + PostArrivalTestNegativeGivenInfectedPercentage() + PostArrivalUntestedGivenInfectedPercentage()), "Uninfected" = c(PostArrivalTestPositiveGivenUninfectedPercentage(), PostArrivalTestNegativeGivenUninfectedPercentage(), PostArrivalUntestedGivenUninfectedPercentage(), PostArrivalTestPositiveGivenUninfectedPercentage() + PostArrivalTestNegativeGivenUninfectedPercentage() + PostArrivalUntestedGivenUninfectedPercentage()))))
#   output$PostArrivalInfectionGivenTestResultsPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Test" = c("Infected", "Uninfected", "Total"), "Positive" = c(PostArrivalTestInfectedGivenPositivePercentage(), PostArrivalTestUninfectedGivenPositivePercentage(), PostArrivalTestInfectedGivenPositivePercentage() + PostArrivalTestUninfectedGivenPositivePercentage()), "Negative" = c(PostArrivalTestInfectedGivenNegativePercentage(), PostArrivalTestUninfectedGivenNegativePercentage(), PostArrivalTestInfectedGivenNegativePercentage() + PostArrivalTestUninfectedGivenNegativePercentage()))))
#   output$PostArrivalTestClearedPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Stopped" = PostArrivalTestStoppedPercentage(), "Allowed" = PostArrivalTestClearedPercentage())))
#   output$PostArrivalTestResidualPercentageTable <- MyRenderDataTablePercentage(reactive(data.frame("Result" = "Percent", "Infected" = PostArrivalTestResidualInfectedPercentage(), "Uninfected" = PostArrivalTestResidualUninfectedPercentage())))
# 
#   # ============================================================================
#   # 2.5 Post-arrival, post-test outcomes
#   # ============================================================================
#   
#   # Summary tab
#   output$PostArrivalDestinationSummary <- renderUI({
#     HTML(
#       paste(
#         "<ul>",
#         "<li>Pre-departure and post-arrival testing have jointly decreased the importation risk from <span style=color:#1E32FA>", formattable::percent(OriPrv(), 1), "</span> (<span style=color:#1E32FA>", formatC(PreDepTstPopInfCnt(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PreDepTstPopCnt(), format = "d", big.mark = ","), "</span>) to <span style=color:#1E32FA>", formattable::percent(PostArrivalTestResidualInfectedPercentage(), 1), "</span> (<span style=color:#1E32FA>", formatC(PostArrivalTestResidualInfectedCount(), format = "d", big.mark = ","), "</span> out of <span style=color:#1E32FA>", formatC(PostArrivalTestPopulationCount(), format = "d", big.mark = ","), "</span>).</li>",
#         "<ul>",
#         "<li>The residual prevalence is <span style=color:#1E32FA>", ifelse(PostArrivalTestResidualInfectedPercentage() > DesPrv(), "higher", "lower"), "</span> than the prevalence at destination (<span style=color:#1E32FA>", formattable::percent(DesPrv(), 1), "</span>).</li>",
#         "</ul>",
#         "</ul>",
#         sep = ""
#       )
#     )
#   })
}

# ============================================================================
# 3. App
# ============================================================================

shinyApp(ui = ui, server = server)