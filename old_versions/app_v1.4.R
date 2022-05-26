# ==============================================================================
#    NAME: app.R
# ACTIONS: Creates a Shiny interactive dashboard to measure the residual risk of
#          COVID-19 case importation from origin to destination based on testing
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Clear the console
cat("\014")

library(DT)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)

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
      column(6L, h2("COVID-19 case importation risk assessment tool")),
      column(
        6L,
        tags$img(
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
        inputId  = "OriginPrevalence",
        label    = "Prevalence at origin (%)",
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
        inputId  = "TestSensitivity",
        label    = "Clinical sensitivity (%)",
        min      = 0L,
        max      = 100L,
        value    = 80L
      ),

      # Test specificity assumptions
      sliderInput(
        inputId  = "TestSpecificity",
        label    = "Clinical specificity (%)",
        min      = 0L,
        max      = 100L,
        value    = 98L
      ),

      icon(name = "", verify_fa = FALSE),

    ), # End of sidePanel

    # ==========================================================================
    # 1.3 Main panel
    # ==========================================================================

    mainPanel(

      # ========================================================================
      # 1.3.1 At origin
      # ========================================================================

      # Header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-home'></i>",
            "At origin"
          )
        )
      ),

      # Labels
      fluidRow(
        column(6L,
          div(
            align = "center",
            em("Likelihood that any pre-departing pax is infected or uninfected")
          )
        )
      ),

      # Tables
      fluidRow(
        column(6L, withSpinner(dataTableOutput("OriginTable1")))
      ),

      hr(),

      # ========================================================================
      # 1.3.2 Pre-departure test
      # ========================================================================

      # Header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-microscope'></i>",
            "Pre-departure test"
          )
        )
      ),

      # Labels
      fluidRow(
        column(6L,
          div(
            align = "center",
            em("Likelihood that a pre-departing pax tests positive or negative given their epidemiological status")
          )
        ),
        column(6L,
          div(
            align = "center",
            em("Likelihood that a pre-departing pax is infected or not given their test result")
          )
        )
      ),

      # Tables
      fluidRow(
        column(6L, withSpinner(dataTableOutput("PreTestTable1"))),
        column(6L, withSpinner(dataTableOutput("PreTestTable2")))
      ),

      hr(),

      # ==============================================================================
      # 1.3.3 At boarding
      # ==============================================================================

      # Header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-plane-departure'></i>",
            "At boarding"
          )
        )
      ),

      # Labels
      fluidRow(
        column(6L,
          div(
            align = "center",
            em("Pax boarding and not boarding by epidemiological status")
          )
        ),
        column(6L,
          div(
            align = "center",
            em("New percentage of pax on board by status")
          )
        )
      ),

      # Tables
      fluidRow(
        column(6L, withSpinner(dataTableOutput("BoardingTable1"))),
        column(6L, withSpinner(dataTableOutput("BoardingTable2")))
      ),

      hr(),

      # ==============================================================================
      # 1.3.4 Post-arrival test
      # ==============================================================================

      # Header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-plane-arrival'></i>",
            "Post-arrival test"
          )
        )
      ),

      # Labels
      fluidRow(
        column(6L,
          div(
            align = "center",
            em("Likelihood that an arriving pax tests positive or negative given their epidemiological status")
          )
        ),
        column(6L,
          div(
            align = "center",
            em("Likelihood that an arriving pax is infected or not given their test result")
          )
        )
      ),

      # Tables
      fluidRow(
        column(6L, withSpinner(dataTableOutput("PostTestTable1"))),
        column(6L, withSpinner(dataTableOutput("PostTestTable2")))
      ),

      hr(),

      # ==============================================================================
      # 1.3.5 At destination
      # ==============================================================================

      # Header
      tags$h4(
        HTML(
          paste(
            "<i class='fa fa-hotel'></i>",
            "At destination"
          )
        )
      ),

      # Labels
      fluidRow(
        column(6L,
          div(
            align = "center",
            em("Pax being granted/denied entry into the destination country by epidemiological status")
          )
        ),
        column(6L,
          div(
            align = "center",
            em("New percentage of pax entering the destination country by epidemiological status")
          )
        )
      ),

      # Tables
      fluidRow(
        column(6L, withSpinner(dataTableOutput("EnteringTable1"))),
        column(6L, withSpinner(dataTableOutput("EnteringTable2")))
      ),
      br()

    ) # End of main panel
  ) # End of sidebarLayout
) # End of fluidPage

# ==============================================================================
# 2 Server-side logic
# ==============================================================================

server <- function(input, output) {

  # ============================================================================
  # 2.0 Set helper functions
  # ============================================================================

  MyRenderDataTable <- function(DataFrame) {
    renderDataTable({
      datatable(
        DataFrame(),
        rownames = NULL,
        options = list(dom = "t", ordering = FALSE, paging = FALSE)
      ) |>
        formatPercentage(columns = 2L:length(DataFrame()), digits = 1L) |>
        formatStyle(columns = 2L:length(DataFrame()), color = "#1E32FA")
    })
  }

  # ============================================================================
  # 2.1 Define variables
  # ============================================================================

  # Assumptions
  OriginPrevalence                <- reactive({input$OriginPrevalence / 100L})
  TestSensitivity                 <- reactive({input$TestSensitivity  / 100L})
  TestSpecificity                 <- reactive({input$TestSpecificity  / 100L})

  # At origin
  OriginInfected                  <- reactive({OriginPrevalence()})
  OriginUninfected                <- reactive({1L - OriginInfected()})

  # Pre-departure test
  PreTestPositiveGivenInfected    <- reactive({TestSensitivity()})
  PreTestPositiveGivenUninfected  <- reactive({1L - TestSpecificity()})
  PreTestNegativeGivenInfected    <- reactive({1L - TestSensitivity()})
  PreTestNegativeGivenUninfected  <- reactive({TestSpecificity()})
  PreTestPositive                 <- reactive({PreTestPositiveGivenInfected()    * OriginInfected()   + PreTestPositiveGivenUninfected() * OriginUninfected()})
  PreTestNegative                 <- reactive({PreTestNegativeGivenInfected()    * OriginInfected()   + PreTestNegativeGivenUninfected() * OriginUninfected()})
  PreTestInfectedGivenPositive    <- reactive({PreTestPositiveGivenInfected()    * OriginInfected()   / PreTestPositive()})
  PreTestInfectedGivenNegative    <- reactive({PreTestNegativeGivenInfected()    * OriginInfected()   / PreTestNegative()})
  PreTestUninfectedGivenPositive  <- reactive({PreTestPositiveGivenUninfected()  * OriginUninfected() / PreTestPositive()})
  PreTestUninfectedGivenNegative  <- reactive({PreTestNegativeGivenUninfected()  * OriginUninfected() / PreTestNegative()})

  # At boarding
  BoardingAcceptedGivenInfected   <- reactive({PreTestNegativeGivenInfected()    * OriginInfected()})
  BoardingAcceptedGivenUninfected <- reactive({PreTestNegativeGivenUninfected()  * OriginUninfected()})
  BoardingAcceptedTotal           <- reactive({BoardingAcceptedGivenInfected()   + BoardingAcceptedGivenUninfected()})
  BoardingDeniedGivenInfected     <- reactive({PreTestPositiveGivenInfected()    * OriginInfected()})
  BoardingDeniedGivenUninfected   <- reactive({PreTestPositiveGivenUninfected()  * OriginUninfected()})
  BoardingDeniedTotal             <- reactive({BoardingDeniedGivenInfected()     + BoardingDeniedGivenUninfected()})
  BoardingInfected                <- reactive({BoardingAcceptedGivenInfected()   / BoardingAcceptedTotal()})
  BoardingUninfected              <- reactive({BoardingAcceptedGivenUninfected() / BoardingAcceptedTotal()})

  # Post-arrival test
  PostTestPositiveGivenInfected   <- reactive({TestSensitivity()})
  PostTestPositiveGivenUninfected <- reactive({1L - TestSpecificity()})
  PostTestNegativeGivenInfected   <- reactive({1L - TestSensitivity()})
  PostTestNegativeGivenUninfected <- reactive({TestSpecificity()})
  PostTestPositive                <- reactive({PostTestPositiveGivenInfected()   * BoardingInfected()   + PostTestPositiveGivenUninfected() * BoardingUninfected()})
  PostTestNegative                <- reactive({PostTestNegativeGivenInfected()   * BoardingInfected()   + PostTestNegativeGivenUninfected() * BoardingUninfected()})
  PostTestInfectedGivenPositive   <- reactive({PostTestPositiveGivenInfected()   * BoardingInfected()   / PostTestPositive()})
  PostTestInfectedGivenNegative   <- reactive({PostTestNegativeGivenInfected()   * BoardingInfected()   / PostTestNegative()})
  PostTestUninfectedGivenPositive <- reactive({PostTestPositiveGivenUninfected() * BoardingUninfected() / PostTestPositive()})
  PostTestUninfectedGivenNegative <- reactive({PostTestNegativeGivenUninfected() * BoardingUninfected() / PostTestNegative()})

  # At destination
  EnteringAcceptedGivenInfected   <- reactive({PostTestNegativeGivenInfected()   * BoardingInfected()})
  EnteringAcceptedGivenUninfected <- reactive({PostTestNegativeGivenUninfected() * BoardingUninfected()})
  EnteringAcceptedTotal           <- reactive({EnteringAcceptedGivenInfected()   + EnteringAcceptedGivenUninfected()})
  EnteringDeniedGivenInfected     <- reactive({PostTestPositiveGivenInfected()   * BoardingInfected()})
  EnteringDeniedGivenUninfected   <- reactive({PostTestPositiveGivenUninfected() * BoardingUninfected()})
  EnteringDeniedTotal             <- reactive({EnteringDeniedGivenInfected()     + EnteringDeniedGivenUninfected()})
  EnteringInfected                <- reactive({EnteringAcceptedGivenInfected()   / EnteringAcceptedTotal()})
  EnteringUninfected              <- reactive({EnteringAcceptedGivenUninfected() / EnteringAcceptedTotal()})

  # ============================================================================
  # 2.2 Assemble tables
  # ============================================================================

  output$OriginTable1 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Status" = c("Infected", "Uninfected", "Total"),
        "Value"  = c(OriginInfected(), OriginUninfected(), OriginInfected() + OriginUninfected())
      )
    )
  )

  output$PreTestTable1 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Test"       = c("Positive", "Negative", "Total"),
        "Infected"   = c(PreTestPositiveGivenInfected(), PreTestNegativeGivenInfected(), PreTestPositiveGivenInfected() + PreTestNegativeGivenInfected()),
        "Uninfected" = c(PreTestPositiveGivenUninfected(), PreTestNegativeGivenUninfected(), PreTestPositiveGivenUninfected() + PreTestNegativeGivenUninfected()),
        "Either"     = c(PreTestPositive(), PreTestNegative(), PreTestPositive() + PreTestNegative())
      )
    )
  )

  output$PreTestTable2 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Test"     = c("Infected", "Uninfected", "Total"),
        "Positive" = c(PreTestInfectedGivenPositive(), PreTestUninfectedGivenPositive(), PreTestInfectedGivenPositive() + PreTestUninfectedGivenPositive()),
        "Negative" = c(PreTestInfectedGivenNegative(), PreTestUninfectedGivenNegative(), PreTestInfectedGivenNegative() + PreTestUninfectedGivenNegative()),
        "Either"   = c(OriginInfected(), OriginUninfected(), OriginInfected() + OriginUninfected())
      )
    )
  )

  output$BoardingTable1 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Test"       = c("Boarding accepted", "Boarding denied", "Total"),
        "Infected"   = c(BoardingAcceptedGivenInfected(), BoardingDeniedGivenInfected(), BoardingAcceptedGivenInfected() + BoardingDeniedGivenInfected()),
        "Uninfected" = c(BoardingAcceptedGivenUninfected(), BoardingDeniedGivenUninfected(), BoardingAcceptedGivenUninfected() + BoardingDeniedGivenUninfected()),
        "Either"     = c(BoardingAcceptedTotal(), BoardingDeniedTotal(), BoardingAcceptedTotal() + BoardingDeniedTotal())
      )
    )
  )

  output$BoardingTable2 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Status"          = c("Infected", "Uninfected", "Total"),
        "Without testing" = c(OriginInfected(), OriginUninfected(), OriginInfected() + OriginUninfected()),
        "With testing"    = c(BoardingInfected(), BoardingUninfected(), BoardingInfected() + BoardingUninfected())
      )
    )
  )

  output$PostTestTable1 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Test"       = c("Positive", "Negative", "Total"),
        "Infected"   = c(PostTestPositiveGivenInfected(), PostTestNegativeGivenInfected(), PostTestPositiveGivenInfected() + PostTestNegativeGivenInfected()),
        "Uninfected" = c(PostTestPositiveGivenUninfected(), PostTestNegativeGivenUninfected(), PostTestPositiveGivenUninfected() + PostTestNegativeGivenUninfected()),
        "Either"     = c(PostTestPositive(), PostTestNegative(), PostTestPositive() + PostTestNegative())
      )
    )
  )

  output$PostTestTable2 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Test"     = c("Infected", "Uninfected", "Total"),
        "Positive" = c(PostTestInfectedGivenPositive(), PostTestUninfectedGivenPositive(), PostTestInfectedGivenPositive() + PostTestUninfectedGivenPositive()),
        "Negative" = c(PostTestInfectedGivenNegative(), PostTestUninfectedGivenNegative(), PostTestInfectedGivenNegative() + PostTestUninfectedGivenNegative()),
        "Either"   = c(BoardingInfected(), BoardingUninfected(), BoardingInfected() + BoardingUninfected())
      )
    )
  )

  output$EnteringTable1 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Test"       = c("Entry granted", "Entry denied", "Total"),
        "Infected"   = c(EnteringAcceptedGivenInfected(), EnteringDeniedGivenInfected(), EnteringAcceptedGivenInfected() + EnteringDeniedGivenInfected()),
        "Uninfected" = c(EnteringAcceptedGivenUninfected(), EnteringDeniedGivenUninfected(), EnteringAcceptedGivenUninfected() + EnteringDeniedGivenUninfected()),
        "Either"     = c(EnteringAcceptedTotal(), EnteringDeniedTotal(), EnteringAcceptedTotal() + EnteringDeniedTotal())
      )
    )
  )

  output$EnteringTable2 <- MyRenderDataTable(
    reactive(
      data.frame(
        "Status"          = c("Infected", "Uninfected", "Total"),
        "Without testing" = c(BoardingInfected(), BoardingUninfected(), BoardingInfected() + BoardingUninfected()),
        "With testing"    = c(EnteringInfected(), EnteringUninfected(), EnteringInfected() + EnteringUninfected())
      )
    )
  )

}

# ============================================================================
# 3. App
# ============================================================================

shinyApp(ui = ui, server = server)