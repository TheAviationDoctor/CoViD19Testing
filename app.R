###############################################################################
# HOUSEKEEPING CODE                                                           #
###############################################################################

# Load libraries
library(shiny)

# Clear the console
cat("\014")

###############################################################################
# VARIABLE DECLARATION                                                        #
###############################################################################

#origins <- list("AF","AX","AL","DZ","AS","AD","AO","AI","AG","AR","AM","AW","AU","AT","AZ","BS","BH","BD","BB","BY","BE","BZ","BJ","BM","BT","BO","BQ","BA","BW","BV","BR","IO","BN","BG","BF","BI","CV","KH","CM","CA","KY","CF","TD","CL","CN","CX","CC","CO","KM","CG","CD","CK","CR","CI","HR","CU","CW","CY","CZ","DK","DJ","DM","DO","EC","EG","SV","GQ","ER","EE","ET","FK","FO","FJ","FI","FR","GF","PF","TF","GA","GM","GE","DE","GH","GI","GR","GL","GD","GP","GU","GT","GG","GN","GW","GY","HT","HM","VA","HN","HK","HU","IS","IN","ID","IR","IQ","IE","IM","IL","IT","JM","JP","JE","JO","KZ","KE","KI","KP","KR","XK","KW","KG","LA","LV","LB","LS","LR","LY","LI","LT","LU","MO","MK","MG","MW","MY","MV","ML","MT","MH","MQ","MR","MU","YT","MX","FM","MD","MC","MN","ME","MS","MA","MZ","MM","NA","NR","NP","NL","NC","NZ","NI","NE","NG","NU","NF","MP","NO","OM","PK","PW","PS","PA","PG","PY","PE","PH","PN","PL","PT","PR","QA","RE","RO","RU","RW","BL","SH","KN","LC","MF","PM","VC","WS","SM","ST","SA","SN","RS","SC","SL","SG","SX","SK","SI","SB","SO","ZA","GS","SS","ES","LK","SD","SR","SJ","SZ","SE","CH","SY","TW","TJ","TZ","TH","TL","TG","TK","TO","TT","TN","TR","TM","TC","TV","UG","UA","AE","GB","UM","US","UY","UZ","VU","VE","VN","VG","VI","WF","EH","YE","ZM","ZW")

###############################################################################
# APPLICATION CODE                                                            #
###############################################################################

if (interactive()) {
  options(device.ask.default = FALSE)
  
  #############################################################################
  # USER INTERFACE LOGIC                                                      #
  #############################################################################
  
  ui <- fluidPage(
    
    # App title
    titlePanel("SARS-CoV-2 Air Travel Screening Simulator"),
    
    # Layout with input and output definitions
    sidebarLayout(
      
      # Sidebar panel for selecting inputs
      sidebarPanel(
        selectInput(inputId = "CountryOrigin", label = "Country of origin", choices = origins), 
        sliderInput(inputId = 'SampleSize', label = 'Sample size', min=0, max=2200000000000, value=2200000000000),
        sliderInput(inputId = 'SamplePrevalence', label = 'Sample prevalence', min=0, max=1, value=.1),
        sliderInput(inputId = 'TestSensitivity', label = 'Test sensitivity', min=0, max=1, value=.7),
        sliderInput(inputId = 'TestSpecificity', label = 'Test specificity', min=0, max=1, value=.95)
      ),
      # Main panel for displaying outputs
      mainPanel(
        ls(all.names = TRUE),
        textOutput("ProbabilityPositiveGivenInfected")
      )
    )
  )

  #############################################################################
  # SERVER LOGIC                                                              #
  #############################################################################
  
  server <- function(input, output) {
    
    output$ProbabilityPositiveGivenInfected <- renderText({ input$TestSensitivity })
    # output$ProbabilityPositiveGivenNotInfected <- renderText({ 1- input$TestSpecificity })
    # output$ProbabilityNegativeGivenInfected <- renderText({ 1 - input$TestSensitivity })
    # output$ProbabilityNegativeGivenNotInfected <- renderText({ input$TestSpecitificity })
    
  }

  #############################################################################
  # RUN APPLICATION                                                           #
  #############################################################################
  
  shinyApp(ui, server)
}