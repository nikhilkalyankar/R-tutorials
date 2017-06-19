library(shiny)
library(leaflet)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Analysis of New York State's Energy Star® Program"),
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Intro",
      br(),
      p(strong(
        "Source: ",
        a(href = "https://data.ny.gov/api/views/assk-vu73/rows.json?accessType=DOWNLOAD", "New York Government Data")
      )),
      p(
        em(
          "The One- to Four-Family Home Performance with ENERGY STAR® Program is a market transformation program that uses Building Performance Institute (BPI) Goldstar contractors to install comprehensive energy-efficient improvements.  The program is designed to use building science and a whole-house approach to reduce energy use in the New York State’s existing one-to-four family and low-rise multifamily residential buildings and capture heating fuel and electricity-related savings.  The Program provides incentives on eligible energy efficiency improvements including building shell measures, high efficiency heating and cooling measures, ENERGY STAR appliances and lighting."
        )
      ),
      
      p(
        em("Statewide Home Performance with ENERGY STAR Program dataset includes the following data points for projects completed during Green Jobs Green-NY, beginning November 15, 2010 through March 31, 2016: Home Performance Project ID, Home Performance Site ID, Project County, Project City, Project Zip, Gas Utility, Electric Utility, Project Completion, Date, Customer Type, Low-Rise or Home Performance Indicator, Total Project Cost (USD), Total Incentives (USD), Type of Program Financing, Amount Financed Through Program (USD), Pre-Retrofit Home Heating Fuel Type, Estimated Annual kWh Savings, Estimated Annual MMBtu Savings, First Year Energy Savings $ Estimate (USD), Homeowner Received Green Jobs-Green NY Free/Reduced Cost Audit (Y/N)"
      )),
      hr(),
      h3("Let's begin"),
      p(
        strong("This project is divided in to four phases*: "),
        tags$ol(
          tags$li("Data Preview"),
          tags$li("Exploratory Graphs #"),
          tags$li("Linear Regression Analysis and related graphs #"),
          tags$li("Geospatial Overview #")
        )
      ),
      p(
        strong("* Graphs, map and tables may have loading delays"),
        br(),
        strong("# Click on the tabs above to navigate through the website")
      ),
      hr(),
      h3("Data Preview"), p("Here you may review each of the selected column data and summary"),
      hr(),
      fluidRow(
        column(
          4,
          selectInput(
            "variable",
            "Select column to view summary**",
            c(
              "Project Cost" = "project_cost",
              "Incentives" = "incentives",
              "Amount Financed" = "amount_financed",
              "Annual Savings Energy (USD)" = "savings_energy",
              "Annual Savings KWH" = "savings_kwh",
              "Annual Savings MMBTU" = "savings_mmbtu",
              "Performance Indicator" = "performance_indicator",
              "Gas Utility" = "gas_utility",
              "Electric Utility" = "electric_utility",
              "Customer Type" = "customer_type",
              "Type of Financing" = "type_of_financing",
              "Fuel Type" = "fuel_type",
              "Audit" = "audit",
              "Location" = "location"
            )
          ),
          h5("**The ID columns and date columns have been ignored"),
          submitButton("Submit")
        ),
        column(8, h3("Summary"), tableOutput("summary"))
      ),
      hr(),
      h2("First 5 values of selected columns"),
      hr(),
      h3("Factor Variables:"),
      hr(),
      tableOutput("factors"),
      hr(),
      fluidRow(
        column(4, h3("Predictor Variables:"), hr(),
               tableOutput("predictors")),
        column(4, h3("Outcome Variables:"), hr(),
               tableOutput("outcomes"))
      )
      
    ),
    tabPanel(
      title = "Exploratory Graphs",
      h3("Exploratory Graphs"),p("You may review the relation of factors viz-a-viz the predictor and outcome columns. You may do so by selecting the appropriate columns and clicking the submit button"),
      hr(),
      fluidRow(
        column(3, selectInput(
          "predictorF",
          "Predictor (USD):",
          c(
            "Project Cost" = "project_cost",
            "Incentives" = "incentives",
            "Amount Financed" = "amount_financed"
          )
        )),
        column(3,
               selectInput(
                 "outcomeF",
                 "Outcome:",
                 c(
                   "Annual Savings Energy (USD)" = "savings_energy",
                   "Annual Savings KWH" = "savings_kwh",
                   "Annual Savings MMBTU" = "savings_mmbtu"
                 )
               )),
        column(3,
               selectInput(
                 "colorF",
                 "Factor:",
                 c(
                   "Performance Indicator" = "performance_indicator",
                   "Gas Utility" = "gas_utility",
                   "Electric Utility" = "electric_utility",
                   "Customer Type" = "customer_type",
                   "Type of Financing" = "type_of_financing",
                   "Fuel Type" = "fuel_type",
                   "Audit" = "audit"
                 )
               )),
        column(3,
               h3(" "),
               submitButton("Submit"))
      ),
      hr(),
      fluidRow(column(12, plotOutput("outfactor")), hr(), column(12, plotOutput("predfactor")))
    ),
    tabPanel(
      title = "Regression Analysis",
      h3("Linear Regression Analysis and related graphs"),p("In this page, you will able to calculate predicted values of the outcome columns with regression analysis. To do so, please select the appropriate outcome and predictor columns,enter a numerical value in the 'Newdata' field and click the Submit button."),
      hr(),
      fluidRow(
        column(4, h3("Predicted Value:"), h2(textOutput("predict"))),
        column(
          4,
          h4("Select and enter appropriate values below:"),
          selectInput(
            "predictor",
            "Predictor (USD):",
            c(
              "Project Cost" = "project_cost",
              "Incentives" = "incentives",
              "Amount Financed" = "amount_financed"
            )
          ),
          selectInput(
            "outcome",
            "Outcome:",
            c(
              "Annual Savings Energy (USD)" = "savings_energy",
              "Annual Savings KWH" = "savings_kwh",
              "Annual Savings MMBTU" = "savings_mmbtu"
            )
          ),
          selectInput(
            "facet",
            "Facet:",
            c(
              "None" = " ",
              "Performance Indicator" = "performance_indicator",
              "Gas Utility" = "gas_utility",
              "Electric Utility" = "electric_utility",
              "Customer Type" = "customer_type",
              "Type of Financing" = "type_of_financing",
              "Fuel Type" = "fuel_type",
              "Audit" = "audit"
            )
          )
        ),
        column(
          4,
          selectInput(
            "color",
            "Color:",
            c(
              "None" = " ",
              "Performance Indicator" = "performance_indicator",
              "Gas Utility" = "gas_utility",
              "Electric Utility" = "electric_utility",
              "Customer Type" = "customer_type",
              "Type of Financing" = "type_of_financing",
              "Fuel Type" = "fuel_type",
              "Audit" = "audit"
            )
          ),
          numericInput("newdata", "New Data to predict:", value = 0),
          submitButton("Submit")
        )
        
        
      ),
      hr(),
      fluidRow(column(12,
                      plotOutput("out", height = 1000))),
      hr()
    ),
    
    tabPanel(
      title = "Geospatial Overview",
      h3("Geospatial Overview"),
      h4("This map shows the locations and associated 'Energy Savings'"),
      hr(),
      leafletOutput("intro", height = 1000)
    )
  )
))
