library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(scales)
library(plotly)
library(formattable)

shinyServer(function(input, output) {
  
  ## Download the data
  # data <-read.csv("https://data.ny.gov/api/views/assk-vu73/rows.csv?accessType=DOWNLOAD")
  data <- read.csv("energystar.csv")
  
  ## Set clear and workable column names
  colnames(data) <-
    c(
      "reporting_period",
      "project_id",
      "site_id",
      "county",
      "city",
      "zip",
      "gas_utility",
      "electric_utility",
      "completion_date",
      "customer_type",
      "performance_indicator",
      "project_cost",
      "incentives",
      "type_of_financing",
      "amount_financed",
      "fuel_type",
      "savings_kwh",
      "savings_mmbtu",
      "savings_energy",
      "audit",
      "location"
    )
  
  ## Declare labels
  labels <-
    list(
      "gas_utility" = "Gas Utility",
      "electric_utility" = "Electric Utility",
      "customer_type" = "Customer Type",
      "performance_indicator" = "Performance Indicator",
      "project_cost" = "Project Cost",
      "incentives" = "Incentives",
      "amount_financed" = "Amount Financed",
      "fuel_type" = "Fuel Type",
      "savings_kwh" = "Annual Savings KWH",
      "savings_mmbtu" = "Annual Savings MMBTU",
      "savings_energy" = "Annual Savings Energy",
      "audit" = "Audit"
    )
  
  ## Functions for normalizing data and combining factor levels
  combofactor <-
    function(pattern_vector,
             replacement_vector,
             vec_data) {
      levels <- levels(vec_data)
      for (i in 1:length(pattern_vector))
        levels[which(pattern_vector[i] == levels)] <-
          replacement_vector[i]
      levels(vec_data) <- levels
      vec_data
    }
  normalize <- function(vector)
  {
    (vector - mean(vector)) / sd(vector)
  }
  ## Combine factor variables
  data$fuel_type <-
    combofactor("Natural gas", "Natural Gas", data$fuel_type)
  data$gas_utility <-
    combofactor(
      c("Corning NATURAL Gas", ""),
      c("Corning Natural Gas", "Unknown Gas Provider"),
      data$gas_utility
    )
  
  ## Group by factor variables and summarize on predictor and outcome variables
  data <-
    data %>% tbl_df %>% group_by(
      location,
      gas_utility,
      electric_utility,
      customer_type,
      performance_indicator,
      type_of_financing,
      fuel_type,
      audit
    ) %>% summarize(
      project_cost = sum(project_cost),
      incentives = sum(incentives),
      amount_financed = sum(amount_financed),
      savings_kwh = sum(savings_kwh),
      savings_mmbtu = sum(savings_mmbtu),
      savings_energy = sum(savings_energy)
    ) %>% data.frame()
  
  ## Fitler dataset rows that have missing or incorrect location
  lat_num <-
    which(!is.na(as.numeric((
      gsub('.*\\((.*),.*', '\\1', data$location)
    ))))
  data <- data[lat_num,]
  
  ## Create dataset for showcasing leaflet map
  latitude <-
    as.numeric((gsub('.*\\((.*),.*', '\\1', data$location)))
  longitude <-
    as.numeric((gsub('.*, (.*)\\).*', '\\1', data$location)))
  savings_energy <- data$savings_energy
  map_data <- data.frame(latitude, longitude, savings_energy)
  
  ## Reactive methods for calculate model fit and prediction
  fit <-
    reactive({
      predictor <- data[, input$predictor]
      lm(data = data, formula = data[, input$outcome] ~ predictor)
    })
  
  pred <-
    reactive({
      predictor <- data[, input$predictor]
      predict(fit(), newdata = data.frame(predictor = c(input$newdata)))
    })
  
  
  output$intro <- renderLeaflet({
    leaflet(map_data) %>% addTiles() %>%
      addCircleMarkers(
        lng = ~ longitude,
        lat = ~ latitude,
        radius = ~ savings_energy / 10000
      )
  })
  
  output$factors <- renderTable({
    factors <- data[1:5, 1:8]
    colnames(factors) <- c(
      "Location",
      "Gas Utility",
      "Electric Utility",
      "Customer Type",
      "Performance Indicator",
      "Type of Financing",
      "Fuel Type",
      "Audit"
    )
    factors
  })
  
  output$predictors <- renderTable({
    predictors <- data[1:5, 9:11]
    colnames(predictors) <- c("Project Cost",
                              "Incentives",
                              "Amount Financed")
    predictors
  })
  
  output$outcomes <- renderTable({
    outcomes <- data[1:5, 12:14]
    colnames(outcomes) <- c("Annual Savings KWH",
                            "Annual Savings MMBTU",
                            "Annual Savings Energy")
    outcomes
  })
  
  
  
  th <- theme(
    title = element_text(face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    legend.text = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10)
    
  )
  
  
  data_out <- reactive({
    data %>% tbl_df() %>% mutate(factor = data[, input$colorF], outcome = data[, input$outcomeF]) %>%
      group_by(factor) %>%
      summarize(outcome = sum(outcome)) %>%
      data.frame()
  })
  
  data_pred <- reactive({
    data %>% tbl_df() %>% mutate(factor = data[, input$colorF], predictor = data[, input$predictorF]) %>%
      group_by(factor) %>%
      summarize(predictor = sum(predictor)) %>%
      data.frame()
  })
  
  
  
  output$outfactor <- renderPlot({
    data2 <- data_out()
    g <-
      ggplot(data2, aes(factor, outcome)) +
      geom_col(width = .5) +
      th +
      labs(
        x = paste(labels[input$colorF]),
        y = paste(labels[input$outcomeF]),
        title = paste(labels[input$colorF], "vs", paste(labels[input$outcomeF]))
      )
    if (input$outcomeF == "savings_energy")
      g <- g + scale_y_continuous(labels = dollar)
    else
      g <- g + scale_y_continuous(labels = comma)
    g <- g + coord_flip()
    g
    
  })
  
  output$predfactor <- renderPlot({
    data2 <- data_pred()
    ggplot(data2, aes(factor, predictor)) +
      geom_col(width = .5) +
      th +
      labs(
        x = paste(labels[input$colorF]),
        y = paste(labels[input$predictorF]),
        title = paste(labels[input$colorF], "vs", paste(labels[input$predictorF]))
      ) +  scale_y_continuous(labels = dollar) +  coord_flip()
  })
  
  
  output$facetSize <- renderText({
    if (input$facet != " ")
      paste(length(levels(data[, input$facet])) * 300)
    else
      paste("600")
  })
  
  output$out <- renderPlot({
    g <- ggplot(data = data,
                aes(data[, input$predictor],
                    data[, input$outcome])) +
      geom_point(size = normalize(data[, input$predictor]))
    if (input$color != " ") {
      g <- g +
        aes(colour = data[, input$color]) + labs(colour = paste(labels[input$color]))
    }
    g <- g +
      labs(
        title = paste(labels[input$predictor], "vs", paste(labels[input$outcome])),
        x = paste(labels[input$predictor]),
        y = paste(labels[input$outcome])
        
      ) + th +  scale_x_continuous(labels = dollar) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "black",
        formula = y ~ x
      )
    if (input$facet != " ")
      g <- g + facet_grid(data[, input$facet] ~ .)
    if (input$outcome == "savings_energy")
      g <- g + scale_y_continuous(labels = dollar)
    else
      g <- g + scale_y_continuous(labels = comma)
    g
  })
  
  
  
  output$predict <- renderText({
    if (input$outcome == "savings_energy")
      paste(currency(pred()))
    else
      paste(comma(pred()))
  })
  
  summary <-
    data.frame(unclass(summary(data)),
               check.names = FALSE,
               stringsAsFactors = FALSE)
  colnames(summary) <- c(
    "location",
    "gas_utility",
    "electric_utility",
    "customer_type",
    "performance_indicator",
    "type_of_financing",
    "fuel_type",
    "audit",
    "project_cost",
    "incentives",
    "amount_financed",
    "savings_kwh",
    "savings_mmbtu",
    "savings_energy"
  )
  
  output$summary <- renderTable({
    summary[, input$variable]
  })
  
})
