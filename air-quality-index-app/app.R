library(shiny)
library(shinyjs)
library(markdown)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(randomForestSRC)

shinyLink <- function(to, label) {
  tags$a(class = "shiny__link", href = to, label)
}

dataset <- read_delim(file = "www/air-quality-countries.csv",
                      delim = ",",
                      col_names = TRUE)
dataset <- dataset %>%
  mutate(Country = factor(Country)) %>%
  mutate(City = factor(City)) %>%
  mutate(`AQI Category` = factor(`AQI Category`)) %>%
  mutate(`CO AQI Category` = factor(`CO AQI Category`)) %>%
  mutate(`Ozone AQI Category` = factor(`Ozone AQI Category`)) %>%
  mutate(`NO2 AQI Category` = factor(`NO2 AQI Category`)) %>%
  mutate(`PM2.5 AQI Category` = factor(`PM2.5 AQI Category`))

categorical_variables <- dataset %>%
  dplyr::select(where(is.factor))

continuous_variables <- dataset %>%
  dplyr::select(where(is.double))

min_continuous_variables <- sapply(continuous_variables, min, na.rm = TRUE)
mean_continuous_variables <- sapply(continuous_variables, mean, na.rm = TRUE)
max_continuous_variables <- sapply(continuous_variables, max, na.rm = TRUE)
sd_continuous_variables <- sapply(continuous_variables, sd, na.rm = TRUE)
median_continuous_variables <- sapply(continuous_variables, median, na.rm = TRUE)

stats <- data.frame(
  min = min_continuous_variables,
  mean = mean_continuous_variables,
  median = median_continuous_variables,
  max = max_continuous_variables,
  std = sd_continuous_variables
)

distinct_values_list <- data.frame(
  Column = character(),
  Distinct_Values = character(),
  stringsAsFactors = FALSE
)

for (col in colnames(categorical_variables)) {
  if (col == 'Country' || col == 'City') {
    next
  }
  
  distinct_values <- paste(levels(categorical_variables[[col]]), collapse = ", ")
  distinct_values_list <- rbind(
    distinct_values_list,
    data.frame(
      Column = col,
      Distinct_Values = distinct_values,
      stringsAsFactors = FALSE
    )
  )
}

aqi_percentage <- dataset %>%
  count(`AQI Category`) %>%
  mutate(percentage = round(n / sum(n) * 100, 3)) %>%
  arrange(desc(percentage))

dataset <- dataset %>%
  mutate(
    `Air Quality Index Category` = case_when(
      `AQI Category` %in% c(
        "Unhealthy for Sensitive Groups",
        "Unhealthy",
        "Very Unhealthy",
        "Hazardous"
      ) ~ "Bad",
      `AQI Category` == "Good" ~ "Good",
      `AQI Category` == "Moderate" ~ "Moderate"
    )
  )

dataset <- dataset %>%
  filter(!is.na(Country))

final_dataset <- dataset %>% mutate(`Air Quality Index Category` = factor(`Air Quality Index Category`))

final_dataset <- final_dataset %>% dplyr::select(
  Country,
  `CO AQI Value`,
  `CO AQI Category`,
  `Ozone AQI Value`,
  `Ozone AQI Category`,
  `NO2 AQI Value`,
  `NO2 AQI Category`,
  `PM2.5 AQI Value`,
  `PM2.5 AQI Category`,
  lat,
  lng,
  `Air Quality Index Category`
)

final_dataset <- final_dataset %>%
  rename(
    CO_AQI_Value = `CO AQI Value`,
    CO_AQI_Category = `CO AQI Category`,
    Ozone_AQI_Value = `Ozone AQI Value`,
    Ozone_AQI_Category = `Ozone AQI Category`,
    NO2_AQI_Value = `NO2 AQI Value`,
    NO2_AQI_Category = `NO2 AQI Category`,
    PM25_AQI_Value = `PM2.5 AQI Value`,
    PM25_AQI_Category = `PM2.5 AQI Category`,
    Air_Quality_Index_Category = `Air Quality Index Category`
  )

final_rf_model = rfsrc(
  Air_Quality_Index_Category ~ Ozone_AQI_Category + Ozone_AQI_Value + PM25_AQI_Value + PM25_AQI_Category + NO2_AQI_Value + CO_AQI_Value,
  data = as.data.frame(final_dataset),
  mtry = 2,
  importance = TRUE
)

ui <- tagList(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(type = "text/javascript", src = "code.js"),
  ),
  navbarPage(
    "Air Quality Index Explorer",
    tabPanel(
      "Description",
      p(
        "The data set called",
        shinyLink(to = "https://www.kaggle.com/datasets/adityaramachandran27/world-air-quality-index-by-city-and-coordinates/data", label = "World Air Quality Index by City and Coordinates"),
        "aims to provide valuable insights about the air quality of different regions. The goal of my project is to classify the air quality of a given city (*Good*, *Moderate*, etc.). This is predicted using the country and city name, carbon monoxide, ozone, PM2.5 and nitrogen value."
      ),
      p(
        "Initially, the data set has 14 columns and 16695 rows. After the cleaning process, it only remains 12 variables, the City, AQI Value being dropped. The original features are Country, City, AQI Value, AQI Category, CO AQI Value, CO AQI Category, Ozone AQI Value, Ozone AQI Category, NO2 AQI Value, NO2 AQI Category, PM2.5 AQI Value, PM2.5 AQI Category, lat, lng, and they are described as follows:"
      ),
      htmlOutput("list_description"),
      p(
        "For the above mentioned goal, the target column will be AQI Category."
      )
    ),
    tabPanel("Data", DT::dataTableOutput("table")),
    tabPanel("Map", leafletOutput("aqi_map", height = "1000px")),
    tabPanel(
      "Stats",
      h2("Continous Features"),
      DT::dataTableOutput("summary"),
      h2("Categorical Features"),
      h3("Distinct Values for Each AQI Categorical Variable"),
      DT::dataTableOutput("distinct_values_list"),
      h3("Air Quality Index Categories"),
      DT::dataTableOutput("aqi_percentage")
    ),
    tabPanel("Explore", sidebarLayout(
      sidebarPanel(
        numericInput(
          "ozone_aqi_value",
          "Ozone AQI Value",
          value = round(mean(final_dataset$Ozone_AQI_Value), 2),
          min = min(final_dataset$Ozone_AQI_Value),
          max = max(final_dataset$Ozone_AQI_Value)
        ),
        textInput("ozone_aqi_category", "Ozone AQI Category", "Good"),
        numericInput(
          "pm25_aqi_value",
          "PM2.5 AQI Value",
          value = round(mean(final_dataset$PM25_AQI_Value), 2),
          min = min(final_dataset$PM25_AQI_Value),
          max = max(final_dataset$PM25_AQI_Value)
        ),
        textInput("pm25_aqi_category", "PM2.5 AQI Category", "Moderate"),
        numericInput(
          "no2_aqi_value",
          "NO2 AQI Value",
          value = round(mean(final_dataset$NO2_AQI_Value), 2),
          min = min(final_dataset$NO2_AQI_Value),
          max = max(final_dataset$NO2_AQI_Value)
        ),
        numericInput(
          "co_aqi_value",
          "CO AQI Value",
          value = round(mean(final_dataset$CO_AQI_Value), 2),
          min = min(final_dataset$CO_AQI_Value),
          max = max(final_dataset$CO_AQI_Value)
        ),
        actionButton("predict", "Predict"),
      ),
      mainPanel(
        h1("Predicted Air Quality Index Category"),
        p(
          "Some figures for the polluants in different regions can be found",
          shinyLink(to = "https://www.iqair.com/world-air-quality-ranking?srsltid=AfmBOooPXg5hUA_swxmgbo1FQM-QbISWejSaODP9YroZ_UOxMRaofLBj", label = "here.")
        ),
        plotOutput("feature_importance"),
        textOutput("prediction_result"),
        br(),
        uiOutput("prediction_image")
      )
    ))
  )
)

server <- function(input, output, session) {
  shinyjs::disable("pm25_aqi_category")
  shinyjs::disable("ozone_aqi_category")
  
  output$summary <- DT::renderDataTable({
    DT::datatable(stats)
  })
  
  color_map <- function(aqi_category) {
    if (aqi_category == "Good") {
      return("blue")
    } else if (aqi_category == "Moderate") {
      return("yellow")
    } else if (aqi_category == "Bad") {
      return("red")
    } else {
      return("gray")
    }
  }
  
  output$aqi_map <- renderLeaflet({
    leaflet(data = final_dataset) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(
        lng = ~ lng,
        lat = ~ lat,
        color = ~ sapply(Air_Quality_Index_Category, color_map),
        radius = 10,
        popup = ~ paste(Air_Quality_Index_Category),
        fillOpacity = 0.8
      ) %>% setView(lng = 6.895,
                    lat = 52.267,
                    zoom = 9)
  })
  
  output$feature_importance <- renderPlot({
    feature_importance_df <- as.data.frame(final_rf_model$importance)
    feature_importance_all <- data.frame("feature" = rownames(feature_importance_df),
                                         "all" = feature_importance_df$all) %>% arrange(desc(all))
    
    ggplot(feature_importance_all, aes(x = feature, y = all)) +
      geom_bar(stat = "identity") +
      labs(title = "Random Forest Overall Feature Importance", x = "Feature", y = "Importance") +
      theme_minimal() +
      coord_flip()
  })
  
  output$distinct_values_list <- DT::renderDataTable({
    DT::datatable(distinct_values_list)
  })
  
  output$aqi_percentage <- DT::renderDataTable({
    DT::datatable(aqi_percentage)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(dataset)
  })
  
  output$list_description <- renderUI({
    HTML(
      "<ul>
            <li><b>Country:</b> Country Name</li>
            <li><b>City:</b> City Name</li>
            <li><b>AQI Value/AQI Category:</b> Air Quality Index</li>
            <li><b>CO AQI Value/CO AQI Category:</b> Carbon Monoxide (CO) is a colorless, odorless gas that is produced by the incomplete burning of fossil fuels. High levels of CO can be toxic to humans and can cause headaches, dizziness, and nausea.</li>
            <li><b>Ozone AQI Value/Ozone AQI Category:</b> Ozone is a gas that can form in the atmosphere through a chemical reaction between sunlight and other pollutants. High levels of ozone can be harmful to human health, particularly for those with respiratory issues.</li>
            <li><b>PM2.5 AQI Value/PM2.5 AQI Category:</b> PM2.5 refers to tiny particles or droplets in the air that are 2.5 micrometers or less in width. They can be harmful to human health when inhaled, especially in high concentrations.</li>
            <li><b>NO2 AQI Value/NO2 AQI Category:</b> Nitrogen Dioxide (NO₂) is a gas that forms in the atmosphere from the combustion of fossil fuels, mainly from vehicles and industrial activities. Elevated levels of NO₂ can be harmful to human health, particularly for individuals with respiratory conditions, as it can irritate the airways and worsen lung diseases like asthma.</li>
            <li><b>lat:</b> Region Latitude</li>
            <li><b>lng:</b> Region Longitude</li>
          </ul>"
    )
  })
  
  prediction <- reactive({
    ozone_value <- ifelse(
      is.na(input$ozone_aqi_value) ||
        input$ozone_aqi_value < 0,
      0,
      input$ozone_aqi_value
    )
    pm25_value <- ifelse(is.na(input$pm25_aqi_value) ||
                           input$pm25_aqi_value < 0,
                         0,
                         input$pm25_aqi_value)
    no2_value <- ifelse(is.na(input$no2_aqi_value) ||
                          input$no2_aqi_value < 0,
                        0,
                        input$no2_aqi_value)
    co_value <- ifelse(is.na(input$co_aqi_value) ||
                         input$co_aqi_value < 0,
                       0,
                       input$co_aqi_value)
    pm25_category <- "Good"
    ozone_category <- "Good"
    
    if (pm25_value >= 51 && pm25_value <= 99) {
      pm25_category <- "Moderate"
    } else if (pm25_value >= 100 && pm25_value <= 149) {
      pm25_category <- "Unhealthy for Sensitive Groups"
    } else if (pm25_value >= 150 && pm25_value <= 200) {
      pm25_category <- "Unhealthy"
    } else if (pm25_value >= 201 && pm25_value <= 296) {
      pm25_category <- "Very Unhealthy"
    } else if (pm25_value >= 297) {
      pm25_category <- "Hazardous"
    }
    
    updateTextInput(session, "pm25_aqi_category", value = pm25_category)
    
    if (ozone_value >= 51 && ozone_value <= 99) {
      ozone_category <- "Moderate"
    } else if (ozone_value >= 100 && ozone_value <= 149) {
      ozone_category <- "Unhealthy for Sensitive Groups"
    } else if (ozone_value >= 150 && ozone_value <= 200) {
      ozone_category <- "Unhealthy"
    } else if (ozone_value >= 201) {
      ozone_category <- "Very Unhealthy"
    }
    
    updateTextInput(session, "ozone_aqi_category", value = ozone_category)
    
    
    input_data <- data.frame(
      Ozone_AQI_Value = ozone_value,
      PM25_AQI_Value = pm25_value,
      NO2_AQI_Value = no2_value,
      CO_AQI_Value = co_value,
      PM25_AQI_Category = pm25_category,
      Ozone_AQI_Category = ozone_category
    )
    prediction <- predict(final_rf_model, newdata = input_data)$predicted
    max_label_index <- which.max(prediction)
    max_label <- colnames(prediction)[max_label_index]
    
    max_label
  })
  
  output$prediction_result <- renderText({
    req(input$predict)
    predicted_value <- prediction()
    paste("The predicted air quality index category is: ",
          predicted_value)
  })
  
  output$prediction_image <- renderUI({
    req(input$predict)
    predicted_value <- prediction()
    
    if (predicted_value == "Good") {
      img(src = "happy_face.png",
          width = "100px",
          style = "display: block; margin: 0 auto;")
    } else if (predicted_value == "Moderate") {
      img(src = "meh_face.png",
          width = "100px",
          style = "display: block; margin: 0 auto;")
    } else {
      img(src = "sad_face.png",
          width = "100px",
          style = "display: block; margin: 0 auto;")
    }
  })
  
}

shinyApp(ui = ui, server = server)
