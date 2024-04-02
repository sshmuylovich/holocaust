if(!require(shiny)){install.packages('shiny', dependencies = TRUE)}
if(!require(DT)){install.packages('DT', dependencies = TRUE)}
if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
if(!require(tidyverse)){install.packages('tidyverse', dependencies = TRUE)}
if(!require(dplyr)){install.packages('dplyr', dependencies = TRUE)}
if(!require(maps)){install.packages('maps', dependencies = TRUE)}
library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(maps)

data <- read_csv("Auschwitz_Death_Certificates_1942-1943 - Auschwitz.csv")

data(world.cities)

city_country_mapping <- world.cities

city_country_mapping <- city_country_mapping %>%
  rename(
    city = name, 
    country = country.etc
  ) %>%
  select(city, country)

city_country_mapping <- data %>% 
  distinct(Birthplace) %>%
  arrange(Birthplace) %>%
  left_join(city_country_mapping, by = c("Birthplace" = "city")) %>%
  filter(!is.na(country)) %>% 
  distinct(Birthplace, country)

data <- data %>%
  left_join(city_country_mapping, by = c("Birthplace" = "Birthplace"), relationship = "many-to-many")

data <- data %>% rename(
  DateOfBirth = 'Date of Birth',
  DateOfDeath = 'Date of Death'
) %>%
  mutate(
    Nationality = country, 
    AgeAtDeath = floor(interval(ymd(DateOfBirth), ymd(DateOfDeath)) / years(1))
  )

ui <- fluidPage(
  titlePanel("Auschwitz 1942-1943 Death Certificates Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataSelection",
                  label = "Select Data for Graph:",
                  choices = c("Nationality", "AgeAtDeath", "Religion")),
      uiOutput("nationalityOrAgeOrReligionInput")
      ),
    mainPanel(
      plotOutput("holocaustPlot"),
      DTOutput("holocaustTable")
      )
    )
  )


server <- function(input, output, session) {
  output$nationalityOrAgeOrReligionInput <- renderUI({
    if (input$dataSelection == "Nationality") {
      checkboxGroupInput(inputId = "Nationalities",
                         label = "Select Nationalities for Graph:",
                         choices = sort(unique(data$Nationality)),
                         selected = sort(unique(data$Nationality))[1])
    } 
    else if (input$dataSelection == "AgeAtDeath"){
      checkboxGroupInput(inputId = "Ages",
                         label = "Select Ages:",
                         choices = sort(unique(data$AgeAtDeath)),
                         selected = sort(unique(data$AgeAtDeath))[1])
    } 
    else {
      checkboxGroupInput(inputId = "Religions",
                         label = "Select Religions:",
                         choices = sort(unique(data$Religion)),
                         selected = sort(unique(data$Religion))[1])
    }
  })
  
  
output$holocaustPlot <- renderPlot({
  if (input$dataSelection == "Nationality") {
    selected <- input$Nationalities
    data_to_plot <- data %>%
      filter(Nationality %in% selected) %>%
      group_by(Nationality) %>%
      summarise(Count = n())
    
    ggplot(data_to_plot, aes(x = Nationality, y = Count, fill = Nationality)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.5) + 
      labs(x = "Nationality", y = "Count", title = "Number of People Murdered by Nationality") +
      theme_minimal()
    }
  else if (input$dataSelection %in% c("AgeAtDeath")) {
    selected <- input$Ages
    data_to_plot <- data %>%
      filter(AgeAtDeath %in% selected) %>%
      group_by(AgeAtDeath) %>%
      summarise(Count = n())
    
    ggplot(data_to_plot, aes(x = AgeAtDeath, y = Count, fill = AgeAtDeath)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.5) + 
      labs(x = "Age At Death", y = "Count", title = "Number of People Murdered by Age") +
      theme_minimal()
    }
  else {
    selected <- input$Religions
    data_to_plot <- data %>%
      filter(Religion %in% selected) %>%
      group_by(Religion) %>%
      summarise(Count = n())
    
    ggplot(data_to_plot, aes(x = Religion, y = Count, fill = Religion)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.5) + 
      labs(x = "Religion", y = "Count", title = "Number of People Murdered by Religion") +
      theme_minimal()
    }
  })
  
output$holocaustTable <- renderDT({
  if (input$dataSelection %in% c("Nationality")) {
    selected <- input$Nationalities
    data_to_display <- data %>%
      filter(Nationality %in% selected)
  } else if (input$dataSelection %in% c("AgeAtDeath")) {
    selected <- input$Ages
    data_to_display <- data %>%
      filter(AgeAtDeath %in% selected)
  } else {
    selected <- input$Religions
    data_to_display <- data %>%
      filter(Religion %in% selected)
  }
  datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE))
  })
}
  
shinyApp(ui = ui, server = server)
