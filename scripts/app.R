library(tidyverse)
library(shiny)


combined_data <- readRDS("../dataset/combined_nypd_acs_with_coords_fixed.rds") %>%
  mutate(unemployment_rate = as.numeric(unemployment_rate))

boro_choices   <- c("All", sort(unique(combined_data$borough)))
offense_choices <- c("All", sort(unique(combined_data$ofns_desc)))
race_choices   <- c("All", sort(unique(combined_data$perp_race)))
sex_choices    <- c("All", sort(unique(combined_data$perp_sex)))
age_choices    <- sort(unique(combined_data$age_group))


ui <- fluidPage(
  titlePanel("Arrest Trends Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("boro",    "Borough:",           choices = boro_choices),
      selectInput("offense", "Offense Type:",      choices = offense_choices),
      selectInput("demo",    "Demographic Filter:", choices = c("None","Race","Sex","Age Group")),
      
      conditionalPanel(
        "input.demo == 'Race'",
        selectInput("race", " Race:", choices = race_choices)
      ),
      conditionalPanel(
        "input.demo == 'Sex'",
        selectInput("sex", " Sex:", choices = sex_choices)
      ),
      conditionalPanel(
        "input.demo == 'Age Group'",
        selectInput("age", " Age Group:", choices = age_choices)
      )
    ),
    
    mainPanel(
      plotOutput("barPlot", height = "500px")
    )
  )
)


server <- function(input, output, session) {
  filtered <- reactive({
    df <- combined_data
    
    if (input$boro    != "All") df <- df %>% filter(borough   == input$boro)
    if (input$offense != "All") df <- df %>% filter(ofns_desc == input$offense)
    
    if (input$demo == "Race"      && input$race != "All")
      df <- df %>% filter(perp_race == input$race)
    
    if (input$demo == "Sex"       && input$sex  != "All")
      df <- df %>% filter(perp_sex  == input$sex)
    
    if (input$demo == "Age Group" && input$age  != "")
      df <- df %>% filter(age_group == input$age)
    
    df
  })
  
  output$barPlot <- renderPlot({
    filtered() %>%
      count(borough) %>%
      ggplot(aes(x = reorder(borough, n), y = n, fill = borough)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = "Number of Arrests by Borough",
        x     = NULL,
        y     = "Arrest Count"
      ) +
      theme_minimal()
  })
}


shinyApp(ui, server)
