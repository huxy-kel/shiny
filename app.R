library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)

prizes <- read_csv("prizes.csv")

ui <- navbarPage(
  "British Literary Prizes Dashboard",
  # Page 1: Authors Explorer
  tabPanel("Authors Explorer",
           fluidPage(
             
             tags$head(
               tags$style(HTML("
                 .dataTables_wrapper { width: 100% !important; }
               "))
             ),
             
             sidebarLayout(
               
               sidebarPanel(
                 width = 4,
                 h3("Filter Authors"),
                 
                 selectInput(
                   "filter_var", "Choose a filter variable:",
                   choices = c(
                     "Gender" = "gender",
                     "Ethnicity" = "ethnicity_macro",
                     "Degree Institution" = "degree_institution"
                   )
                 ),
                 
                 uiOutput("filter_value_ui"),
                 
                 radioButtons("role", "Select role:",
                              choices = c("Winners" = "winner",
                                          "Shortlisted" = "shortlisted"),
                              inline = TRUE)
               ),
               
               mainPanel(
                 width = 8,
                 h3("Filtered Authors"),
                 DTOutput("authors_table")
               )
             )
           )
  ),
  
  # Page 2: Trends
  tabPanel("Prize Trends",
           fluidPage(
             h3("Prize Count by Year"),
             plotlyOutput("year_trend"),
             
             h3("Winners by Gender Over Time"),
             plotlyOutput("gender_trend")
           )
  ),
  
  # Page 3: Demographics
  tabPanel("Demographics Explorer",
           fluidPage(
             fluidRow(
               column(6,
                      h3("Gender Distribution"),
                      plotOutput("gender_plot")
               ),
               column(6,
                      h3("Ethnicity Distribution"),
                      plotOutput("ethnicity_plot")
               )
             ),
             
             h3("Top 10 Degree Institutions"),
             plotOutput("degree_plot")
           )
  )
)


server <- function(input, output, session) {
  
  # Update right-side dropdown values
  output$filter_value_ui <- renderUI({
    req(input$filter_var)
    
    vals <- prizes %>%
      filter(!is.na(.data[[input$filter_var]])) %>%
      distinct(.data[[input$filter_var]]) %>%
      pull()
    
    selectInput("filter_value", "Select value:", choices = vals)
  })
  
  # Filtered dataset
  filtered_data <- reactive({
    req(input$filter_var, input$filter_value, input$role)
    
    prizes %>%
      filter(
        .data[[input$filter_var]] == input$filter_value,
        person_role == input$role
      ) %>%
      select(
        name,
        prize_name,
        prize_year,
        prize_genre,
        person_role,
        gender,
        ethnicity_macro,
        degree_institution,
        degree_field
      )
  })
  
  # Data Table
  output$authors_table <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Page 2: Trend charts
  
  # Prize count over time
  output$year_trend <- renderPlotly({
    p <- prizes %>%
      group_by(prize_year) %>%
      summarise(count = n()) %>%
      ggplot(aes(prize_year, count)) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Number of Prizes")
    
    ggplotly(p)
  })
  
  # Gender winner trend
  output$gender_trend <- renderPlotly({
    p <- prizes %>%
      filter(person_role == "winner") %>%
      group_by(prize_year, gender) %>%
      summarise(count = n(), .groups = "drop") %>%
      ggplot(aes(prize_year, count, color = gender)) +
      geom_line() +
      labs(x = "Year", y = "Winner Count")
    
    ggplotly(p)
  })
  
  # Page 3: Demographics
  
  output$gender_plot <- renderPlot({
    prizes %>%
      count(gender) %>%
      ggplot(aes(gender, n)) +
      geom_col(fill = "steelblue") +
      labs(x = "Gender", y = "Count")
  })
  
  output$ethnicity_plot <- renderPlot({
    prizes %>%
      count(ethnicity_macro) %>%
      ggplot(aes(reorder(ethnicity_macro, n), n)) +
      geom_col(fill = "salmon") +
      coord_flip() +
      labs(x = "Ethnicity", y = "Count")
  })
  
  output$degree_plot <- renderPlot({
    prizes %>%
      count(degree_institution) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(reorder(degree_institution, n), n)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(x = "Degree Institution", y = "Count")
  })
}

shinyApp(ui, server)
