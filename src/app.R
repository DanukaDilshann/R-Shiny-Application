# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("data.table")
# install.packages("ggplot2")
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)
library(gridExtra)
library(sp)


# Load data
df <- read.csv('Disaster.csv')

#####################################################################
ui <- fluidPage(

  
  titlePanel("Natural Disasters (1970 -2021)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Natural disasters are catastrophic events that occur due to natural processes of the Earth. 
              These events can cause significant damage to life, property, and the environment. 
               Examples of natural disasters include earthquakes, floods, wildfires, 
               Strom and droughts ect."),
      selectInput("disaster", 
                  label = "Choose the Disaster",
                  choices = c(unique(df$Disaster.Type)),
                  selected = "Flood"),
      selectInput("contr", 
                  label = "Choose a Country",
                  choices = unique(df$Country),
                  selected = "China"),
      selectInput("year", 
                  label = "Year",
                  choices = unique(df$YearCat),
                  selected = '2012-2021'),
      fluidRow(
        column(4, actionButton("resetButton", "Reset to Default", icon = icon("reset"))),
        column(4, actionButton("myButton", "About Dataset", icon = icon("database"))),
        column(4, actionButton("myButton1", "Return", icon = icon("undo")))
      ),
      helpText(
        HTML("
    <p>By default, the following conditions are selected:</p>
    <ul>
      <li>Year: 2012-2021</li>
      <li>Disaster Type: Flood</li>
      <li>Country: China</li>
    </ul>
    <p>This selection shows the impact of floods in China from 2012 to 2021.</p>
    <p>Scatter plot and line chart change according to our selections.</p>
    <p>Bar graph shows the number of deaths in each disaster type in the selected country and year range.</p>
  ")
      )
    ),
    mainPanel(
      width = 50,
      uiOutput("panel1"),
    )
  )
)



###########################################################################

server <- function(input, output, session) {
  


  panel_state <- reactiveValues(panel = "data")
  

  observeEvent(input$myButton, {
    panel_state$panel <- "about"
  })
  
  observeEvent(input$myButton1, {
    panel_state$panel <- "data"
  })
  

  output$panel1 <- renderUI({
    if (panel_state$panel == "about") {
      mainPanel(
        width = 100,
        tags$h3("Description"),
        tags$p("The World Disaster dataset provides comprehensive information about various natural disasters that have occurred worldwide. The dataset covers a wide range of disasters including earthquakes, floods, hurricanes, tsunamis, droughts, and wildfires, among others. It includes data on the geographical location, time, magnitude, impact, and aftermath of these disasters."),
        tags$h3("Data Source"),
        HTML("
          <p>Data Source: <a href='https://www.kaggle.com/datasets/brsdincer/all-natural-disasters-19002021-eosdis'>Link to the data source</a></p>
        "),
        tags$h3("Data Preprocessing"),
        tags$p("To ensure data integrity and consistency, missing values in numerical columns such as 'Total Deaths,' 'Total Affected,' 'No. Homelessness,' and 'No. Injured' were addressed by replacing them with 0."),
        tags$p("The years have been categorized into groups of 10, and a new column named 'YearCat' has been created to reflect this categorization."),
        tags$h3("Dataset Preview"),
        verbatimTextOutput("dataset_preview")
      )
    } else {
      mainPanel(
        tableOutput("table"),
        plotOutput('barplot3'),
        plotOutput('barplot'),
        
      )
    }
  }) 
  
  # Dataset preview
  output$dataset_preview <- renderPrint({
    head(df, n = 5)
  })
  
  # Output table
  output$table <- renderTable({
    filtered_data <- df[df$YearCat == input$year & df$Country == input$contr & df$Disaster.Type == input$disaster, ]
    total_deaths <- sum(filtered_data$Total.Deaths)
    total_injuries <- sum(filtered_data$No.Injured)
    total_Affected <- sum(filtered_data$Total.Affected)
    NO.Hom <- sum(filtered_data$No.Homeless)
    recon <- sum(filtered_data$Reconstruction.Costs)
    data.frame("NO.Deaths" = format(total_deaths, big.mark = ",", scientific = FALSE),
               "NO.Injuries" = format(total_injuries, big.mark = ",", scientific = FALSE),
               "Total_NO.Affected" = format(total_Affected, big.mark = ",", scientific = FALSE),
               'No.Homelessness' = format(NO.Hom, big.mark = ",", scientific = FALSE),
               'Reconstruction_cost_in_USD' = format(recon, big.mark = ",", scientific = FALSE))
  }, rownames = FALSE)
  
  # Plot for barplot3
  output$barplot3 <- renderPlot({
    filtered_data3 <- df[df$YearCat == input$year & df$Country == input$contr & df$Disaster.Type == input$disaster,]
    
    if (nrow(filtered_data3) == 0) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No data available for aggregation", type = "n", xlab = "", ylab = "")
      return()
    }
    
    aggregated_data3 <- aggregate(Total.Damages ~ Year, data = filtered_data3, FUN = sum)
    
    p2 <- ggplot(aggregated_data3, aes(x = as.factor(Year), y = Total.Damages, group = 1)) +
      geom_line(color = "red") +
      labs(title = paste("Line Plot of Total Damages by Year in ", input$contr, " (", input$year, ")"), x = "Year", y = "Total Damages") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.title.y.right = element_text(color = "red"),
            plot.title = element_text(size = 14, face = "bold", color = "blue"))
    
    p1 <- ggplot(filtered_data3, aes(x = Total.Deaths, y = Total.Damages)) +
      geom_point() +
      labs(title = paste("Scatter plot of Total Damages vs Total Deaths in ", input$contr, " (", input$year, ")"), x = "Total Deaths", y = "Total Damages") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold", color = "blue"))
    
    grid.arrange(p1, p2, ncol = 1)
  })
  
  # Plot for barplot
  output$barplot <- renderPlot({
    filtered_data1 <- df[df$YearCat == input$year & df$Country == input$contr, ]
    aggregated_data <- aggregate(Total.Deaths ~ Disaster.Type, data = filtered_data1, FUN = sum)
    
    aggregated_data$Disaster.Type <- factor(aggregated_data$Disaster.Type, levels = aggregated_data$Disaster.Type[order(-aggregated_data$Total.Deaths)])
    
    ggplot(aggregated_data, aes(x = reorder(Disaster.Type, -Total.Deaths), y = Total.Deaths)) +
      geom_bar(stat = "identity", fill = "skyblue", colour = "black") +
      geom_text(aes(label = Total.Deaths), vjust = -0.5, size = 5) +
      labs(title = paste("Comparison of Total Deaths by Disaster Type in ", input$contr, " (", input$year, ")"), x = "Disaster Type", y = "Total Deaths") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 15),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 14, face = "bold", color = "blue"))+
      coord_cartesian(clip = "off") + 
      theme(plot.margin = margin(1, 1, 1, 4, "cm")) 
      
  })
  
  
  # Reset button action
  observeEvent(input$resetButton, {
    updateSelectInput(session, "disaster", selected = "Flood")
    updateSelectInput(session, "contr", selected = "China")
    updateSelectInput(session, "year", selected = '2012-2021')
  })
}

shinyApp(ui = ui, server = server)
