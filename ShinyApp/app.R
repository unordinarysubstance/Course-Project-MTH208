library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(RColorBrewer)
library(shinythemes)
library(imager)

# Load data (Set your working directory)

global_data <- read.csv("global_data.csv")
GDP <- get(load('Data/GDP.Rdata'))
Rankings <- get(load('Data/QS_Rankings.Rdata'))
Ratio <- get(load('Data/Times_Rankings.Rdata'))
Rankings$Country = replace(Rankings$Country, Rankings$Country == "China (Mainland)", "China")
Rankings$Country = replace(Rankings$Country, Rankings$Country == "Hong Kong SAR", "Hong Kong")
Rankings$Country = replace(Rankings$Country, Rankings$Country == "Macau SAR", "Macao")
names(GDP)[1] = "Country"



# UI Definition
ui <- navbarPage(theme = shinytheme("cyborg"),
                 title = "Global Rankings of Universities Analysis Dashboard",
                 
                 # Panel 1: Colored Regions
                 tabPanel("Country wise Number of Universities Graph",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("region", "Select Region:", 
                                          choices = c("All", sort(unique(global_data$Region)) ) )
                            ),
                            mainPanel(
                              plotOutput("regionPlot",height = "900px" , width = "1000px")
                            )
                          )
                 ),
                 
                 # Panel 2: Compare
                 tabPanel("Compare Universities",
                          fluidRow(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  "university1",
                                  "Select University 1:",
                                  choices = global_data$Title %>% unique(),
                                  selected = global_data$Title[1]  # Pre-select the first university
                                ),
                                selectInput(
                                  "university2",
                                  "Select University 2:",
                                  choices = global_data$Title %>% unique(),
                                  selected = global_data$Title[2]  # Pre-select the first university
                                )
                              ),
                              mainPanel(
                                plotOutput("university_images", height = 200 , width = 700),
                                tableOutput("comparison")
                              )
                            ),
                            h3("Global Universities Database"),
                            DTOutput("data_table")
                          ),
                          hr()
                 ),
                 
                 # Panel 3: Variable Analysis
                 tabPanel("Variable Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var1", "Select First Variable:", NULL),
                              selectInput("var2", "Select Second Variable:", NULL),
                              radioButtons("plot_type", "Select Plot Type:",
                                           choices = c("Scatter Plot" = "scatter",
                                                       "Box Plot" = "box"))
                            ),
                            mainPanel(
                              plotOutput("var_plot", height = "600px")
                            )
                          )
                 ),
                 
                 
                 
                 # Panel 4: Multiple Feature Linear Plots
                 tabPanel("Linear Plots for Top 100 Universities",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("selected_features", "Select Features (Max 3):",NULL)
                            ),
                            mainPanel(
                              plotOutput("linear_plot", height = "800px")
                            )
                          )
                 ),
                 # Panel 5: GDP vs Population
                 tabPanel("University Ranking vs. GDP per Capita Plot",
                          mainPanel(
                            plotOutput("gdpPlot",height = "500px" , width = "1000px")
                          )
                          
                 )
                 
                 
)

# Server Definition
server <- function(input, output, session) {
  # Load data
  data <- reactive({
    read.csv("global_data.csv")
  })
  
  # Update input choices based on data
  observe({
    req(data())
    numeric_cols <- names(select_if(data(), is.numeric))
    updateSelectInput(session, "var1", choices = numeric_cols)
    updateSelectInput(session, "var2", choices = numeric_cols)
    updateSelectInput(session, "x_var", choices = numeric_cols)
    updateSelectInput(session, "y_var", choices = numeric_cols)
    updateSelectInput(session, "color_var", choices = names(data()))
    updateCheckboxGroupInput(session, "selected_features", 
                             choices = numeric_cols,
                             selected = numeric_cols[1:min(8, length(numeric_cols))])
  })
  
  
  # Panel 1: RegionPlot
  output$regionPlot <- renderPlot({
    
    
    # Calculate the number of universities for each country in Rankings
    country_counts <- Rankings %>%
      count(Country, sort = TRUE) %>%
      rename(Count = n)
    
    # Add the Region column by joining with Rankings
    country_counts <- country_counts %>%
      left_join(Rankings %>% select(Country, Region) %>% distinct(Country, Region), by = "Country")
    
    
    if (input$region == "All") {
      # Plot for all regions
      plot <- ggplot(country_counts, aes(y = reorder(Country, Count), x = Count, fill = Region)) +
        geom_bar(stat = "identity") +
        labs(y = "Country", x = "Number of Universities", 
             title = "Number of Universities by Country (All Regions)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        scale_fill_brewer(palette = "Set2")
    } else {
      # Plot for a specific region
      region_data <- country_counts %>%
        filter(Region == input$region)
      
      plot <- ggplot(region_data, aes(y = reorder(Country, Count), x = Count, fill = Region)) +
        geom_bar(stat = "identity") +
        labs(y = "Country", x = "Number of Universities", 
             title = paste("Number of Universities by Country in", input$region)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
              plot.title = element_text(face = "bold", size = 20)) +
        scale_fill_brewer(palette = "Set2")
    }
    plot
  })
  
  # Panel 2
  #  selected universities
  university_data <- reactive({
    global_data %>%
      filter(Title %in% c(input$university1, input$university2))
  })
  
  # Create comparison table
  output$comparison <- renderTable({
    university_data() %>%
      select(-Title , -ImageLink) %>%
      arrange(rownames(.)) %>%
      t %>%
      as.data.frame() %>%
      mutate(Variable = rownames(.)) %>%
      select(Variable, everything()) %>%
      rename(!!input$university1 := 2,
             !!input$university2 := 3)
  })
  
  # Plot logos for each university in a single row
  output$university_images <- renderPlot({
    par(mfrow = c(1, 2))
    
    link1 <- university_data() %>%
      filter(Title == input$university1) %>%
      pull(ImageLink)
    img1 <- load.image(link1)
    plot(img1, main = input$university1, axes = FALSE )
    
    link2 <- university_data() %>%
      filter(Title == input$university2) %>%
      pull(ImageLink)
    img2 <- load.image(link2)
    plot(img2, main = input$university2,axes = FALSE )
  })
  
  output$university2_image <- renderPlot({
    link <- university_data() %>%
      filter(Title == input$university2) %>%
      pull(ImageLink)
    img <- load.image(link)
    plot(img)
  })
  
  # Panel 4 GDP vs Population
  output$gdpPlot <- renderPlot({
    Rankings$Rank <- as.numeric(Rankings$Rank)
    # Step 1: Calculate the average ranking for each country
    avg_ranking <- Rankings %>%
      group_by(Country) %>%
      summarise(Avg_Ranking = mean(Rank, na.rm = TRUE))
    
    # Step 2: Join the average ranking data with GDP data
    # Ensure GDP dataset has necessary columns: Country, GDP_per_Capita, Population
    data_merged <- avg_ranking %>%
      left_join(GDP, by = "Country")
    
    # Step 3: Plot Average Ranking vs. GDP per capita, colored by Population
    plot <- ggplot(data_merged, aes(y = data_merged$`GDP per Capita (in $)`, x = Avg_Ranking, color = data_merged$Population)) +
      geom_point(size = 6) +
      labs(y = "GDP per Capita", x = "Average University Ranking",
           title = "Average University Ranking vs. GDP per Capita",
           color = "Population") +
      scale_color_gradient(low = "lightblue", high = "darkblue") +  # Light to dark color gradient based on population
      theme_minimal() + theme(
        axis.title.x = element_text(face = "bold", size = 18),  # Bold x-axis label
        axis.title.y = element_text(face = "bold", size = 18), # Bold y-axis label
        plot.title = element_text(face = "bold", size = 20)
        
      )
    plot
  })
  
  
  # Panel 2: Analysis Summary
  output$analysis_summary <- renderPrint({
    req(data(), input$var1, input$var2)
    cat("Correlation Analysis:\n")
    cor_test <- cor.test(data()[[input$var1]], data()[[input$var2]])
    print(cor_test)
  })
  
  # Panel 2: Data Information
  output$data_info <- renderPrint({
    req(data())
    cat("Dataset Dimensions:", dim(data()), "\n\n")
    cat("Variable Types:\n")
    print(sapply(data(), class))
  })
  
  # Panel 2: Variable Statistics
  output$var_stats <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Panel 2: Data Table
  output$data_table <- renderDT({
    req(data())
    datatable(data(), 
              options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Panel 3: Variable Analysis Plot
  output$var_plot <- renderPlot({
    req(data(), input$var1, input$var2)
    
    df <- data()
    
    switch(input$plot_type,
           "scatter" = {
             ggplot(df, aes_string(x = input$var1, y = input$var2, color = "Region") ) +
               geom_point(size = 5 , alpha = 0.6) +
               theme_minimal() +
               labs(title = paste("Scatter Plot:", input$var1, "vs", input$var2)) + 
               scale_fill_brewer(palette = "Set1")
           },
           "box" = {
             df_long <- df %>%
               select(all_of(c(input$var1, input$var2))) %>%
               gather(key = "variable", value = "value")
             
             ggplot(df_long, aes(x = variable, y = value)) +
               geom_boxplot(fill = "steelblue", alpha = 0.7) +
               theme_minimal() +
               labs(title = "Box Plot Comparison")
           })
  })
  
  
  
  # Panel 5: Linear Plots
  output$linear_plot <- renderPlot({
    req(data(), input$selected_features)
    global_data <- 
      filtered_data <- global_data %>% head(100)
    
    # Limit to 3 features
    selected_features <- input$selected_features[1:min(3,length(input$selected_features))]
    
    # Prepare data for plotting
    df_long <- filtered_data %>%
      select(all_of(selected_features)) %>%
      mutate(index = 1:n()) %>%
      gather(key = "variable", value = "value", -index)
    
    # Create plot with different colors for each feature
    ggplot(df_long, aes(x = index, y = value, color = variable)) +
      geom_line(size = 1, alpha = 0.9 , linetype = "dotted") +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      labs(title = "Feature Linear Plot",
           x = "Index",
           y = "Value") +
      theme(legend.position = "right")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
