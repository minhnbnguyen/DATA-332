library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(DT)
library(scales)
#install.packages(c("maps", "mapproj", "viridis"))

# Read and clean data
dataset <- readRDS("cleaned_nursing.rds")

# Define UI
ui <- navbarPage("Nursing Home Industry Research",
                 tabPanel("Industry Overview",
                          fluidPage(
                            # Financial KPI table at the top
                            fluidRow(
                              column(12,
                                     h4("Industry KPI"),
                                     tableOutput("industry_table")
                              )
                            ),
                            
                            # State filter below the KPI table
                            fluidRow(
                              column(12,
                                     wellPanel(
                                       selectInput("selected_state", "State:", 
                                                   choices = dataset %>%
                                                     filter(!is.na(state) & state != "NA") %>%
                                                     pull(state) %>%
                                                     as.character() %>%
                                                     unique() %>%
                                                     sort(),
                                                   selected = dataset %>%
                                                     filter(!is.na(state) & state != "NA") %>%
                                                     pull(state) %>%
                                                     as.character() %>%
                                                     .[1])
                                     )
                              )
                            ),
                            
                            # Income growth rate and income structure side by side
                            fluidRow(
                              column(6,
                                     h4("Occupancy Rate by Ownership Type"),
                                     plotOutput("occupancy_ownership", height = "300px")
                              ),
                              column(6,
                                     h4("Rating by Ownership Type"),
                                     plotOutput("rating_ownership", height = "300px")
                              )
                            ),
                            
                            # Revenue vs cost and analytics insights side by side
                            fluidRow(
                              column(6,
                                     h4("Research Explanation"),
                                     div(
                                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                       textOutput("research_explanation")
                                     )                              ),
                              column(6,
                                     h4("Industry Overview Graphs Explanations"),
                                     div(
                                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                       textOutput("industry_comments")
                                     )
                              )
                            )
                          )
                 ),
                 tabPanel("Financial Performance",
                          fluidPage(
                            # Financial KPI table at the top
                            fluidRow(
                              column(12,
                                     h4("Financial KPI"),
                                     tableOutput("financial_table")
                              )
                            ),
                            
                            # State filter below the KPI table
                            fluidRow(
                              column(12,
                                     wellPanel(
                                       selectInput("selected_state", "State:", 
                                                   choices = unique(as.character(dataset$state)), 
                                                   selected = as.character(dataset$state[1]))
                                     )
                              )
                            ),
                            
                            # Income growth rate and income structure side by side
                            fluidRow(
                              column(6,
                                     h4("Income Growth Rate"),
                                     plotOutput("income_growth", height = "300px")
                              ),
                              column(6,
                                     h4("Total Income vs. Operations Income"),
                                     plotOutput("income_structure", height = "300px")
                              )
                            ),
                            
                            # Revenue vs cost and analytics insights side by side
                            fluidRow(
                              column(6,
                                     h4("Revenue vs. Cost"),
                                     plotOutput("revenue_vs_cost", height = "300px")
                              ),
                              column(6,
                                     h4("Financial Graphs Explanations"),
                                     div(
                                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                       textOutput("financial_comments")
                                     )
                              )
                            )
                          )
                 ),
                 tabPanel("Geometry Info",
                          fluidPage(
                            fluidRow(
                              column(6,
                                     h4("Number of Providers by State"),
                                     plotOutput("provider_state", height = "300px")
                              ),
                              column(6,
                                     h4("Net Income by State"),
                                     plotOutput("net_income_state", height = "300px")
                              )
                            ),
                            fluidRow(
                                     h4("Geometry Graphs Explanations"),
                                     div(
                                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                       textOutput("geometry_comments")
                                     )
                              )
                          )
                 ),
                 tabPanel("Predicted Income",
                          fluidPage(
                            fluidRow(
                              column(6,
                                     h4("Net Income Forecasting"),
                                     plotOutput("net_income_forecast", height = "300px")
                              )
                            ),
                            fluidRow(
                                     h4("Forecasting Explanations"),
                                     div(
                                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 300px; overflow-y: auto;",
                                       textOutput("forecasting_comments")
                                     )
                              )
                          )
                 )
)
# Server
server <- function(input, output, tab) {
  
  # First Tab: Industry Overview
  filtered_year <- reactive({
    dataset %>% filter(as.numeric(year) == input$selected_year)
  })

  output$industry_table <- renderTable({
    filtered_year() %>%
      mutate(
        restot = as.numeric(restot),
        tot_penlty_cnt = as.numeric(tot_penlty_cnt),
        total_days_total_annualized = as.numeric(total_days_total_annualized),
        total_bed_days_available_annualized = as.numeric(total_bed_days_available_annualized),
        occupancy_rate = total_days_total_annualized/total_bed_days_available_annualized * 100,
        snf_admissions_total_annualized = as.numeric(snf_admissions_total_annualized),
        admission_rate = 365*(snf_admissions_total_annualized/total_bed_days_available_annualized*100)
      ) %>%
      summarise(
        Total_Providers = comma(as.integer(n_distinct(provnum, na.rm = TRUE))),
        Residents_per_Day = comma(as.integer(sum(restot, na.rm = TRUE))),
        Total_Penalty = comma(as.integer(sum(tot_penlty_cnt, na.rm = TRUE))),
        Occupancy_Rate = paste0(round(mean(occupancy_rate, na.rm = TRUE), 2), "%"),
        Admission_Rate = paste0(round(mean(admission_rate, na.rm = TRUE), 2), "%")
      )
  })
  
  output$occupancy_ownership <- renderPlot({
    filtered_data() %>%
      mutate(
        total_days_total_annualized = as.numeric(total_days_total_annualized),
        total_bed_days_available_annualized = as.numeric(total_bed_days_available_annualized),
        occupancy_rate = total_days_total_annualized/total_bed_days_available_annualized * 100
      ) %>%
      group_by(ownership) %>%  # Make sure to group by the ownership column
      summarise(
        Occupancy_Rate = mean(occupancy_rate, na.rm = TRUE)
      ) %>%
      ggplot(aes(x = ownership, y = Occupancy_Rate)) +
      geom_bar(stat = 'identity') +
      coord_flip() + 
      labs(
        title = NULL,
        x = NULL,
        y = "Occupancy Rate (%)"
      ) +
      theme_minimal() +
      # Format y-axis as percentage
      scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
      theme(legend.position = "none")  # Remove redundant legend
  })
  
  output$rating_ownership <- renderPlot({
    filtered_data() %>%
      mutate(
        overall_rating = as.numeric(overall_rating)
      ) %>%
      group_by(ownership) %>%  # Make sure to group by the ownership column
      summarise(
        avg_rating = mean(overall_rating, na.rm = TRUE)
      ) %>%
      ggplot(aes(x = ownership, y = avg_rating)) +
      geom_bar(stat = 'identity') +
      coord_flip() + 
      labs(
        title = NULL,
        x = NULL,
        y = "Average Rating (scale from 1-5)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # Remove redundant legend
  })
      
  output$industry_comments <- renderText({
    "Example - You have within your visualizations to do a Geo Spatial application with car crash data.
    I need to know why you did not do this other than you couldn't or did not know how.
    That will get you points deducted.
  
  Note: This analysis is based on historical data and specific state selection."
  })
  
  output$research_explanation <- renderText({
    "Example - You have within your visualizations to do a Geo Spatial application with car crash data.
    I need to know why you did not do this other than you couldn't or did not know how.
    That will get you points deducted.
  
  Note: This analysis is based on historical data and specific state selection."
  })
    # Tab 2: Financial Overview
    
  filtered_data <- reactive({
    dataset %>% 
      filter(as.character(state) == input$selected_state)
  })
    
  output$revenue_vs_cost <- renderPlot({
    # Reshape the data from wide to long format
    long_data <- filtered_data() %>%
      select(year, gross_revenue_annualized, less_total_operating_expense_annualized) %>%
      rename(Revenue = gross_revenue_annualized, 
             Expense = less_total_operating_expense_annualized) %>%
      pivot_longer(
        cols = c(Revenue, Expense),
        names_to = "Category",
        values_to = "Amount"
      )
    
    # Create the clustered bar chart
    ggplot(long_data, aes(x = factor(year), y = Amount, fill = Category)) +
      geom_bar(position = "dodge", stat = "identity") +
      labs(
        title = NULL,
        x = "Year",
        y = "Amount ($)",
        fill = ""
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("Revenue" = "green", "Expense" = "red"))
  })
  
  output$income_structure <- renderPlot({
    aggregated_data <- filtered_data() %>%
      group_by(year) %>%
      summarize(
        total_income_annualized = sum(total_income_annualized, na.rm = TRUE),
        net_income_from_patients_annualized = sum(net_income_from_patients_annualized, na.rm = TRUE)
      ) %>%
      ungroup()
    # Reshape the data from wide to long format
    long_data <- aggregated_data %>%
      select(year, total_income_annualized, net_income_from_patients_annualized) %>%
      rename(Total_Income = total_income_annualized, 
             Operating_Income = net_income_from_patients_annualized
      ) %>%
      pivot_longer(
        cols = c("Total_Income", "Operating_Income"),
        names_to = "Income_Type",
        values_to = "Amount"
      )
    
    long_data$year <- as.numeric(as.character(long_data$year))
    long_data <- long_data %>% filter(!is.na(Amount), !is.na(year))
    
    # Create the dual line graph
    ggplot(long_data, aes(x = year, y = Amount, color = Income_Type, group = Income_Type)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      labs(
        title = NULL,
        x = "Year",
        y = "Amount ($)",
        color = ""
      ) +
      theme_minimal() +
      scale_color_manual(values = c(Total_Income = "blue", Operating_Income = "orange")) +
      scale_x_continuous(breaks = unique(long_data$year)) # Ensure all years are shown on x-axis
  })
  
  output$income_growth <- renderPlot({
    # Store the filtered data for inspection
    filtered <- filtered_data()
    
    # Calculate average total income by year with diagnostic output
    yearly_income <- filtered %>%
      mutate(year = as.numeric(year)) %>%
      mutate(total_income_annualized = as.numeric(total_income_annualized)) %>%
      group_by(year) %>%
      summarize(
        avg_income = mean(total_income_annualized, na.rm = TRUE),
        count = n()) %>%
      arrange(year)
    
    yearly_income <- yearly_income %>%
      filter(!is.na(avg_income), !is.na(year), is.finite(avg_income))
    
    # Use a different approach - calculate growth rates manually
    years <- yearly_income$year
    incomes <- yearly_income$avg_income
    
    # Create empty dataframe for growth rates
    growth_data <- data.frame(
      year = numeric(),
      avg_income = numeric(),
      prev_year_income = numeric(),
      growth_rate = numeric(),
      count = numeric()
    )
    
    # Manually calculate growth rates
    for(i in 2:length(years)) {
      current_year <- years[i]
      current_income <- incomes[i]
      prev_income <- incomes[i-1]
      count <- yearly_income$count[i]
      
      # Calculate growth rate
      growth <- ((current_income - prev_income) / prev_income) * 100
      
      # Add to growth_data
      growth_data <- rbind(growth_data, data.frame(
        year = current_year,
        avg_income = current_income,
        prev_year_income = prev_income,
        growth_rate = growth,
        count = count
      ))
    }
    
    # Create plot with manually calculated growth data
    ggplot(growth_data, aes(x = year, y = growth_rate, group = 1)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = NULL,
        x = "Year",
        y = "Growth Rate (%)"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = unique(growth_data$year)) +
      scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"))
  })
    
  output$financial_table <- renderTable({
    filtered_data() %>%
      mutate(
        total_income_annualized = as.numeric(total_income_annualized),
        gross_revenue_annualized = as.numeric(gross_revenue_annualized),
        net_income_from_patients_annualized = as.numeric(net_income_from_patients_annualized),
        net_profit_margin = total_income_annualized/gross_revenue_annualized * 100,
        net_operating_margin = net_income_from_patients_annualized / gross_revenue_annualized * 100,
        return_on_revenue = total_income_annualized / gross_revenue_annualized * 100
      ) %>%
      summarise(
        Gross_Revenue = paste0("$",comma(round(sum(gross_revenue_annualized, na.rm = TRUE),2))),
        Total_Income = paste0("$",comma(round(sum(total_income_annualized, na.rm = TRUE),2))),
        Net_Profit_Margin = paste0(round(mean(net_profit_margin, na.rm = TRUE), 2), "%"),
        Net_Operating_Margin = paste0(round(mean(net_operating_margin, na.rm = TRUE), 2), "%"),
        Return_On_Revenue = paste0(round(mean(return_on_revenue, na.rm = TRUE), 2), "%")
      )
  })

  output$financial_comments <- renderText({
    "Example - You have within your visualizations to do a Geo Spatial application with car crash data.
    I need to know why you did not do this other than you couldn't or did not know how.
    That will get you points deducted.
  
  Note: This analysis is based on historical data and specific state selection."
  })

    # Tab 3: Geometry Info 
  output$provider_state <- renderPlot({
    provider_counts <- dataset %>%
      group_by(state) %>%
      summarize(
        provider_count = n_distinct(provnum),
        .groups = "drop"
      )
    
    states_map <- map_data("state")
    
    state_lookup <- data.frame(
      state_abbr = state.abb,
      state_name = tolower(state.name),
      stringsAsFactors = FALSE
    )
    
    if(nchar(provider_counts$state[1]) == 2) {
      provider_counts <- provider_counts %>%
        left_join(state_lookup, by = c("state" = "state_abbr")) %>%
        rename(region = state_name)  # maps package uses "region" for state names
    } else {
      # If your data already uses full state names
      provider_counts <- provider_counts %>%
        mutate(region = tolower(state))
    }
    
    map_data <- states_map %>%
      left_join(provider_counts, by = "region")
    
    ggplot(map_data, aes(x = long, y = lat, group = group, fill = provider_count)) +
      geom_polygon(color = "white", size = 0.2) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      scale_fill_viridis_c(
        name = "Number of Providers",
        option = "plasma",
        direction = -1,  # Darker colors for higher values
        na.value = "grey90"
      ) +
      labs(
        title = NULL,
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      )
  })
  
  incomeByState <- reactive({
    dataset %>%
      group_by(state) %>%
      summarize(
        total_income = sum(total_income_annualized, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$net_income_state <- renderPlot({
    # Get income by state
    income_by_state <- incomeByState()
    
    # Get US states map data
    states_map <- map_data("state")
    
    # Convert state abbreviations to full names (if needed)
    state_lookup <- data.frame(
      state_abbr = state.abb,
      state_name = tolower(state.name),
      stringsAsFactors = FALSE
    )
    
    # Join income data with state names
    if(nchar(income_by_state$state[1]) == 2) {
      # If your data uses state abbreviations
      income_by_state <- income_by_state %>%
        left_join(state_lookup, by = c("state" = "state_abbr")) %>%
        rename(region = state_name)
    } else {
      # If your data already uses full state names
      income_by_state <- income_by_state %>%
        mutate(region = tolower(state))
    }
    
    # Join the map data with income data
    map_data <- states_map %>%
      left_join(income_by_state, by = "region")
    
    # Create the heatmap
    ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_income)) +
      geom_polygon(color = "white", size = 0.2) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      scale_fill_viridis_c(
        name = "Total Income ($)",
        option = "viridis",
        direction = -1,
        na.value = "grey90",
        labels = scales::dollar_format(scale = 1/1000000, suffix = "M")
      ) +
      labs(
        title = NULL,
        subtitle = "Values in Millions of Dollars", 
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"
      )
  })
  
  
  output$geometry_comments <- renderText({
    "Example - You have within your visualizations to do a Geo Spatial application with car crash data.
    I need to know why you did not do this other than you couldn't or did not know how.
    That will get you points deducted.
  
  Note: This analysis is based on historical data and specific state selection."
  })

  # Tab 4: Forecasting
  output$forecasting_comments <- renderText({
    "Example - You have within your visualizations to do a Geo Spatial application with car crash data.
    I need to know why you did not do this other than you couldn't or did not know how.
    That will get you points deducted.
  
  Note: This analysis is based on historical data and specific state selection."
  })
  
  
  output$predicted_income <- renderPlot({
    # Count trips by weekday
    linear_data <- filtered_data %>% 
      count(week_day) %>%
      # Convert weekday to a factor with levels in correct order (Sun to Sat)
      mutate(
        # First convert to proper weekday order
        weekday_num = as.numeric(week_day)
      )
    
    # Build linear model based on weekday
    model <- lm(n ~ weekday_num, data = linear_data)
    
    # Create prediction plot
    ggplot(linear_data, aes(x = week_day, y = n, group = 1)) +
      geom_point(color = 'blue', size = 3) + 
      geom_smooth(aes(x = weekday_num), method = 'lm', color = 'red') + 
      labs(title = 'Prediction Ride by Weekday using Linear Regression', 
           x = 'Day of Week', 
           y = 'Number of Trips') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the App
shinyApp(ui, server)
