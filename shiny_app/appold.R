library(shiny)
library(tidyverse)
library(ggdist)
library(ggthemes)
library(tidyquant)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Simulation Study Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Show/hide based on active tab
      conditionalPanel(
        condition = "input.tabs == 'Plot'",
        h4("Data Filters"),
        
        selectInput("plot_type", "Select Plot Type:",
                    choices = c(
                      "MAPD - Unconstrained" = "mapd_u",
                      "MAPD - Complement" = "mapd_c",
                      "MSD - Unconstrained" = "msd_u",
                      "MSD - Complement" = "msd_c",
                      "Distribution - BESc" = "dist_besc",
                      "Distribution - PMP1c_H" = "dist_pmp1c",
                      "Distribution - BESu" = "dist_besu",
                      "Distribution - PMP1u_H" = "dist_pmp1u"
                    ),
                    selected = "mapd_u"),
        
        checkboxGroupInput("d_values", "Effect Size (d):",
                           choices = list("d = 0.2" = "0.2",
                                          "d = 0.5" = "0.5",
                                          "d = 0.8" = "0.8"),
                           selected = c("0.2", "0.5", "0.8")),
        
        checkboxGroupInput("c_values", "Correlation (c):",
                           choices = list("c = 0.0" = "0.0", 
                                          "c = 0.2" = "0.2", 
                                          "c = 0.5" = "0.5",
                                          "c = 0.8" = "0.8"),
                           selected = c("0.0", "0.2", "0.5", "0.8")),
        
        conditionalPanel(
          condition = "!input.plot_type.startsWith('dist_')",
          checkboxGroupInput("n_sample_values", "Sample Size:",
                             choices = NULL,
                             selected = NULL)
        ),
        
        conditionalPanel(
          condition = "input.plot_type.startsWith('dist_')",
          checkboxGroupInput("n_sample_dist", "Sample Size (for distributions):",
                             choices = NULL,
                             selected = NULL),
          
          selectInput("d_single", "Effect Size (d):",
                      choices = NULL,
                      selected = NULL),
          
          selectInput("c_single", "Correlation (c):",
                      choices = NULL,
                      selected = NULL)
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Summary Statistics'",
        h4("Summary Filters"),
        
        checkboxGroupInput("summary_d", "Effect Size (d):",
                           choices = list("d = 0.2" = "0.2",
                                          "d = 0.5" = "0.5",
                                          "d = 0.8" = "0.8"),
                           selected = c("0.2", "0.5", "0.8")),
        
        checkboxGroupInput("summary_c", "Correlation (c):",
                           choices = list("c = 0.0" = "0.0", 
                                          "c = 0.2" = "0.2", 
                                          "c = 0.5" = "0.5",
                                          "c = 0.8" = "0.8"),
                           selected = c("0.0", "0.2", "0.5", "0.8")),
        
        checkboxGroupInput("summary_n", "Sample Size:",
                           choices = NULL,
                           selected = NULL),
        
        checkboxGroupInput("summary_metrics", "Metrics:",
                           choices = list(
                             "PMP1u_H" = "PMP1u_H",
                             "PMP1c_H" = "PMP1c_H",
                             "BESc" = "BESc",
                             "BESu" = "BESu",
                             "MAPDu" = "MAPDu",
                             "MAPDc" = "MAPDc",
                             "MSDu" = "MSDu",
                             "MSDc" = "MSDc"
                           ),
                           selected = c("PMP1u_H", "PMP1c_H", "BESc", "BESu", 
                                        "MAPDu", "MAPDc", "MSDu", "MSDc"))
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Data Table'",
        h4("Table Filters"),
        
        checkboxGroupInput("table_d", "Effect Size (d):",
                           choices = list("d = 0.2" = "0.2",
                                          "d = 0.5" = "0.5",
                                          "d = 0.8" = "0.8"),
                           selected = c("0.2", "0.5", "0.8")),
        
        checkboxGroupInput("table_c", "Correlation (c):",
                           choices = list("c = 0.0" = "0.0", 
                                          "c = 0.2" = "0.2", 
                                          "c = 0.5" = "0.5",
                                          "c = 0.8" = "0.8"),
                           selected = c("0.0", "0.2", "0.5", "0.8")),
        
        checkboxGroupInput("table_n", "Sample Size:",
                           choices = NULL,
                           selected = NULL)
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        tabPanel("Plot",
                 plotOutput("main_plot", height = "600px"),
                 br(),
                 textOutput("plot_description")
        ),
        
        tabPanel("Summary Statistics",
                 h4("Summary Statistics and Detailed Data"),
                 p("Combined table showing summary statistics (min, max, mean, median, SD) and aggregated means in long format."),
                 DTOutput("summary_table")
        ),
        
        tabPanel("Data Table",
                 h4("Full Results Data"),
                 p("Showing individual simulation results (not aggregated)."),
                 DTOutput("data_table")
        ),
        
        tabPanel("Help",
                 h3("App Guide"),
                 tags$ul(
                   tags$li(strong("Plot Types:"), "Choose between MAPD (Mean Average Percentage Difference) and MSD (Mean Signed Difference) for both unconstrained and complement hypotheses, or view distribution plots."),
                   tags$li(strong("Filters:"), "Select specific effect sizes (d) and correlations (c) to focus your analysis."),
                   tags$li(strong("Distribution Plots:"), "For distribution plots, select specific sample sizes and a single d/c combination."),
                   tags$li(strong("Summary Statistics:"), "View min, max, mean, and other statistics for filtered scenarios.")
                 ),
                 br(),
                 h4("Metrics Explained:"),
                 tags$ul(
                   tags$li(strong("MAPD:"), "Mean Average Percentage Difference = ||(PMP - BES) / PMP|| × 100"),
                   tags$li(strong("MSD:"), "Mean Signed Difference = PMP - BES"),
                   tags$li(strong("BES:"), "Bayes Equality Score"),
                   tags$li(strong("PMP:"), "Posterior Model Probability")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load and prepare data 
  load_data <- reactive({
    # Load your data here
    load("../data/Sim_1.RData")
    
    # Data preparation as in your code
    Sim_1$BESc <- ifelse(is.nan(Sim_1$BESc), 1, Sim_1$BESc)
    Sim_1$n_sample <- Sim_1$n_sample * 3
    Sim_1$ind_p_all <- ifelse(Sim_1$ind_p1 == 1 & Sim_1$ind_p2 == 1 & Sim_1$ind_p3 == 1, 1, 0)
    
    Sim_1 <- Sim_1 %>%
      group_by(n_sample, d, c) %>%
      mutate(
        MAPDu = abs((PMP1u_H - BESu) / PMP1u_H) * 100,
        MSDu = PMP1u_H - BESu,
        MAPDc = abs((PMP1c_H - BESc) / PMP1c_H) * 100,
        MSDc = PMP1c_H - BESc
      )
    
    Sim_1$MAPDc <- ifelse(is.nan(Sim_1$MAPDc), 0, Sim_1$MAPDc)
    
    Sim_1
  })
  
  # Aggregated data
  agg_data <- reactive({
    load_data() %>%
      group_by(n_sample, c, d) %>%
      summarise(
        PMP1u_H = mean(PMP1u_H),
        PMP1c_H = mean(PMP1c_H),
        BESc = mean(BESc),
        BESu = mean(BESu),
        MAPDu = mean(MAPDu),
        MAPDc = mean(MAPDc),
        MSDu = mean(MSDu),
        MSDc = mean(MSDc),
        .groups = "drop"
      )
  })
  
  # Update choices based on actual data
  observe({
    data <- agg_data()
    
    # Get unique values
    unique_d <- sort(unique(data$d))
    unique_c <- sort(unique(data$c))
    unique_n <- sort(unique(data$n_sample))
    
    # Update sample size choices for line plots
    updateCheckboxGroupInput(session, "n_sample_values",
                             choices = setNames(as.character(unique_n), 
                                                paste("n =", unique_n)),
                             selected = as.character(unique_n))
    
    # Update sample size choices for distribution plots
    updateCheckboxGroupInput(session, "n_sample_dist",
                             choices = setNames(as.character(unique_n), 
                                                paste("n =", unique_n)),
                             selected = as.character(unique_n))
    
    # Update single selection dropdowns for distribution plots
    updateSelectInput(session, "d_single",
                      choices = setNames(as.character(unique_d), 
                                         paste("d =", unique_d)),
                      selected = as.character(unique_d[1]))
    
    updateSelectInput(session, "c_single",
                      choices = setNames(as.character(unique_c), 
                                         paste("c =", unique_c)),
                      selected = as.character(unique_c[1]))
    
    # Update summary dropdowns
    updateCheckboxGroupInput(session, "summary_n",
                             choices = setNames(as.character(unique_n), 
                                                paste("n =", unique_n)),
                             selected = as.character(unique_n))
    
    # Update data table dropdowns
    updateCheckboxGroupInput(session, "table_n",
                             choices = setNames(as.character(unique_n), 
                                                paste("n =", unique_n)),
                             selected = as.character(unique_n))
  })
  
  # Filtered aggregated data
  filtered_agg_data <- reactive({
    req(input$d_values, input$c_values)
    data <- agg_data()
    
    filtered <- data %>%
      filter(
        d %in% as.numeric(input$d_values),
        c %in% as.numeric(input$c_values),
        d != 0
      )
    
    # Filter by sample size for line plots
    if (!startsWith(input$plot_type, "dist_") && !is.null(input$n_sample_values)) {
      filtered <- filtered %>%
        filter(n_sample %in% as.numeric(input$n_sample_values))
    }
    
    filtered
  })
  
  # Filtered data for summary statistics
  filtered_summary_data <- reactive({
    req(input$summary_d, input$summary_c, input$summary_n)
    
    agg_data() %>%
      filter(
        d %in% as.numeric(input$summary_d),
        c %in% as.numeric(input$summary_c),
        n_sample %in% as.numeric(input$summary_n),
        d != 0
      )
  })
  
  # Filtered data for data table (full results, not aggregated)
  filtered_table_data <- reactive({
    req(input$table_d, input$table_c, input$table_n)
    
    load_data() %>%
      filter(
        d %in% as.numeric(input$table_d),
        c %in% as.numeric(input$table_c),
        n_sample %in% as.numeric(input$table_n),
        d != 0
      )
  })
  
  # Main plot
  output$main_plot <- renderPlot({
    req(filtered_agg_data())
    
    if (startsWith(input$plot_type, "dist_")) {
      # Distribution plots
      data <- load_data() %>%
        filter(
          n_sample %in% as.numeric(input$n_sample_dist),
          c == as.numeric(input$c_single),
          d == as.numeric(input$d_single)
        )
      
      if (input$plot_type == "dist_besc") {
        ggplot(data, aes(y = factor(n_sample), x = BESc, fill = factor(ind_comp))) +
          geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.5) +
          stat_dots(side = "right", justification = 1.2, alpha = 0.2) +
          scale_fill_tq() +
          theme_tq() +
          labs(title = "BESc Distribution - Complement Hypothesis",
               x = "BESc", y = "", fill = "Indicator") +
          facet_wrap(~ n_sample, scales = "free") +
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
        
      } else if (input$plot_type == "dist_pmp1c") {
        ggplot(data, aes(y = factor(n_sample), x = PMP1c_H, fill = factor(ind_p_all))) +
          geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.5) +
          stat_dots(side = "right", justification = 1.2, alpha = 0.2) +
          scale_fill_tq() +
          theme_tq() +
          labs(title = "PMP1c_H Distribution - Complement Hypothesis",
               x = "PMP1c_H", y = "", fill = "Indicator") +
          facet_wrap(~ n_sample, scales = "free") +
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
        
      } else if (input$plot_type == "dist_besu") {
        ggplot(data, aes(y = factor(n_sample), x = BESu, fill = factor(ind_comp))) +
          geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.5) +
          stat_dots(side = "right", justification = 1.2, alpha = 0.2) +
          scale_fill_tq() +
          theme_tq() +
          labs(title = "BESu Distribution - Unconstrained Hypothesis",
               x = "BESu", y = "", fill = "Indicator") +
          facet_wrap(~ n_sample, scales = "free") +
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
        
      } else if (input$plot_type == "dist_pmp1u") {
        ggplot(data, aes(y = factor(n_sample), x = PMP1u_H, fill = factor(ind_p_all))) +
          geom_boxplot(width = 0.12, outlier.color = NA, alpha = 0.5) +
          stat_dots(side = "right", justification = 1.2, alpha = 0.2) +
          scale_fill_tq() +
          theme_tq() +
          labs(title = "PMP1u_H Distribution - Unconstrained Hypothesis",
               x = "PMP1u_H", y = "", fill = "Indicator") +
          facet_wrap(~ n_sample, scales = "free") +
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
      }
      
    } else {
      # Line plots
      data <- filtered_agg_data()
      
      plot_config <- list(
        mapd_u = list(y = "MAPDu", title = "MAPD: Tested against unconstrained Hypothesis"),
        mapd_c = list(y = "MAPDc", title = "MAPD: Tested against complement Hypothesis"),
        msd_u = list(y = "MSDu", title = "MSD: Tested against unconstrained Hypothesis"),
        msd_c = list(y = "MSDc", title = "MSD: Tested against complement Hypothesis")
      )
      
      config <- plot_config[[input$plot_type]]
      
      ggplot(data) +
        geom_line(aes(x = as.factor(n_sample), y = .data[[config$y]], 
                      color = as.factor(d), group = 1)) +
        labs(x = "Sample Size (Total)", y = config$y) +
        ggtitle(config$title) +
        scale_color_discrete(guide = "none") +
        theme_bw() +
        facet_grid(c ~ d, labeller = label_both) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Plot description
  output$plot_description <- renderText({
    descriptions <- list(
      mapd_u = "Mean Average Percentage Difference between PMP and BES for unconstrained hypothesis across sample sizes.",
      mapd_c = "Mean Average Percentage Difference between PMP and BES for complement hypothesis across sample sizes.",
      msd_u = "Mean Signed Difference (PMP - BES) for unconstrained hypothesis across sample sizes.",
      msd_c = "Mean Signed Difference (PMP - BES) for complement hypothesis across sample sizes.",
      dist_besc = "Distribution of BES values for complement hypothesis by sample size.",
      dist_pmp1c = "Distribution of PMP values for complement hypothesis by sample size.",
      dist_besu = "Distribution of BES values for unconstrained hypothesis by sample size.",
      dist_pmp1u = "Distribution of PMP values for unconstrained hypothesis by sample size."
    )
    
    descriptions[[input$plot_type]]
  })
  
  # Summary statistics - removed the text output
  
  # Summary table - metrics in long format, statistics in wide format
  output$summary_table <- renderDT({
    req(input$summary_d, input$summary_c, input$summary_n, input$summary_metrics)
    
    # Use the full raw data for summary statistics
    raw_data <- load_data() %>%
      filter(
        d %in% as.numeric(input$summary_d),
        c %in% as.numeric(input$summary_c),
        n_sample %in% as.numeric(input$summary_n),
        d != 0
      )
    
    # Calculate summary statistics for each combination
    summary_data <- raw_data %>%
      group_by(n_sample, d, c) %>%
      summarise(
        across(all_of(input$summary_metrics),
               list(
                 min = ~min(., na.rm = TRUE),
                 max = ~max(., na.rm = TRUE),
                 mean = ~mean(., na.rm = TRUE),
                 median = ~median(., na.rm = TRUE),
                 sd = ~sd(., na.rm = TRUE)
               ),
               .names = "{.col}_{.fn}"),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = -c(n_sample, d, c),
        names_to = c("metric", "statistic"),
        names_sep = "_(?=[^_]+$)",
        values_to = "value"
      ) %>%
      pivot_wider(
        names_from = statistic,
        values_from = value
      )
    
    summary_data %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE) %>%
      formatRound(columns = c("min", "max", "mean", "median", "sd"), digits = 4)
  })
  
  # Data table
  output$data_table <- renderDT({
    data <- filtered_table_data()
    
    # Select relevant columns to display
    data %>%
      select(nsim, n_sample, d, c, PMP1u_H, PMP1c_H, BESc, BESu, 
             MAPDu, MAPDc, MSDu, MSDc, 
             ind_p1, ind_p2, ind_p3, ind_p_all, ind_comp) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE) %>%
      formatRound(columns = c("PMP1u_H", "PMP1c_H", "BESc", "BESu", 
                              "MAPDu", "MAPDc", "MSDu", "MSDc"), digits = 4)
  })
}

# Run the app
shinyApp(ui = ui, server = server)