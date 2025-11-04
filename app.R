
## load libraries
library(shiny)
library(shinydashboard)
library(highcharter)
library(viridis)
library(dplyr)
library(stringr)
library(plotly)
library(reshape2)
library(Hmisc)

## load in the data 
my_data <- read.csv("final_data.csv")
attribute_map <- c(
  "Strength"     = "strength",
  "Dexterity"    = "dex",
  "Constitution" = "const",
  "Intelligence" = "intelligence",
  "Wisdom"       = "wisdom",
  "Charisma"     = "charisma"
)

upper_cols <- str_to_title(colnames(my_data))
upper_cols[3:4] <- c("Dexterity", "Constitution")
colnames(my_data) <- upper_cols
my_data$Race <- str_to_title(my_data$Race)

## more colors 
colors <- viridis::magma(100)
dark_red <- colors[15]

## more filters
my_data <- my_data[my_data$Level <= 20, ]

## ui
ui <- dashboardPage(
  dashboardHeader(
    title = "D&D 5'th Edition Characters",
    titleWidth = 280
  ),
  
  skin = "red",
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Character Creation", tabName = "overview", icon = icon("dashboard")),
      menuItem("Character Statistics", tabName = "backgrounds", icon = icon("dashboard"))
    ),
    # Sidebar with checkbox group for races
    checkboxGroupInput(
      inputId = "raceCheckbox",
      label = h3("Select Races"),
      choices = sort(unique(my_data$Race)),
      selected = "Dragonborn"
    ),
    width = 280,
    sliderInput(
      inputId = "levelSlider",
      label = h3("Character Level"),
      min = min(my_data$Level, na.rm = TRUE),
      max = max(my_data$Level, na.rm = TRUE),
      value = c(min(my_data$Level, na.rm = TRUE), max(my_data$Level, na.rm = TRUE)),  # start at full range
      step = 1
    )
  ),
  
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
    fluidRow(
      # Left column: plot box
      column(
        width = 6,
        box(
          title = "Class Distributions",
          width = 12,
          height = 600,
          highchartOutput("classPlot", height = "550px")
        )
      ),
      
      # Right column: two stacked boxes
      column(
        width = 6,
        box(
          title = "Mean Attributes",
          width = 12,
          height = 290,
          
          # --- Dropdown to select attribute ---
          selectInput(
            "attribute",
            label = "Select Attribute",
            choices = c(
              "Charisma" = "Charisma",
              "Strength" = "Strength",
              "Dexterity" = "Dexterity",
              "Constitution" = "Constitution",
              "Intelligence" = "Intelligence",
              "Wisdom" = "Wisdom"
            ),
            selected = "Strength"
          ),
          
          # --- Data table output ---
          DT::dataTableOutput("attr_table", height = "200px")
        ),
        box(
          width = 12,
          height = 290,
          highchartOutput("backgroundPie", height = "250px")
        )
      )
    )
  ),
  
  tabItem(
    tabName = "backgrounds",
    
    fluidRow(
      # Left column
      column(
        width = 6,
        box(
          title = "Attribute Correlation Heatmaps",
          width = 12,
          height = 600,
          plotlyOutput("attrCorrelation")
        )
      ),
      
      # Right column
      column(
        width = 6,
        box(
          title = "Hit Point Distributions",
          width = 12,
          height = 600,
          plotOutput("hpHist")
        )
      )
    )
  )
    )
  )
)

## server
server <- function(input, output) {
  
  # Reactive filtered data based on checkbox selection
  filtered_data <- reactive({
    req(input$raceCheckbox)
    my_data %>% filter(Race %in% input$raceCheckbox) %>%  
      filter(Level >= input$levelSlider[1], Level <= input$levelSlider[2])
  })
  
  ## Make barplot ## 
  output$classPlot <- renderHighchart({
    
    df <- filtered_data() %>%
      group_by(Class) %>%
      dplyr::summarize(Total = n(), .groups = "drop") %>%
      mutate(Proportion = Total / sum(Total)) %>%
      arrange(desc(Proportion))
    
    custom_colors <- viridis::magma(n = nrow(df))
    
    # add the colors into the dataframe
    df <- df %>% mutate(color = custom_colors)
    
    hchart(
      df,
      "column",
      hcaes(x = Class, y = Proportion, color = color)
    ) %>%
      hc_tooltip(useHTML = TRUE,
                 formatter = htmlwidgets::JS(
                   "function() {
                   return '<b>Proportion:</b> ' +
                          Highcharts.numberFormat(this.y * 100, 1) + '%<br>';
                 }"
                 )) %>%
      hc_xAxis(title = list(text = "Class")) %>%
      hc_yAxis(
        title = list(text = "Proportion"),
        min = 0,
        max = 0.5    # because proportion is between 0 and 1
      ) %>%
      hc_add_theme(hc_theme_google())
  })
  
  ## Make table ## 
  
  # Reactive dataset for attribute table (filtered by race checkbox)
  df_attr <- reactive({
    req(input$attribute)
    req(input$raceCheckbox)  # ensure at least one race is selected
    
    my_data %>% 
      mutate(Class = str_to_title(Class)) %>% 
      filter(Race %in% input$raceCheckbox) %>%
      filter(Level >= input$levelSlider[1], Level <= input$levelSlider[2]) %>% 
      group_by(Class) %>%
      dplyr::summarize(
        avg_value = round(mean(.data[[input$attribute]], na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_value))
  })

  
  ## Render DataTable
  output$attr_table <- DT::renderDataTable({

    DT::datatable(
      df_attr(),
      rownames = FALSE,
      options = list(
        pageLength = nrow(df_attr()),
        scrollY = "110px",
        dom = 't',
        ordering = TRUE
      ),
      colnames = c("Class", str_to_title(input$attribute))
    ) %>%
      DT::formatRound(columns = "avg_value", digits = 2)
  })
  
  ## Render pie chart
  # Reactive dataset for pie chart
  df_background <- reactive({
    req(input$raceCheckbox)
    
    my_data %>%
      filter(Race %in% input$raceCheckbox) %>%
      filter(Level >= input$levelSlider[1], Level <= input$levelSlider[2]) %>% 
      mutate(Background = str_to_title(Background)) %>%
      group_by(Background) %>%
      dplyr::summarise(count = n(), .groups = "drop") %>%
      mutate(perc = round(100 * count / sum(count), 1))
  })
  
  # Render pie chart
  output$backgroundPie <- renderHighchart({
    pie_data <- df_background()
    pie_colors <- viridis::inferno(n = nrow(pie_data))
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Proportion of Character Backgrounds") %>%
      hc_add_series(
        data = list_parse(
          pie_data %>% transmute(name = Background, y = count)
        ),
        name = "Characters",
        colorByPoint = TRUE
      ) %>%
      hc_colors(pie_colors) %>%
      hc_tooltip(pointFormat = "{point.name}: {point.percentage:.1f}%") %>%
      hc_plotOptions(
        pie = list(
          allowPointSelect = TRUE,
          cursor = "pointer",
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_add_theme(hc_theme_google())
  })
  
  ## make the correlation plot
  # Reactive filtered numeric data
  numeric_filtered <- reactive({
    req(input$raceCheckbox)
    req(input$levelSlider)
    
    my_data %>%
      filter(Race %in% input$raceCheckbox,
             Level >= input$levelSlider[1],
             Level <= input$levelSlider[2]) %>%
      select(all_of(c("Strength", "Dexterity", "Constitution", 
                      "Intelligence", "Wisdom", "Charisma")))
  })
  
  # Render correlation heatmap
  output$attrCorrelation <- renderPlotly({
    df <- numeric_filtered()
    req(nrow(df) > 1)  # require at least 2 rows to calculate correlation
    
    # Compute correlation
    cor_res <- rcorr(as.matrix(df))
    cor_matrix <- cor_res$r
    
    # Melt matrix
    cor_long <- melt(cor_matrix)
    
    # Inferno colors
    inferno_colors <- viridis::inferno(200)
    low_color <- inferno_colors[50]
    high_color <- inferno_colors[150]
    
    # Reorder y-axis for top-left to bottom-right diagonal
    cor_long$Var2 <- factor(cor_long$Var2, levels = rev(colnames(cor_matrix)))
    
    # Create ggplot heatmap
    gx <- ggplot(cor_long, aes(
      x = Var1, 
      y = Var2, 
      fill = value * 100,   # scale 0-1 to 0-100
      text = paste0(round(value*100), "%")
    )) +
      geom_tile(color = "black") +
      scale_fill_gradient(
        name = "Correlation",
        low = low_color,
        high = high_color,
        limits = c(0, 100),
        labels = function(x) paste0(x, "%")  # add percent sign
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,        # aligns text to the end
          vjust = 1,        # moves text closer to the plot
          margin = margin(t = 0)  # remove top margin
        ),
        axis.text.y = element_text(margin = margin(r = 1)),
        axis.title = element_blank(),
        panel.grid = element_blank()
      ) +
      coord_fixed()
    
    # Convert to interactive plotly
    ggplotly(gx, tooltip = "text") %>%
      layout(
        autosize = FALSE,
        width = 510,
        height = 510
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Reactive filtered data for HP histogram
  filtered_hp <- reactive({
    req(input$raceCheckbox, input$levelSlider)
    my_data %>%
      filter(
        Race %in% input$raceCheckbox,
        Level >= input$levelSlider[1],
        Level <= input$levelSlider[2],
        Hp > 0
      )
  })
  
  # Render HP histogram ## 
  output$hpHist <- renderPlot({
    df <- filtered_hp()
    req(nrow(df) > 0)  # Ensure we have data
    
    ggplot(df, aes(x = Hp)) +
      geom_histogram(
        aes(y = after_stat(count / sum(count))),
        bins = 20,
        fill = viridis::inferno(50)[25],
        color = "black"
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(
        x = "HP",
        y = "Proportion"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.margin = margin(5, 5, 5, 5),
        axis.title = element_text(size = 12)
      )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
