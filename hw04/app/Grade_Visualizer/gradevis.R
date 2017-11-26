source("../../code/functions.R")

library(shiny)
library(ggplot2)
library(ggvis)
library(stringr)

rawmatrix <- rawmatrix 
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Grade Visualizer"),
   
   # Sidebar with different widgets depending on the selected tab
   sidebarLayout(
     sidebarPanel(
       conditionalPanel(condition = "input.tabselected==1",
                        h3("Grade Distribution"),
                        position = 'right',
                        tableOutput("table1")),
       conditionalPanel(condition = "input.tabselected==2",
                        selectInput("type", label = strong("X-Axis Variable"),
                                    choices = unique(names(rawmatrix)[2:10]),
                                                     selected = "HW1"),
                        sliderInput("bins", "Bin Width",
                                    min = 1, max = 10, value = 10)),
       conditionalPanel(condition = "input.tabselected==3",
                        selectInput("type3", label = strong("X-Axis Variable"),
                                    choices = unique(names(rawmatrix)[2:21]),
                                    selected  = "EX1"),
                        selectInput("type4", label = strong("Y-Axis Variable"),
                                    choices = unique(names(rawmatrix)[2:21]),
                                    selected = "Overall"),
                        sliderInput("inputOpacity", "Opacity",
                                    min = 0, max = 1, value = .5),
                        
                        radioButtons("type5", "Show Line", 
                                     choices = list("none" = "none", "lm" = "lm", "loess" = "smooth"),
                                     selected = "none")
     )
   ),
      
      # Show a plot of the generated distribution
   mainPanel(
         tabsetPanel(type = "tabs",
                     id = "tabselected",
                     tabPanel("Barchart", value = 1,
                              ggvisOutput("barchart")),
                     tabPanel("Histogram", value = 2,
                              ggvisOutput("histogram"),
                              verbatimTextOutput("summaryTitle"),
                              verbatimTextOutput("summary")),
                     tabPanel("Scatterplot", value = 3,
                              ggvisOutput("scatter"),
                              verbatimTextOutput("info"))
                    )
      )
   )
)
# Define server logic 
server <- function(input, output) {
  # Barchart for first tab
  vis_barchart <- reactive({
    rawmatrix %>%
      ggvis(x = ~Grade, fill := "#ef623b") %>% 
      layer_bars(stroke := "#ef623b", width = 0.9,
                 fillOpacity := 0.8, fillOpacity.hover := 1) %>%
      add_axis("y", title = 'frequency')
  })
  
  vis_barchart %>% bind_shiny("barchart")
  
  # Histogram for second tab
  vis_histogram <- reactive({
    var2 <- prop("x", as.symbol(input$type))
    rawmatrix %>%
      ggvis(x = var2, fill := "#abafb5") %>%
      layer_histograms(stroke := 'white',
                       width = input$bins)
  })
  
  vis_histogram %>% bind_shiny("histogram")
  
  output$summaryTitle <- renderText({
    "Summary Statistics"
  })
  
  output$summary <- renderPrint({
    print_stats(rawmatrix[ , input$type)]
  })
  
  grade_table <- table(rawmatrix[, "Grade"])
  grades <- names(grade_table)
  total_grades <- sum(grade_table)
  freq <- c()
  prop <- c()
  for (i in 1:10) {
    freq <- append(freq, grade_table[[i]])
    prop <- append(prop, round(grade_table[[i]] / total_grades, digits = 2))
  }
  
  grade_dists <- data.frame(grades, freq, prop)
  
  output$table1 <- renderTable(grade_dists)
  
  # Scatterplot for third tab
  output$Scatterplot <- renderPlot({
    x <- rawmatrix[ , input$type3]
    y <- rawmatrix[ , input$type4]
    plot(x, y, xlab = input$type3, ylab = input$type4)
  })

  vis_scatter <- reactive({
    var2 = prop("x", as.symbol(input$type3))
    var3 = prop("y", as.symbol(input$type4))
    var4 = prop("model", as.symbol(input$type5))
    print(var4)
    
    gg <- rawmatrix %>% ggvis(x = var2, y = var3, opacity := input$inputOpacity) %>% layer_points()
    if (input$type5 == "lm") {
      gg = gg %>% layer_model_predictions(model = "lm", stroke := "purple")
    }
    else if (input$type5 == "smooth") {
      gg = gg %>%  layer_model_predictions(model = "loess", stroke := "purple")
  }
  gg
  })
  
  vis_scatter %>% bind_shiny("scatter")
  
  output$info <- renderText({
    x <- rawmatrix[,input$type3]
    y <- rawmatrix[,input$type4]
    paste0("Correlation : ", cor(x, y))
  })
}  
# Run the application 
shinyApp(ui = ui, server = server)

