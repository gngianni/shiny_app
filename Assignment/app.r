library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Model Generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"), 
               
            # Horizontal line ----
            tags$hr(),
            
            actionButton("runModel", "Plot Linear Model"), 
            
            downloadButton("export", "Export Linear Model as PNG")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           plotOutput("plot"), 
           numericInput("slope", "Slope:", value = 1),
           numericInput("intercept", "Intercept:", value = 0),
           h2("Regression Result"),
           verbatimTextOutput("regOutput"),
           tableOutput("contents")
           
        )
    )
)

# Define server logic required to draw a scatter plot 
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
 
# Server logic to output the linear regression only if the button is pressed 
output$plot <- renderPlot({
    
    if (input$runModel > 0) {
      req(dataInput())

        
 # Code to generate the plot used for linear regression        
      ggplot(dataInput(), aes(x = x, y = y)) +
        geom_point() +
        geom_abline(intercept = input$intercept, slope = input$slope, color = "red") +
        labs(x = "X", y = "Y",
        title = "Scatter Plot with Linear Regression") + 
        theme_light()
    }
  })
  
  # Fit linear model
  model <- eventReactive(input$runModel, {
    req(dataInput())
    lm(y ~ x, data = dataInput(), slope = input$slope, intercept = input$intercept)
  })

 # output the slope, intercept, and correlation coefficient 
    
    # Generate the model summary of the regression 
 output$regOutput <- renderPrint({
    req(model())
    reg_summary <- summary(model())
     
     # Select the column index that corresponds to each slope, intercept, and correlation
    slope <- reg_summary$coefficients[2, 1]
    intercept <- reg_summary$coefficients[1, 1]
    correlation <- cor(dataInput()$x, dataInput()$y)
   
    paste("Slope:", round(slope, 2),
          " , ", 
          "Intercept:", round(intercept, 2),
          " , ", 
          "Correlation Coefficient:", round(correlation, 2))
 })

# Define the plotOutput for the downloadable plot 
    # Recreated plot so it is possible to download without generating the plot first on the shiny app 
output$export <- downloadHandler(
  filename = function() { "linear_regression_plot.png" },
  content = function(file) {
    temp_plot <- ggplot(dataInput(), aes(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "X", y = "Y",
           title = "Scatter Plot with Linear Regression") +
      theme_light()
    
# print plot as PNG
 png(file, width = 2000, height = 1000, res = 300)
    print(temp_plot)
    dev.off()

  })


}

# Run the application 
shinyApp(ui = ui, server = server)