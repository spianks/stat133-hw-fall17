library(shiny)
library(ggvis)

source("functions.R")


library(readr)
library(dplyr)
library(ggvis)
tab <- read.csv('stat133-hws-fall17/hw04/data/cleandata/cleanscores.csv')
tab
(tab$Grade)

myTable <- data.frame(table(tab$Grade))
myTable$Prop <- prop.table(myTable$Freq)
colnames(myTable) <- c("Grade", "Freq", "Prop")
ordering <- c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F")
myTable <- myTable[match(ordering, myTable$Grade),]


 plotType <- function(type){
   switch(EXP = "tab %>%
ggvis(x = xvar, y = yvar) %>%
  layer_points(fillOpacity := input$width2) ", none = "", lm = "%>% layer_model_predictions(model = 'lm')",
                                               lowess = "%>% layer_smooths()")
 }

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Grade Visualizer"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",h3("Grades Distribution"),
            tableOutput("table")),
     
      conditionalPanel(condition = "input.tabselected==2", 
                  selectInput("select1", label = ("X-axis variable"), 
                            choices = list("HW1" = "HW1", "HW2" = "HW2", "HW3" = "HW3", "HW4" = "HW4",
                                           "HW5" = "HW5", "HW6" = "HW6", "HW7" = "HW7",
                                           "HW8" = "HW8", "HW9" = "HW9", "Quiz1" = "QZ1", "Quiz2" = "QZ2", "Quiz3" = "QZ3", 
                                           "Quiz4" = "QZ4", "Test1" = "Test1", "Test2" = "Test2", "Lab" = "Lab", "Homework" = "Homework",
                                           "Quiz" = "Quiz", "Overall" = "Overall"), 
                            selected = "HW1"),
                  sliderInput("width", "Bin Width", min = 0, max = 10, value = 10)
                  #textInput("txt", summary_stats(tab$HW1))
                  ),
      conditionalPanel(condition = "input.tabselected==3",
                 selectInput("select2", label = ("X-axis variable"), 
                             choices = list("HW1" = "HW1", "HW2" = "HW2", "HW3" = "HW3", "HW4" = "HW4",
                                            "HW5" = "HW5", "HW6" = "HW6", "HW7" = "HW7",
                                            "HW8" = "HW8", "HW9" = "HW9", "Quiz1" = "QZ1", "Quiz2" = "QZ2", "Quiz3" = "QZ3", 
                                            "Quiz4" = "QZ4", "Test1" = "Test1", "Test2" = "Test2", "Lab" = "Lab", "Homework" = "Homework",
                                            "Quiz" = "Quiz", "Overall" = "Overall"),
                             selected = "Test1"),
                 selectInput("select3", label = ("Y-axis variable"), 
                             choices = list("HW1" = "HW1", "HW2" = "HW2", "HW3" = "HW3", "HW4" = "HW4",
                                            "HW5" = "HW5", "HW6" = "HW6", "HW7" = "HW7",
                                            "HW8" = "HW8", "HW9" = "HW9", "Quiz1" = "QZ1", "Quiz2" = "QZ2", "Quiz3" = "QZ3", 
                                            "Quiz4" = "QZ4", "Test1" = "Test1", "Test2" = "Test2", "Lab" = "Lab", "Homework" = "Homework",
                                            "Quiz" = "Quiz", "Overall" = "Overall"),
                             selected = "Overall"),
                 sliderInput("width2", "Opacity", min = 0, max = 1, value = 0.5),
                 radioButtons("scale", label = ("Show line"),
                              choices = list("none" = 1, 
                                             "lm" = 2, 
                                             "loess" = 3), 
                              selected = 1)
                  ) #condition closed 
            ), #sidebar panel 
     #sidebar layout
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Barplot", value = 1, ggvisOutput("barplot")),
        
        tabPanel("Histogram", value = 2, ggvisOutput("distPlot"), 
                 h4("Summary Statistics"), verbatimTextOutput("summarystat")),
        tabPanel("Scatterplot", value = 3, ggvisOutput("scatterplot"), 
                 h4("Correlation"), verbatimTextOutput("correlate")),
        id = "tabselected")
    )#main panel
  ) #sideb
) #fluid page closed




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
              
            
            vis_plot <- reactive({
              # Normally we could do something like props(x = ~HW1, y = ~HW2),
              # but since the inputs are strings, we need to do a little more work.
              xvar <- prop("x", as.symbol(input$select1))
              histogram <- tab %>%
                ggvis(x = xvar) %>%
                layer_histograms(stroke := 'white', width = input$width)
              
            })
            vis_plot %>% bind_shiny("distPlot")
            output$summarystat <- renderPrint(print_stats(summary_stats(unlist(select(tab, input$select1), use.names = FALSE))))
          
            
            vis_plot2 <- reactive({
              # tab$Grade <- factor(tab$Grade, levels = tab$Grade)
              histog <- tab %>%
                ggvis(x = (~Grade)) %>%
                layer_bars(fill := "blue") %>%
                add_axis("x", title = "Grade") %>%
                add_axis("y", title = "frequency") %>%
                scale_ordinal('x', domain = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
              
             
            })
            
            vis_plot2 %>% bind_shiny("barplot")
            
            
            output$correlate <- renderPrint(cat(cor(x=select(tab, input$select2), y = select(tab, input$select3))))
          
            output$table <-  renderTable({head(myTable, n = nrow(myTable))}, align = "c")
 
            vis_plot3 <- reactive({
              xvar <- prop("x", as.symbol(input$select2))
              yvar <- prop("y", as.symbol(input$select3))
              
              if (input$scale == 1){
                 tab %>%
                  ggvis(x = xvar, y = yvar) %>%
                  layer_points(fillOpacity := input$width2) 
              }
              else if (input$scale == 2){
                 tab %>%
                  ggvis(x = xvar, y = yvar) %>%
                  layer_points(fillOpacity := input$width2) %>%
                  layer_model_predictions(model = "lm")
              }
              else{
                tab %>%
                  ggvis(x = xvar, y = yvar) %>%
                  layer_points(fillOpacity := input$width2) %>%
                  layer_smooths()
              }
              
              
              

              
                
            })
              
            vis_plot3 %>% bind_shiny("scatterplot")
          
            
            

            
            
  
}

# Run the application 
shinyApp(ui = ui, server = server)

