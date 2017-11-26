library(shiny)
library(ggplot2)

#Assumes all dataset are cleaned. Have no missing values.

#All input elements in a ui component.
#Each input referenced by input$<inputid>. that is how it is accessed in server code.
#This is currently client app side.
#Input values are reactive by default
ui <- fluidPage("K-Means Clustering", 
                titlePanel("Uploading Dataset"),
                fileInput("file1", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                
                # Output: Data file ----
                tableOutput(outputId = "csvTable"),
                plotOutput('histPlot'),
                mainPanel(
                  plotOutput('plot1')
                )
                
                
                
)
server <- function(input, output){
  
  output$csvTable <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    df_ts <- ts(df)
    df_scaled <- scale(as.matrix(df_ts), center = TRUE, scale = TRUE )
    
    #str(df_scaled)
    #df_scaled <- scale(ts(df))
#    str(as.data.frame(df_scaled))
    df_scaled_df <- as.data.frame(df_scaled)
  output$histPlot <- renderPlot({ggplot(df_scaled_df, aes(Open, Close)) + geom_point()})
   # return(df_scaled)
    
    clusters <-  kmeans(df_scaled_df[,c(2,5)], 4)
    output$plot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(df_scaled_df[,c(2,5)],
           col = clusters$cluster,
           pch = 20, cex = 3)
      points(clusters$centers, pch = 4, cex = 4, lwd = 4)
    })
    
 
  })
}
shinyApp(ui=ui, server=server)

