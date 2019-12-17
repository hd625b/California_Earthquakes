#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pkg.names <- c("dplyr", "ggmap", "plot3D", "psych", "qcc", 
               "reshape2", "rgl", "scatterplot3d", "shiny", "tidyverse") 
invisible(suppressMessages(lapply(pkg.names, require, character.only = TRUE)))
# fileLocation <- "https://github.com/hd625b/California_Earthquakes/blob/master/californiaEarthquakes.csv"
df <- read_csv("californiaEarthquakes.csv", 
               col_types = "nnnn")
rbPal <- colorRampPalette(c( "#000000", "#9BB2CC", "#43C43E", 
                             "#FFBA08", "#0982ED", "#D00000"))
df$Col <- rbPal(6)[as.numeric(cut(df$mag, breaks = 6))]
eqCount <- prettyNum(as.numeric(nrow(df)), big.mark = ',')

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Sidebar with an input for number of clusters 
    pageWithSidebar(
        headerPanel('California Earthquakes k-means clustering'),
        sidebarPanel(
            selectInput('xcol', "Longitude", names(df[2])),
            selectInput('ycol', "Latitude", names(df[1])),
            selectInput('zcol', "Depth", names(df[3]),
                        selected=names(df)[[4]]),
            numericInput('clusters', 'Clusters', 16,
                         min = 1, max = 20), width = 2,
        ),
        mainPanel(
            plotOutput("plot1", height = "800px", width = "1500px"),
            plotOutput("plot2")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Combine the selected variables into a new data frames
    selectedData <- reactive({
        df[, c(input$xcol, input$ycol)]
    })
    Plot3dData <- reactive({
        df[, c(input$xcol, input$ycol, input$zcol)]
    })
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    output$plot2 <- renderPlot({
        par3d("windowRect" = c(0, 52, 1100, 1000), "zoom" = 0.7)
        plot3d(df$latitude, df$longitude, df$depth, 
               xlab = "", ylab = "", zlab = "", type = "s",
               col = df$Col, size = df$mag * .25)
        title3d(main = paste(eqCount, "Earthquakes"), cex = 2,
                xlab = "Latitude", ylab = "Longitude",
                zlab = "Depth (km)", col="navyblue")
        bgplot3d({plot.new()})
        # play3d(spin3d(axis = c(-1,0,0), rpm = 4), duration = 6)
        # play3d(spin3d(axis = c(-0.01,-0.01,.10), rpm = 3), duration = 6)
        # play3d(spin3d(axis = c(-0.0,.01,0.1), rpm = 3), duration = 1)
    })
    output$plot1 <- renderPlot({
        # par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             type = "p",
             cex = df$mag + 0.25,
             col = clusters()$cluster,
             pch = 19,
             xaxt = "n",
             yaxt = "n",
             xlab = "",
             ylab = "")
        title(paste(eqCount, "Earthquake in the last 30 days", sep = " "), 
              cex.main = 2, font.main= 4, col.main= "darkblue",
              xlab = "Longitude", ylab = "Latitude",
              cex.lab = 1.75, font.lab = 2, col.lab = "darkblue")
        axis(1, at = seq(-115, -125, by = -0.5), col = "darkblue",
             font = 2, col.axis = "darkblue")
        axis(2, at = seq(32, 43, by = 1.5), col = "darkblue", 
             font = 2, col.axis = "darkblue")
        points(clusters()$centers, pch = 4, cex = 5, lwd = 4)
        # write.csv(clusters()$centers, "MyClusters.csv", row.names = F)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

