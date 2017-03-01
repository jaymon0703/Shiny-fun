library(shiny)

#load packages
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(directlabels)

#Step 1: Get the data
data <- read.csv("test.csv", header = TRUE, stringsAsFactors=F)
data_industry <- read.csv("TOP40-INDUSTRY.csv", row.names=1, header = FALSE, stringsAsFactors=F)
rownames(data_industry) <- gsub(rownames(data_industry),pattern = " ", replacement = ".")

#Step 2: Create your indicator
#calculate number of iterations for loop
numloops <- ncol(data)/3
#create new xts objects
sumMACDarr <- xts()
MACD_short <- xts()
MACD_long <- xts()

#time to loop
for(i in 1:numloops){
  s1.dates <- as.Date(data[,i*3-1], format="%d/%m/%Y")
  s1 <- xts(data[,i*3], s1.dates)
  #Compute Short Period MACD
  macd1 <- MACD(s1,12,26,9,maType = "EMA", percent = FALSE)
  m_int1 <- (macd1$macd - macd1$signal)/s1
  m_int1[is.na(m_int1)] <- 0
  colnames(m_int1) <- data[1,i*3-2]
  MACD_short <- cbind(MACD_short,m_int1)
  #Compute Long Period MACD
  macd2 <- MACD(s1,240,520,180,maType = "EMA", percent = FALSE)
  m_int2 <- (macd2$macd - macd2$signal)/s1
  m_int2[is.na(m_int2)] <- 0
  colnames(m_int2) <- data[1,i*3-2]
  MACD_long <- cbind(MACD_long,m_int2)
  
  sumMACDarr <- cbind(sumMACDarr,m_int1 + m_int2)
}

chartdf <- t(rbind(MACD_short[nrow(MACD_short),], MACD_long[nrow(MACD_long),]))
chartdf <- as.data.frame(chartdf)
chartdf <- merge(chartdf, data_industry, by="row.names")

tickerlist <- substr(chartdf[,1],1,3)

ui <- fluidPage(
  
  sidebarPanel(
      selectInput(inputId = "Stocks", "Tickers", tickerlist, selected = tickerlist, multiple = TRUE), 
      width = 3
  ),
  mainPanel(
    plotOutput('ggplot')
  )
  
)
      
server <- function(input, output) {

  output$ggplot <- renderPlot({
    
    subsetvec <- which(tickerlist %in% input$Stocks)
    chartdf <- chartdf[subsetvec,]
    ggplot(chartdf,aes(x=chartdf[,3], y=chartdf[,2], colour = chartdf[,4], label=substr(chartdf[,1],1,3))) + 
      geom_point(position = "jitter") + 
      geom_text(hjust=0.5, vjust=-0.5) + 
      labs(title = "Stock Momentum Trends (Top40)", x="Medium-Term Momentum", y="Short-Term Momentum", colour = "Industry") + 
      geom_vline(xintercept=0, linetype="longdash") + geom_hline(yintercept=0, linetype="longdash") + 
      theme(legend.position="bottom")
    
  }, height = 600, width = 1000)}

shinyApp(ui = ui, server = server)