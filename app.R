library(shiny)

#load packages
require(quantmod)
require(PerformanceAnalytics)
require(ggplot2)
require(directlabels)

#Step 1: Get the data
data <- read.csv("test.csv", header = TRUE, stringsAsFactors=F)
data_industry <- read.csv("TOP40-INDUSTRY.csv", row.names=1, header = FALSE, stringsAsFactors=F)
rownames(data_industry) <- gsub(rownames(data_industry),pattern = " ", replacement = ".")
#data_industry <- t(data_industry)

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
  #m_int1 <- ifelse(m_int1 > 0.02, 0.02, m_int1)
  #m_int1 <- ifelse(m_int1 < -0.02, -0.02, m_int1)
  colnames(m_int1) <- data[1,i*3-2]
  MACD_short <- cbind(MACD_short,m_int1)
  #MACD_short <- as.data.frame(MACD_short)
  #Compute Long Period MACD
  macd2 <- MACD(s1,240,520,180,maType = "EMA", percent = FALSE)
  m_int2 <- (macd2$macd - macd2$signal)/s1
  m_int2[is.na(m_int2)] <- 0
  #m_int2 <- ifelse(m_int2 > 0.06, 0.06, m_int2)
  #m_int2 <- ifelse(m_int2 < -0.06, -0.06, m_int2)
  colnames(m_int2) <- data[1,i*3-2]
  MACD_long <- cbind(MACD_long,m_int2)
  #MACD_long <- as.data.frame(MACD_long)
  
  sumMACDarr <- cbind(sumMACDarr,m_int1 + m_int2)
}

chartdf <- t(rbind(MACD_short[nrow(MACD_short),], MACD_long[nrow(MACD_long),]))
chartdf <- as.data.frame(chartdf)
chartdf <- merge(chartdf, data_industry, by="row.names")

#dataset <- chartdf
tickerlist <- substr(chartdf[,1],1,3)
#tickerlist <- chartdf[,1]

ui <- fluidPage(
  
    sidebarPanel(
      checkboxGroupInput(inputId = "Stocks", "Tickers", tickerlist, selected = tickerlist), width = 1),
  mainPanel(
    plotOutput('ggplot')
  ))
      
server <- function(input, output) {

  output$ggplot <- renderPlot({
    #p <- ggplot(chartdf,aes(x=chartdf[,3], y=chartdf[,2], colour = chartdf[,4], label=substr(chartdf[,1],1,3)))
    subsetvec <- which(substr(chartdf[,1],1,3) %in% input$Stocks)
    chartdf <- chartdf[subsetvec,]
    p <- ggplot(chartdf,aes(x=chartdf[,3], y=chartdf[,2], colour = chartdf[,4], label=substr(chartdf[,1],1,3)))
    p <- p + geom_point(position = "jitter") + geom_text(hjust=0.5, vjust=-0.5) + labs(title = "Stock Momentum Trends (Top40)", x="Medium-Term Momentum", y="Short-Term Momentum")
    p <- p + geom_vline(xintercept=0, linetype="longdash") + geom_hline(yintercept=0, linetype="longdash")
    p + theme(legend.position="bottom")
    
  }, height = 600, width = 1000)}

shinyApp(ui = ui, server = server)