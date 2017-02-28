#Basic framework for constructing a backtest
#Step 1: Get the data
#Step 2: Create your indicator
#Step 3: Construct your trading rule
#Step 4: The trading rules/equity curve
#Step 5: Evaluate strategy performance

#load packages
require(quantmod)
require(PerformanceAnalytics)
require(ggplot2)
require(directlabels)
require(TTR)
require(ggrepel)

#Step 1: Get the data
  setwd("C:/Users/jasen/Work/R/R scripts/Momentum_Chart_app")
  data <- read.csv("test.csv", header = TRUE, stringsAsFactors=F)
  data_industry <- read.csv("TOP40-INDUSTRY.csv", row.names=1, header = FALSE, stringsAsFactors=F)
  #rownames(data_industry) <- gsub(rownames(data_industry),pattern = " ", replacement = ".")
  #data_industry <- t(data_industry)

#Step 2: Create your indicator
#calculate number of iterations for loop
  numloops <- ncol(data)/3
#create new xts objects
  sumMACDarr <- xts()
  MACD_short <- xts()
  MACD_long <- xts()
  
  rsivec <- vector(length = numloops)
  a <- nrow(data) - 14

#time to loop
  for(i in 1:numloops){
    s1.dates <- as.Date(data[,i*3-1], format="%d-%m-%y")
    s1 <- xts(data[,i*3], s1.dates)
    #Compute Short Period MACD
    macd1 <- MACD(s1,12,26,9,maType = "EMA", percent = FALSE)
    m_int1 <- (macd1$macd - macd1$signal)/s1
    m_int1[is.na(m_int1)] <- 0
    m_int1 <- ifelse(m_int1 > 0.01, 0.01, m_int1)
    m_int1 <- ifelse(m_int1 < -0.01, -0.01, m_int1)
    colnames(m_int1) <- data[1,i*3-2]
    MACD_short <- cbind(MACD_short,m_int1)
    #MACD_short <- as.data.frame(MACD_short)
    #Compute Long Period MACD
    macd2 <- MACD(s1,240,520,180,maType = "EMA", percent = FALSE)
    m_int2 <- (macd2$macd - macd2$signal)/s1
    m_int2[is.na(m_int2)] <- 0
    m_int2 <- ifelse(m_int2 > 0.03, 0.03, m_int2)
    m_int2 <- ifelse(m_int2 < -0.03, -0.03, m_int2)
    colnames(m_int2) <- data[1,i*3-2]
    MACD_long <- cbind(MACD_long,m_int2)
    #MACD_long <- as.data.frame(MACD_long)
    
    rsi <- RSI(data[(a:nrow(data)),i * 3])
    rsivec[i] <- round(rsi[15],0)
    
    #sumMACDarr <- cbind(sumMACDarr,m_int1 + m_int2)
  }

# #Merge and write to CSV for inspection    
#   # write.csv(as.data.frame(sumMACDarr),"test_MACD_output.csv")
#   CHECK <- merge(MACD_short, MACD_long)
#   write.csv(as.data.frame(CHECK),"test_MACD_output.csv")
  rsivec <- setNames(rsivec,colnames(MACD_short))
  rsivec <- sort(rsivec)
  chartdf <- t(rbind(MACD_short[nrow(MACD_short),], MACD_long[nrow(MACD_long),]))
  chartdf <- as.data.frame(chartdf)
  chartdf <- merge(chartdf, data_industry, by="row.names")

  p <- ggplot(chartdf,aes(x=chartdf[,3], y=chartdf[,2], colour = chartdf[,4], label=paste(substr(chartdf[,1],1,3), rsivec)))
  p <- p + theme(text = element_text(size = 8.5)) # legend text size
  
  p <- p + coord_cartesian(xlim = c(-0.03,0.03), ylim = c(-0.01,0.01))
  p <- p + theme(legend.position="bottom")

  p <- p + geom_text_repel(size=3)
  p <- p + geom_point(size=1)
  
  # Start adding quandrants
  # Lagging
  p <- p + geom_segment(y=-0.02, yend=-0.00005, x=-0.01, xend=-0.01, linetype="dashed", color="blue")
  p <- p + geom_segment(y=-0.00005, yend=-0.00005, x=-0.03, xend=-0.01, linetype="dashed", color="blue")
  
  # Bust - trim shorts
  p <- p + geom_segment(y=-0.02, yend=-0.005, x=-0.02, xend=-0.02, linetype="dashed", color="brown")
  p <- p + geom_segment(y=-0.005, yend=-0.005, x=-0.03, xend=-0.02, linetype="dashed", color="brown")
  
  # Add to Shorts
  p <- p + geom_segment(y=0.02, yend=0.00005, x=-0.01, xend=-0.01, linetype="dashed", color="red")
  p <- p + geom_segment(y=0.00005, yend=0.00005, x=-0.03, xend=-0.01, linetype="dashed", color="red")
  
  # Watch
  p <- p + geom_segment(y=-0.0025, yend=-0.0025, x=-0.01, xend=0.01, linetype="dashed", color="goldenrod1")
  p <- p + geom_segment(y=0.0025, yend=0.0025, x=-0.01, xend=0.01, linetype="dashed", color="goldenrod1")
  
  # Add to Longs
  p <- p + geom_segment(y=-0.02, yend=-0.00005, x=0.01, xend=0.01, linetype="dashed", color="firebrick4")
  p <- p + geom_segment(y=-0.00005, yend=-0.00005, x=0.03, xend=0.01, linetype="dashed", color="firebrick4")
  
  # Add to Shorts
  p <- p + geom_segment(y=0.02, yend=0.00005, x=0.01, xend=0.01, linetype="dashed", color="forestgreen")
  p <- p + geom_segment(y=0.00005, yend=0.00005, x=0.03, xend=0.01, linetype="dashed", color="forestgreen")
  
  # Boom - trim longs
  p <- p + geom_segment(y=0.02, yend=0.005, x=0.02, xend=0.02, linetype="dashed", color="darkorange1")
  p <- p + geom_segment(y=0.005, yend=0.005, x=0.03, xend=0.02, linetype="dashed", color="darkorange1")

  # Start adding text
  # Bust (trim shorts)
  p <- p + annotate("text", x = -0.025, y = -0.0075, label = "Bust (trim shorts)", colour="darkgrey", size=3.5)
  # Lagging
  p <- p + annotate("text", x = -0.015, y = -0.005, label = "v Lagging v", colour="darkgrey", angle=45, size=3.5)
  # < Breaking Down <
  p <- p + annotate("text", x = 0, y = -0.00625, label = "< Breaking Down <", colour="darkgrey", size=3.5)
  # Add to Longs
  p <- p + annotate("text", x = 0.02, y = -0.005, label = "Add to Longs", colour="darkgrey", size=3.5)
  # Add to Shorts
  p <- p + annotate("text", x = -0.02, y = 0.005, label = "Add to Shorts", colour="darkgrey", size=3.5)
  # < Breaking Out <
  p <- p + annotate("text", x = 0, y = 0.00625, label = "> Breaking Out >", colour="darkgrey", size=3.5)
  # Watch
  p <- p + annotate("text", x = 0, y = 0, label = "Watch", colour="darkgrey", size=3.5)
  # ^ Leading ^
  p <- p + annotate("text", x = 0.015, y = 0.005, label = "^ Leading ^", colour="darkgrey", angle=45, size=3.5)
  # Boom (trim longs)
  p <- p + annotate("text", x = 0.025, y = 0.0075, label = "Bust (trim longs)", colour="darkgrey", size=3.5)
  
  # Add labels
  p <- p + labs(title="Stock Momentum Trends",x = "Medium-Term Momentum",y="Short-Term Momentum")
  p <- p + theme(plot.title = element_text(hjust = 0.5))

  p