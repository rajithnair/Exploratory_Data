plot1<-function(){
  
  ### OBTAINING THE DATA FROM THE SOURCE
  
  fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  zipname <- "./data/exdata-data-NEI_data.zip"
  
  # Creating the data folder
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  # Unzipping the file
  if (!file.exists(zipname)){
    download.file(fileurl, destfile=zipname, mode="wb")
    unzip(zipname, exdir="./data")
  }
  
  ## Loading data
  NEI <- readRDS("./data/summarySCC_PM25.rds")
    
    
  # Calculating total PM_2.5 emissions across each year
  data<-tapply(NEI$Emissions,NEI$year,sum)
  data<-data.frame(year=names(data),total_PM2.5=data,row.names=NULL)
  
  ### Plotting Script
  
  # Initializing png graphich device
  png(file="plot1.png",width=480,height=480)
  
  # Setting the margin values
  par(mar=c(5.1,4.1,3,3))
  
  # Scatterplot
  plot(as.character(data$year),data$total_PM2.5,xaxt="n",pch=19,xlab="Year",ylab="total PM_2.5",main="Total PM 2.5 emissions")
  
  # Changing the X-axis tickers with the actual years
  axis(1, at=as.character(data$year), labels=as.character(data$year))
  
  # Adding a line to the scatterplot
  lines(as.character(data$year),data$total_PM2.5,col="blue")
  
  # Closing the png device
  dev.off()
  
}

