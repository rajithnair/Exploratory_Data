plot3<-function(){
  
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
  
  # Loading Data
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  
  # Subsetting to get the Baltimore City data 
  NEI <- NEI[NEI$fips == "24510",]
  
  # Checking whether the required packages are installed and accordingly the packages are loaded 
  
  if(!(("data.table" %in% rownames(installed.packages())) & ("ggplot2" %in% rownames(installed.packages())))){
    
    if (!("data.table" %in% rownames(installed.packages()))){
      
      install.packages("data.table")
    }
    
    if (!("ggplot2" %in% rownames(installed.packages()))){
      
      install.packages("ggplot2")
    }
    
    library(data.table)
    library(ggplot2)
    
  }else{
    
    library(data.table)
    library(ggplot2)
    
  }
  
  # Converting the data frame to data table
  
  NEI<-data.table(NEI)
  
  # Calculating total PM_2.5 emissions across each year for each type of source
  
  NEI<-NEI[,lapply(.SD,sum),by='year,type',.SDcols=c("Emissions")]
      
  ### Plotting Script
  
  # Initializing png graphich device
  png(file="plot3.png",width=800,height=600)
  
  # Generating Plot
  g<-ggplot(NEI,aes(year,Emissions)) + geom_point(aes(col=type),size=3) + geom_line(aes(col=type),size=.70) + geom_text(aes(label=round(Emissions,2)),hjust=0, vjust=0,size=3.5) + labs(title = "PM 2.5 Emissions across years in Baltimore",x = "Year",y = "Total PM_2.5", color="Type")
  print(g)
  
  # Closing the png device
  dev.off()
  
}
