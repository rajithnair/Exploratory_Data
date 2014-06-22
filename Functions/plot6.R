plot6<-function(){
  
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
    
  # Loading Data
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  SCC <- readRDS("./data/Source_Classification_Code.rds")  
  
  
  # Selecting the motor vehicle related sources from the SCC dataset
  
  ## Selecting motor vehicle related sources from the EI.Sector variable
  vehicle_index<-grep("Vehicles$",SCC$EI.Sector)
  
  
  # Subsetting the NEI dataset with the motor vehicle related sources 
  NEI <- NEI[NEI$SCC %in% SCC$SCC[vehicle_index],]
  
  # Subsetting to get the Baltimore City data 
  NEI <- NEI[NEI$fips %in% c("24510","06037"),]
  
  # Converting the data frame to data table
  
  NEI<-data.table(NEI)
    
  # Calculating total PM_2.5 emissions across each year for Baltimore and Los Angeles
  
  NEI<-NEI[,lapply(.SD,sum),by='year,fips',.SDcols=c("Emissions")]
  NEI$fips<-factor(NEI$fips, levels = c("06037","24510"), labels = c("Los Angeles County","Baltimore City")) 
  
  ### Plotting Script
  
  # Initializing png graphich device
  png(file="plot6.png",width=800,height=600)
  
  # Generating Plot 
  g1<-ggplot(NEI,aes(year,Emissions)) + geom_point(aes(col=fips),size=3) + geom_line(aes(col=fips),size=.70) + geom_text(aes(label=round(Emissions,2)),hjust=0, vjust=0,size=3.5) + labs(title = "Total PM 2.5 Emissions across the years",x = "Year",y = "Total PM_2.5",color="Locations")
  print(g1)
    
  # Closing the png device
  dev.off()
  
}
