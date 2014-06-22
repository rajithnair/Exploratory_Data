plot4<-function(){
  
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
  
  if(!(("data.table" %in% rownames(installed.packages())) & ("ggplot2" %in% rownames(installed.packages())) & ("gridExtra" %in% rownames(installed.packages())))){
    
    if (!("data.table" %in% rownames(installed.packages()))){
      
      install.packages("data.table")
    }
    
    if (!("ggplot2" %in% rownames(installed.packages()))){
      
      install.packages("ggplot2")
    }
    
    if (!("gridExtra" %in% rownames(installed.packages()))){
      
      install.packages("gridExtra")
    }
    
    library(data.table)
    library(ggplot2)
    library(gridExtra)
    
  }else{
    
    library(data.table)
    library(ggplot2)
    library(gridExtra)
  }
  
    
  # Loading Data
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  SCC <- readRDS("./data/Source_Classification_Code.rds")  
  
    
  # Selecting the coal combustion related sources from the SCC dataset
  
  ## Selecting coal combustion related sources from the EI.Sector variable
  coal_index<-grep("Coal$",SCC$EI.Sector)
  
  
  # Subsetting the NEI dataset with the coal combustion related sources 
  NEI <- NEI[NEI$SCC %in% SCC$SCC[coal_index],]
  
  
  # Mapping the EI.Sector variable from the SCC dataset to the NEI dataset
  for(i in 1:nrow(NEI)){
    
    NEI$EI.Sector[i] <- as.character(SCC$EI.Sector[SCC$SCC == NEI$SCC[i]]) 
  }
  
  # Converting the data frame to data table
  
  NEI<-data.table(NEI)
    
  # Calculating total PM_2.5 emissions across each year for each type of source
  
  NEI<-NEI[,lapply(.SD,sum),by='year,EI.Sector',.SDcols=c("Emissions")]
  
  ### Plotting Script
  
  # Initializing png graphich device
  png(file="plot4.png",width=1000,height=900)
  
  # Generating Plot with individual coal combustion sources 
  
  g1<-ggplot(NEI,aes(year,Emissions)) + geom_point(aes(col=EI.Sector),size=3) + geom_line(aes(col=EI.Sector),size=.70) + geom_text(aes(label=round(Emissions,2)),hjust=0, vjust=0,size=3.5) + labs(title = "Total PM 2.5 Emissions from each coal combustion source",x = "Year",y = "Total PM_2.5", color="Coal Combustion Sources")
  
  # Generating Plot with the total emissions from all the coal combustion sources 
  
  ## Calculating the total emissions across each year  
  NEI<-NEI[,lapply(.SD,sum),by='year',.SDcols=c("Emissions")] 
  
  ## Generating
  g2<-ggplot(NEI,aes(year,Emissions)) + geom_point(col="forestgreen") + geom_line(col="forestgreen") + geom_text(aes(label=round(Emissions,2)),hjust=0, vjust=0,size=3.5) + labs(title = "Total PM 2.5 Emissions from all the sources",x = "Year",y = "Total PM_2.5")
  
  # Arranging the plots in a grid
  grid.arrange(g1, g2, nrow=2)
  
  # Closing the png device
  dev.off()
  
}
