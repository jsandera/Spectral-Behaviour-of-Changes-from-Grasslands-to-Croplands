# this script creates spectral curves plot from created Landsat .csv files

library(ggplot2)
library(rChoiceDialogs)
library(lubridate)

# Define path to a working directory in your system
vstupni_adresar <- rchoose.dir(getwd(), "Set path to a directory where original data are located exported from GEE for Landsat bands")
prac_adresar <- setwd(vstupni_adresar)

# Create a list of .csv files in selected directory
csv_files <- list.files(path=getwd(), pattern=".csv", recursive=FALSE)

# For loop in order to create multiple plots for each unique pixel ID
for(i in csv_files){
  # read the data
  the_data <- read.csv(i) 
  
  # Conver Time column in to proper Date Format
  the_data$Time <- ymd(the_data$Time)
  
  # plot data
  sp_curve <- ggplot(data=the_data, aes(x=Time)) + geom_line(aes(y=SR_B2, color="SR_B2"))+
    geom_line(aes(y=SR_B3, color="SR_B3"))+
    geom_line(aes(y=SR_B4, color="SR_B4"))+
    geom_line(aes(y=SR_B5, color="SR_B5"))+
    geom_line(aes(y=SR_B6, color="SR_B6"))+
    geom_line(aes(y=SR_B7, color="SR_B7"))+
    ylab("Reflectance")+
    scale_color_manual(name="Spectral Bands", values=c("SR_B2"="blue", "SR_B3"="green","SR_B4"="red",
                                                       "SR_B5"="yellow", "SR_B6"="magenta", "SR_B7"="pink"), 
                       labels=c("B2", "B3", "B4", "B5", "B6", "B7"))
  
  sp_curve
  
  # Save Results to disk for Each Unique Pixel ID
  # Define File Name
  name1 <- gsub(pattern = "\\.csv$", "", i)
  name2 <- paste("Spectral_Response", name1, ".jpeg")
  disk_export <- ggsave(plot=sp_curve, filename=name2, dpi=300, units="px", width=3000, height=2000)
  
  
}


#################################################################################################################################################################################

