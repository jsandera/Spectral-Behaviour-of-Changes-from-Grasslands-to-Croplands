### This Script tets significant trend in Time Series Landsat Bands with help of Mann-Kendall Test

library(Kendall)
library(rChoiceDialogs)

# Define path to a working directory in your system
vstupni_adresar <- rchoose.dir(getwd(), "Set path to a directory where original data are located exported from GEE for Landsat bands")
prac_adresar <- setwd(vstupni_adresar)

# Create a list of .csv files in selected directory
csv_files <- list.files(path=getwd(), pattern=".csv", recursive=FALSE)


# Begining of the for Loop of every .csv file (pixel id)
for (i in csv_files){
  csv_read <- read.csv(i)
  
  # Function that converts All Landsat Values to Numeric Vector as requested for Mann-Kendall Test
  num = function(x){
    a <- as.numeric(x)
  }
  # Apply Numeric Function to All Spectral Bands
  csv_read[4:9] <- lapply(csv_read[4:9], num)
  
  # Run Mann-Kendall Test
  mann_kendall <- lapply(csv_read[4:9], MannKendall)
  test_stats <- do.call(rbind, mann_kendall)
  
  # Define File Name in For Loop to be saved on HDD
  name_file1 <- gsub(pattern = "\\.csv$", "", i)
  name_file2 <- paste("Mann_Kendall_", name_file1, ".csv")
  
  # Save Results from Mann-Kendall Test to hdd in Excel file
  results <- write.csv(test_stats, file=name_file2)
}

