# Extended Isolation Forest Algorithm for Filtered Pixel ID of Isolation Algorithm

library(h2o)
library(rChoiceDialogs)
library(xlsx)

# Define path to a working directory in your system
vstupni_adresar <- rchoose.dir(getwd(), "Set path to a working directory")
prac_adresar <- setwd(vstupni_adresar)

# Create a List of CSV for ALL Pixel IDS in working directory
ls <- list.files(getwd(), full.names = FALSE, pattern=".csv")

# Start H2O library cluster
h2o.init(nthreads=20, max_mem_size="60G")
h2o.removeAll()

# Start of a For Loop

for (i in ls){
  
# read csv file
re <- read.csv(i) 

# Filter Input Data for Single Vegetation Index
f1 <- subset(re, select=c("point_id", "Time", "NDVI"))


# Extension Level Parameter for Extended Isolation Forest Algorithm Settings
eL <- length(ncol(f1)-1) - 1

# Convert Input Data into H2O frame
data = as.h2o(f1)


###########################################################################################################################################################
###########################################################################################################################################################

# Extended Isolation Forest

# Anomalies Detection in Time Series
ie_dvi <- h2o.extendedIsolationForest(x=colnames(data), training_frame=data,  ntrees=500, extension_level = eL, 
                                      sample_size=nrow(f1))


# Make Predictions for All Models of Extended Isolation Forests Algorithms
p_dvi <- cbind(f1, as.data.frame(predict(ie_dvi, data)$anomaly_score))


# Output File Name Extended Isolation Forest
pat <- gsub(pattern = "\\.csv$", "", i)
out_name1 <- paste("EIF_NDVI_", pat, ".xlsx")

# Save Predicted Models in .xlsx format to hdd
s_dvi <- write.xlsx(x=p_dvi, out_name1)


################################################################################################################################################################
################################################################################################################################################################

# Original Isolation Forest Models

ie_dvi1 <- h2o.isolationForest(x=colnames(data), training_frame=data,  ntrees=500, sample_rate=0.1, max_depth = 20)


# Make Predictions for All Models of Original Isolation Forests Algorithms
p_dvi1 <- cbind(f1, as.data.frame(predict(ie_dvi1, data)$predict))

# Output File Name Extended Isolation Forest
pat1 <- gsub(pattern = "\\.csv$", "", i)
out_name2 <- paste("OIF_NDVI_", pat1, ".xlsx")

# Save Predicted Models in .xlsx format to hdd
s_dvi1 <- write.xlsx(x=p_dvi1, out_name2)

}




