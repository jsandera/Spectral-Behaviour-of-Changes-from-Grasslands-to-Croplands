# This function manipulates dataframe for the Change Detection Analysis of LUCAS Points

library(anytime)
library(rChoiceDialogs)
library(xlsx)
library(dplyr)
library(data.table)
library(caret)

# Define path to a working directory in your system
vstupni_adresar <- rchoose.dir(getwd(), "Set path to a working directory")
prac_adresar <- setwd(vstupni_adresar)

# Choose Exported Data From Google  Earth Engine
A = read.csv(rchoose.files(getwd(), "Load .CSV Exported from GEE"))

# Define a function to transform Data
transform_df <- function(x=A){
  a <- as.data.frame(substring(x$system.index, 15, 22)) # cut string that contains date information
  b <- cbind(a, subset(x, select=c("SR_B2", "SR_B3", "SR_B4", 
                                       "SR_B5", "SR_B6", "SR_B7", "point_id"))) # select columns for new data frame
  
  
  
  c <- colnames(b)[colnames(b) == "substring(x$system.index, 15, 22)"] <- "Time" # change the name of time
  d <- anytime::anydate(b$Time)
  e <- cbind(d, b)
  f <- colnames(e)[colnames(e) == "d"] <- "Time"
  g <- e[order(as.Date(e$Time, format="%Y/%m/%m")),] # Order Data Frame by Date
  h <- g[g$Time >= "2006-01-01" & g$Time <="2015-12-30",] # Subset Data Frame from 2006 to 2015
  i <- subset(h, select=c("point_id", "Time", "SR_B2", "SR_B3", "SR_B4", "SR_B5", 
                                   "SR_B6", "SR_B7"))
  return(i)
}

# Execute function in order to transform the data
B = transform_df(A)

# Export Transformed Data to Microsoft Excel Format
C = write.xlsx(x=B, "Transformed_Data.xlsx")
############################################################################################

# Scale original data for Plot

# Scale Function
sc <- preProcess(as.data.frame(B[,3:8]), method=c("range"))
  
# Scale Data itself
ds <- predict(sc, as.data.frame(B))

# Export Scale Data to Disk
es <- write.xlsx(x=ds, "Scaled_Data.xlsx")

#################################################################################################################################################################
################################################################################################################################################################

# Filter all Ungie Point ,,ids"

# Order Data with the same ,,point_id"
f <- ds[with(ds,order(point_id)),]
f

# Export Ordered Scale Data to Disk
es <- write.xlsx(x=f, "Ordered_Scaled_Data.xlsx")
es1 <- write.csv(f, "Ordered_Scaled_Data.csv")


# Function that orders all the same poit ,,ids" of all scaled extracted values of vegetation indices for each unigue point ,,id"

## ,,f" is the scaled and ordered data frame from previous steps
## id_p is unique pixel ,,id" in the inpurt data frame f

filter_point_id <- function(f, point_id){
df = f %>% 
dplyr:: filter(point_id==f$point_id) %>% 
group_by(point_id)
FilterDF = df[(duplicated(df$point_id, fromLast = FALSE) | duplicated(df$point_id, fromLast = TRUE)),] %>% 

return(FilterDF)

}

FinalDf = filter_point_id(f)

# Create Standalone Data Frames for Each Unigue ,,point_id" of FinalDf data frame and save them as .csv files
splt <- split(FinalDf, FinalDf$point_id)
export <- lapply(names(splt), function(x){
  write.csv(splt[[x]], paste(x, ".csv", sep=""))
})



