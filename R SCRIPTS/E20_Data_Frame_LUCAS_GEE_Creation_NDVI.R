# This function manipulates dataframe for the Change Detection Analysis of LUCAS Points

library(anytime)
library(rChoiceDialogs)
library(xlsx)
library(dplyr)
library(data.table)
library(corrplot)
library(xlsx)
library(caret)

# Define path to a working directory in your system
vstupni_adresar <- rchoose.dir(getwd(), "Set path to a working directory")
prac_adresar <- setwd(vstupni_adresar)

# Choose Exported Data From Google  Earth Engine
A = read.csv(rchoose.files(getwd(), "Load .CSV Exported from GEE"))

# Define a function to transform Data
transform_df <- function(x=A){
  a <- as.data.frame(substring(x$system.index, 15, 22)) # cut string that contains date information
  b <- cbind(a, subset(x, select=c("DVI", "GDVI", "GRVI", "IPVI", 
                                       "MNLI", "MSR", "NDVI", "NLI", "OSAVI", "RDVI", "SAVI",
                                       "SR", "TVI", "transect_2", "point_id"))) # select columns for new data frame
  
  
  
  c <- colnames(b)[colnames(b) == "substring(x$system.index, 15, 22)"] <- "Time" # change the name of time
  d <- anytime::anydate(b$Time)
  e <- cbind(d, b)
  f <- colnames(e)[colnames(e) == "d"] <- "Time"
  g <- e[order(as.Date(e$Time, format="%Y/%m/%m")),] # Order Data Frame by Date
  h <- g[g$Time >= "2006-01-01" & g$Time <="2015-12-30",] # Subset Data Frame from 2006 to 2015
  i <- subset(h, select=c("point_id", "Time", "DVI", "GDVI", "GRVI", "IPVI", 
                                   "MNLI", "MSR", "NDVI", "NLI", "OSAVI", "RDVI", "SAVI",
                                   "SR", "TVI", "transect_2"))
  return(i)
}

# Execute function in order to transform the data
B = transform_df(A)

# Export Transformed Data to Microsoft Excel Format
C = write.xlsx(x=B, "Transformed_Data.xlsx")
############################################################################################

# Scale original data for Plot

# Scale Function
sc <- preProcess(as.data.frame(B[,3:15]), method=c("range"))
  
# Scale Data itself
ds <- predict(sc, as.data.frame(B))

# Export Scale Data to Disk
es <- write.xlsx(x=ds, "Scaled_Data.xlsx")
####################################################

# Scale Data 2
sf <- function(x){
  (x - min(x))/(max(x) - min(x))
}

no <- as.data.frame(lapply(B[,3:15],sf))
sp <- write.xlsx(x=no, "Scaled_Data2.xlsx")
####################################################

# Order Data with the same ,,point_id"
f <- ds[with(ds,order(point_id)),]
f

# Export Ordered Scale Data to Disk
es <- write.xlsx(x=f, "Ordered_Scaled_Data.xlsx")
es1 <- write.csv(f, "Ordered_Scaled_Data.csv")

# Subset Data Frame With the Unigue ID for five random pixels
a = f %>% filter(point_id==46943076)
b = f %>% filter(point_id==46963006)
c = f %>% filter(point_id==47883020)
d = f %>% filter(point_id==46182918)
e = f %>% filter(point_id==45462976)

# Save All Filtered ID of f data frame to disk in the MS Excel Format
a1 <- write.xlsx(x=a, "a.xlsx")
b1 <- write.xlsx(x=b, "b.xlsx")
c1 <- write.xlsx(x=c, "c.xlsx")
d1 <- write.xlsx(x=d, "d.xlsx")
e1 <- write.xlsx(x=e, "e.xlsx")

# Save All Filtered ID of f data frame to disk in the MS Excel Format
a2 <- write.csv(x=a, "a.csv")
b2 <- write.csv(x=b, "b.csv")
c2 <- write.csv(x=c, "c.csv")
d2<- write.csv(x=d, "d.csv")
e2 <- write.csv(x=e, "e.csv")

# Add Correlation Matrix

M <- as.matrix(f[,3:15])
C <- cor(M, use="na.or.complete")
P <- corrplot(C, method="color", addCoef.col = "black")

# Export Correlation Matrix to HDD
jpeg(filename="Correlation_Matrix_Vegetation_Indices.jpeg", units="px", width=3000, height=3000, res=300)
corrplot(C, method="color", addCoef.col = "black")
dev.off()

