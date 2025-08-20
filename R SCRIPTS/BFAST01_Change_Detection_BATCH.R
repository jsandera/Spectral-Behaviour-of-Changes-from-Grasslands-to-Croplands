# This Script implements BFAST algorithm for single pixel of Satellite Time Series Data

library(bfast)
library(bfastSpatial)
library(xts)
library(zoo)
library(forecast)
library(rChoiceDialogs)
library(xlsx)
library(TSstudio)
library(tsbox)

# Define path to a working directory in your system
vstupni_adresar <- rchoose.dir(getwd(), "Set path to a working directory")
prac_adresar <- setwd(vstupni_adresar)

# Create a File List of All Pixels .CSV Files
file_list <- list.files(path=getwd(), pattern=".csv", recursive=FALSE)


# Start of For Loop

for(i in file_list){

# Choose Exported Data From Google  Earth Engine (Save Original .xlsx file as .csv)
A = read.csv(i, header=T)

## Data Preparation

# Subset Input Dataset
dat <- as.matrix(A[,2:4])

# Create Regular Time Series Dataset with NA
ts_reg <- bfastts(data=dat, dates=dat[,2], type="irregular")

# Interpolate Missing Values in Time series Object
ip <- na.approx(ts_reg[,3])
ip

# Detect Breakpoints with Bfast Algorithm (Daily Time Series)
t <- ts(as.numeric(ip), frequency=365, start=c(2006, 1), end=c(2015, 365))

# Estimate Only One Change in Daily Time Series
bf1 <- bfast01(t)
plot(bf1)

# Save bfast01 Plot to Disk
pat1 <- gsub(pattern = "\\.csv$", "", i)
jmeno_bfast_daily <- paste("BFAST01_DAILY_", pat1, ".jpeg")
bf1_plot <- jpeg(filename=jmeno_bfast_daily, units="px", width=4000, height=4000, res=600)
plot(bf1)
dev.off()

# Convert ts Object to xts and aggregate it to monthly Time Series
t_xts <- ts_xts(t)
ts_monthly <- apply.monthly(t_xts, FUN=median)
tes <- xts_to_ts(ts_monthly, frequency=12)

# Estimate Only One Change in Monthly Time Series
bf2 <- bfast01(tes[,1])
plot(bf2)

# Save bfast01 Plot to Disk (Monthly Time Series)
pat2 <- gsub(pattern = "\\.csv$", "", i)
jmeno_bfast_monthly <- paste("BFAST01_MONTHLY_", pat2, ".jpeg")
bf2_plot <- jpeg(filename=jmeno_bfast_monthly, units="px", width=4000, height=4000, res=600)
plot(bf2)
dev.off()


# Capture output from BFAST01 Daily and Detect Type of Change
b_fast01C <- bfast01classify(bf1)
b_fast01D <- capture.output(b_fast01C)
pat3 <- gsub(pattern = "\\.csv$", "", i)
bf01d <- paste("BP_Daily_BFAST01_", pat3, ".txt")
b_fast01DE <- cat(b_fast01D, file=bf01d, sep="\n", append=FALSE)

# Capture output from BFAST01 Monthly
b_fast02C <- bfast01classify(bf2)
b_fast01M <- capture.output(b_fast02C)
pat4 <- gsub(pattern = "\\.csv$", "", i)
bf02d <- paste("BP_Monthly_BFAST01_", pat4, ".txt")
b_fast01ME <- cat(b_fast01M, file=bf02d, sep="\n", append=FALSE)
}



