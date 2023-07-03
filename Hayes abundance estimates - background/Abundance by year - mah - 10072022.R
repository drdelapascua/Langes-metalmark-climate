### Abundance by year for LMB
### Mark A. Hayes
### 1/5/2023
###
###

## Preliminaries

# rm(list=ls()) # Clear the global environment as needed
# help(list) # as needed

## Load dataframe
## Using "Abundance by year - mah - 10072022.csv"
 

data = read.table(file.choose(), header=TRUE, sep=",")
attach(data)
summary(data)

## Plots & histograms

plot(Year, N)
plot(Year, N, type = "b", pch = 19, col = "red", 
     main = "LMB Abundance Estimate (1986-2022)",xlab = "Year", ylab = "Abundance Estimate")

dev.print(tiff, "abundance_plot.tiff", height=4, width=6, units='in', res=300)



plot(Year, r)
plot(Year, r, type = "b", pch = 19, col = "blue", 
     main = "LMB Pop Growth Rate (1986-2022)",xlab = "Year", 
     ylab = "Growth Rate (R)")
dev.print(tiff, "r_plot.tiff", height=4, width=6, units='in', res=300)

hist(N)
# help(hist)
hist(N, breaks = 20)

## This looks Poisson-ish, but with breaks in the histogram and a major
## outlier. Will need to look into how to deal with this. 

dev.print(tiff, "abundance_hist.tiff", height=4, width=6, units='in', res=300)

## END
