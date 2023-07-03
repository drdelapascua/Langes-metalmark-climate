### Phenology by year for LMB
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

## Plots

par(mfrow = c(1,3))

plot(Year, First, ylab = "Day", xlab = "", main = "First Day", ylim = c(200,300))
plot(Year, Peak, xlab = "Year", ylab = "", main = "Peak Day", ylim = c(200,300))
plot(Year, Last, xlab = "", ylab = "", main = "Last Day", ylim = c(200,300))

dev.print(tiff, "phenology_plot.tiff", height=4, width=6, units='in', res=300)

par(mfrow = c(1,3))

plot(Year, Duration, xlab = "Year", ylab = "Days", main = "Flight Period")
plot(Duration, N, main = "Flight Period vs. Abunadance")
plot(Duration, r, main = "r vs. Abunadance")

dev.print(tiff, "flight_period_plots.tiff", height=4, width=6, units='in', res=300)




## END
