## From Ines:
## To calculate the uncertainties on the five-year averages from Table 2, 
## we take the mean error and divide it by the square root of the number of years: 
## for example for 1992-1996, the error is calculated as: 
## MEAN(mb_sigma[ok_time])/SQRT(delta_years) = 88 / 2.2 = 40


IMBIE <- fread("./calibration-data/IMBIE/2022/imbie_antarctica_2022_Gt.csv")

## set relative to 2000 to match our model
IMBIE$`Mass balance (Gt/yr)` <- IMBIE$`Mass balance (Gt/yr)` - mean(IMBIE$`Mass balance (Gt/yr)`[IMBIE$Year >= 2000 & IMBIE$Year < 2001])

## set up bins for averaging over the five year our model uses
post <- 1975 + c(5, seq(from = 10, to = 45, by = 5))
kk <- length(post) - 1 # number of bins

bins <- findInterval(IMBIE$Year, post, rightmost.closed = TRUE)

## means of obs within relative bins
obs <- sapply(1L:kk, function(i) {
  mean(IMBIE$`Mass balance (Gt/yr)`[ bins == i,drop=FALSE])
})

## using Ines' uncertainty calculation: mean(uncertainty)/sqrt(no. of years)
sig <- sapply(1L:kk, function(i) {
  mean(IMBIE$`Mass balance uncertainty (Gt/yr)`[ bins == i,drop=FALSE])/(sqrt(length(bins[bins == i])/12))
})

## convert from Gt SLE to m SLE
obs <- obs/(362.5*1000)
sig <- sig/(362.5*1000)