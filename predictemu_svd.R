setwd("~/")
## run the emulator code
source("jointemu_svd.R")

## to save distributions of predictions, set this to TRUE
save_pred <- FALSE

## load the FAIR data we use for predictions and process
## set fpath to whereever you store the data files
fpath <- "~/Data/"
FORpred <- fread(file.path(fpath, "CLIMATE_FORCING_IPCC_AR6_230706.csv"))
FORpred$ystart <- rowMeans(subset(FORpred,select=y2015:y2044))
FORpred$yend <- rowMeans(subset(FORpred,select=y2271:y2300))
FORpred[, GSAT_2300 := (FORpred$yend-FORpred$ystart)]

## we set up a predictive dataset for SSP119 first
## variables must have the same names as those used to build the emulator
## so we need the GSAT column to be named GSAT_2300
## we create a copy called GSAT_119 for later plots
tmp <- FORpred[scenario == 'SSP119']
GSAT_2300 <- sample(tmp$GSAT_2300, 5000, TRUE)
GSAT_119 <- GSAT_2300

## continuous variables are sampled uniformly from their ranges
## factor variables are randomly sampled from the categories
simoc <- sample(unique(X$simoc), length(GSAT_2300), TRUE)
init_atmos <- sample(unique(X$init_atmos), length(GSAT_2300), TRUE)
lapse_rate <- runif(length(GSAT_2300), -12, -5)
refreeze <- runif(length(GSAT_2300), 0, 15)
refreeze_frac <- runif(length(GSAT_2300), 0.2, 0.8)
PDD_ice <- runif(length(GSAT_2300), 4, 12)
PDD_snow <- runif(length(GSAT_2300), 0, 6)
melt_param <- sample(unique(X$melt_param), length(GSAT_2300), TRUE)

## heat flux parameterisation values are nested in choice of melt param
heat_flux_Burgard <- rep(0, length(GSAT_2300))
heat_flux_ISMIP6_nonlocal <- rep(0, length(GSAT_2300))
heat_flux_ISMIP6_nonlocal_slope <- rep(0, length(GSAT_2300))
heat_flux_PICO <- rep(0, length(GSAT_2300))
heat_flux_Plume <- rep(0, length(GSAT_2300))

## we set heat flux to nominal value if relevant melt_param is not chosen
## and sample uniformly from the range when it is
for (i in 1:length(melt_param)){
  if (as.character(melt_param[i]) == "Burgard"){
    heat_flux_Burgard[i] = runif(1, 1*10**-4, 10*10**-4)
  } 
  else{ heat_flux_Burgard[i] = 5*10**-4}
  if (as.character(melt_param[i]) == "ISMIP6_nonlocal"){
    heat_flux_ISMIP6_nonlocal[i] = runif(1, 1*10**4, 4*10**4)
  } 
  else{ heat_flux_ISMIP6_nonlocal[i] = 1.45*10**4}
  if (as.character(melt_param[i]) == "ISMIP6_nonlocal_slope"){
    heat_flux_ISMIP6_nonlocal_slope[i] = runif(1, 1*10**6, 4*10**6)
  } 
  else{ heat_flux_ISMIP6_nonlocal_slope[i] = 2.06*10**6}
  if (as.character(melt_param[i]) == "PICO"){
    heat_flux_PICO[i] = runif(1, 0.1*10**-5, 10*10**-5)
  } 
  else{ heat_flux_PICO[i] = sample(c(4*10**-5, 7*10**-5),1)}
  if (as.character(melt_param[i]) == "Plume"){
    heat_flux_Plume[i] = runif(1, 1*10**-4, 10*10**-4)
  } 
  else{ heat_flux_Plume[i] = 5.9*10**-4}
}



## we create a data frame for the data used to build predictions
## we predict from both emulators, using the 'moments' type to give mean and sd
## output is then transformed to SLE time series dimensions
## (70 5-year time slices)
message("** SSP119")

pred119 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                 heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred119 <- rbind(X[1, ] , pred119)
pred119 <- pred119[-1,]

emu119_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred119, type = "moments")}) 
SLE119_mean <- matrix( unlist(lapply(emu119_mom, function(j) j[c('mean')])), ncol=2)
SLE119_sd <- matrix( unlist(lapply(emu119_mom, function(j) j[c('sd')])), ncol=2)

SLE119_meanx <- sweep(SLE119_mean %*% Vt, 2L, cc, "+")
SLE119_sdx <- t(sapply(1L:nrow(SLE119_sd), function(i) {
  sqrt(colSums((SLE119_sd[i, ] * Vt)^2)) # n vector
}))

SLE119_varx <- lapply(1L:nrow(SLE119_sd), function(i) {
  as.vector(crossprod(SLE119_sd[i, ] * Vt)) 
})
SLE119_varx <- do.call("cbind", SLE119_varx) 
dim(SLE119_varx) <- c(n, n, nrow(SLE119_sd))
SLE119_varx <- aperm(SLE119_varx, c(3, 1, 2)) 



## this is then done for the four other SSPs, with GSAT being replaced with sampling from FAIR
message("** SSP126")

tmp <- FORpred[scenario == 'SSP126']
GSAT_2300 <- sample(tmp$y2299, 5000, TRUE)
GSAT_126 <- GSAT_2300

pred126 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred126 <- rbind(X[1, ] , pred126)
pred126 <- pred126[-1,]

emu126_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred126, type = "moments")})
SLE126_mean <- matrix( unlist(lapply(emu126_mom, function(j) j[c('mean')])), ncol=2)
SLE126_sd <- matrix( unlist(lapply(emu126_mom, function(j) j[c('sd')])), ncol=2)

SLE126_meanx <- sweep(SLE126_mean %*% Vt, 2L, cc, "+")
SLE126_sdx <- t(sapply(1L:nrow(SLE126_sd), function(i) {
  sqrt(colSums((SLE126_sd[i, ] * Vt)^2)) # n vector
}))

SLE126_varx <- lapply(1L:nrow(SLE126_sd), function(i) {
  as.vector(crossprod(SLE126_sd[i, ] * Vt)) 
})
SLE126_varx <- do.call("cbind", SLE126_varx) 
dim(SLE126_varx) <- c(n, n, nrow(SLE126_sd))
SLE126_varx <- aperm(SLE126_varx, c(3, 1, 2)) 



message("** SSP245")

tmp <- FORpred[scenario == 'SSP245']
GSAT_2300 <- sample(tmp$y2299, 5000, TRUE)
GSAT_245 <- GSAT_2300

pred245 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred245 <- rbind(X[1, ] , pred245)
pred245 <- pred245[-1,]
emu245_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred245, type = "moments")})
SLE245_mean <- matrix( unlist(lapply(emu245_mom, function(j) j[c('mean')])), ncol=2)
SLE245_sd <- matrix( unlist(lapply(emu245_mom, function(j) j[c('sd')])), ncol=2)

SLE245_meanx <- sweep(SLE245_mean %*% Vt, 2L, cc, "+")
SLE245_sdx <- t(sapply(1L:nrow(SLE245_sd), function(i) {
  sqrt(colSums((SLE245_sd[i, ] * Vt)^2)) # n vector
}))

SLE245_varx <- lapply(1L:nrow(SLE245_sd), function(i) {
  as.vector(crossprod(SLE245_sd[i, ] * Vt)) 
})
SLE245_varx <- do.call("cbind", SLE245_varx) 
dim(SLE245_varx) <- c(n, n, nrow(SLE245_sd))
SLE245_varx <- aperm(SLE245_varx, c(3, 1, 2)) 



message("** SSP370")

tmp <- FORpred[scenario == 'SSP370']
GSAT_2300 <- sample(tmp$y2299, 5000, TRUE)
GSAT_370 <- GSAT_2300

pred370 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred370 <- rbind(X[1, ] , pred370)
pred370 <- pred370[-1,]
emu370_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred370, type = "moments")})
SLE370_mean <- matrix( unlist(lapply(emu370_mom, function(j) j[c('mean')])), ncol=2)
SLE370_sd <- matrix( unlist(lapply(emu370_mom, function(j) j[c('sd')])), ncol=2)

SLE370_meanx <- sweep(SLE370_mean %*% Vt, 2L, cc, "+")
SLE370_sdx <- t(sapply(1L:nrow(SLE370_sd), function(i) {
  sqrt(colSums((SLE370_sd[i, ] * Vt)^2)) # n vector
}))

SLE370_varx <- lapply(1L:nrow(SLE370_sd), function(i) {
  as.vector(crossprod(SLE370_sd[i, ] * Vt)) 
})
SLE370_varx <- do.call("cbind", SLE370_varx) 
dim(SLE370_varx) <- c(n, n, nrow(SLE370_sd))
SLE370_varx <- aperm(SLE370_varx, c(3, 1, 2)) 



message("** SSP585")

tmp <- FORpred[scenario == 'SSP585']
GSAT_2300 <- sample(tmp$y2299, 5000, TRUE)
GSAT_585 <- GSAT_2300

pred585 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred585 <- rbind(X[1, ] , pred585)
pred585 <- pred585[-1,]
emu585_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred585, type = "moments")})
SLE585_mean <- matrix( unlist(lapply(emu585_mom, function(j) j[c('mean')])), ncol=2)
SLE585_sd <- matrix( unlist(lapply(emu585_mom, function(j) j[c('sd')])), ncol=2)

SLE585_meanx <- sweep(SLE585_mean %*% Vt, 2L, cc, "+")
SLE585_sdx <- t(sapply(1L:nrow(SLE585_sd), function(i) {
  sqrt(colSums((SLE585_sd[i, ] * Vt)^2)) # n vector
}))

SLE585_varx <- lapply(1L:nrow(SLE585_sd), function(i) {
  as.vector(crossprod(SLE585_sd[i, ] * Vt)) 
})
SLE585_varx <- do.call("cbind", SLE585_varx) 
dim(SLE585_varx) <- c(n, n, nrow(SLE585_sd))
SLE585_varx <- aperm(SLE585_varx, c(3, 1, 2)) 



## we create a sample for each SSP by sampling from the multivariate normal distribution
## using the mean vectors and variances matrices predicted by the emulators
library(MASS)
distn119 <- matrix(rep(0, dim(pred126)[1]*dim(ave)[2]), nrow = dim(pred126)[1], ncol = dim(ave)[2])
distn126 <- matrix(rep(0, dim(pred126)[1]*dim(ave)[2]), nrow = dim(pred126)[1], ncol = dim(ave)[2])
distn245 <- matrix(rep(0, dim(pred126)[1]*dim(ave)[2]), nrow = dim(pred126)[1], ncol = dim(ave)[2])
distn370 <- matrix(rep(0, dim(pred126)[1]*dim(ave)[2]), nrow = dim(pred126)[1], ncol = dim(ave)[2])
distn585 <- matrix(rep(0, dim(pred126)[1]*dim(ave)[2]), nrow = dim(pred126)[1], ncol = dim(ave)[2])

for(i in 1L:dim(SLE119_meanx)[1]){
  distn119[i,] <- mvrnorm(1, SLE119_meanx[i,], SLE119_varx[i,,])  
}
for(i in 1L:dim(SLE126_meanx)[1]){
  distn126[i,] <- mvrnorm(1, SLE126_meanx[i,], SLE126_varx[i,,])  
}
for(i in 1L:dim(SLE245_meanx)[1]){
  distn245[i,] <- mvrnorm(1, SLE245_meanx[i,], SLE245_varx[i,,])  
}
for(i in 1L:dim(SLE370_meanx)[1]){
  distn370[i,] <- mvrnorm(1, SLE370_meanx[i,], SLE370_varx[i,,])  
}
for(i in 1L:dim(SLE585_meanx)[1]){
  distn585[i,] <- mvrnorm(1, SLE585_meanx[i,], SLE585_varx[i,,])  
}

## save the distributions if we wish to
if (save_pred){
  write.csv(distn119, "distn119.csv", row.names = FALSE)
  write.csv(distn126, "distn126.csv", row.names = FALSE)
  write.csv(distn245, "distn245.csv", row.names = FALSE)
  write.csv(distn370, "Ddistn370.csv", row.names = FALSE)
  write.csv(distn585, "distn585.csv", row.names = FALSE)
}
