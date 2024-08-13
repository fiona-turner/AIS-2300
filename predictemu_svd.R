
if(FAIR2){
  ## load new FAIR data
  f2path = '~/Documents/Emulators/Data/FAIR_data/'
  ssp <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")
  FORpred = NULL
  for( s in ssp){
    #ncin <- ncdf4::nc_open(Sys.glob(filef.path(f2path, s, "*.nc")))
    ncin <- ncdf4::nc_open(paste0(f2path,s,".temperature.fair.temperature_climate.nc"))
    st <- as.data.frame(ncdf4::ncvar_get(ncin, paste0(s, "/surface_temperature")))
    st  <- cbind(rep(s, dim(st)[1]), st)
    rbind(FORpred,data.frame(st))->FORpred
  }
  year <- ncdf4::ncvar_get(ncin,"year")
  colnames(FORpred) <- c('scenario',paste0("y", year))
  FORpred$ystart <- rowMeans(subset(FORpred,select=y2015:y2044))
  FORpred$yend <- rowMeans(subset(FORpred,select=y2271:y2300))
  FORpred[, GSAT_2300 := (FORpred$yend-FORpred$ystart)]
  tmp <- FORpred[scenario == 'ssp119'] 
 } else {
  ## or IPCC FAIR data
  fpath <- "~/Documents/Emulators/Data/"
  FORpred <- fread(file.path(fpath, "CLIMATE_FORCING_IPCC_AR6_230706.csv"))
  FORpred$ystart <- rowMeans(subset(FORpred,select=y2015:y2044))
  FORpred$yend <- rowMeans(subset(FORpred,select=y2271:y2300))
  FORpred[, GSAT_2300 := (FORpred$yend-FORpred$ystart)]
  tmp <- FORpred[scenario == 'SSP119'] 
}


GSAT_2300 <- sample(tmp$GSAT_2300, 5000, TRUE)
GSAT_119 <- GSAT_2300
simoc <- sample(X$simoc, length(GSAT_2300), TRUE)
init_atmos <- sample(X$init_atmos, length(GSAT_2300), TRUE)
lapse_rate <- runif(length(GSAT_2300), -12, -5)
refreeze <- runif(length(GSAT_2300), 0, 15)
refreeze_frac <- runif(length(GSAT_2300), 0.2, 0.8)
PDD_ice <- runif(length(GSAT_2300), 4, 12)
PDD_snow <- runif(length(GSAT_2300), 0, 6)
melt_param <- sample(X$melt_param, length(GSAT_2300), TRUE)

heat_flux_Burgard <- runif(length(GSAT_2300), 1*10**-4, 10*10**-4)
heat_flux_ISMIP6_nonlocal <- runif(length(GSAT_2300), 1*10**4, 4*10**4)
heat_flux_ISMIP6_nonlocal_slope <- runif(length(GSAT_2300), 1*10**6, 4*10**6)
heat_flux_PICO <- runif(length(GSAT_2300), 0.1*10**-5, 10*10**-5)
heat_flux_Plume <- runif(length(GSAT_2300), 1*10**-4, 10*10**-4)

#heat_flux_Burgard <- rep(0, length(GSAT_2300))
#heat_flux_ISMIP6_nonlocal <- rep(0, length(GSAT_2300))
#heat_flux_ISMIP6_nonlocal_slope <- rep(0, length(GSAT_2300))
#heat_flux_PICO <- rep(0, length(GSAT_2300))
#heat_flux_Plume <- rep(0, length(GSAT_2300))


#for (i in 1:length(melt_param)){
#  if (as.character(melt_param[i]) == "Burgard"){
#    heat_flux_Burgard[i] = runif(1, 1*10**-4, 10*10**-4)
#  } 
#  else{ heat_flux_Burgard[i] = 5*10**-4}
#  if (as.character(melt_param[i]) == "ISMIP6_nonlocal"){
#    heat_flux_ISMIP6_nonlocal[i] = runif(1, 1*10**4, 4*10**4)
#  } 
#  else{ heat_flux_ISMIP6_nonlocal[i] = 1.45*10**4}
#  if (as.character(melt_param[i]) == "ISMIP6_nonlocal_slope"){
#    heat_flux_ISMIP6_nonlocal_slope[i] = runif(1, 1*10**6, 4*10**6)
#  } 
#  else{ heat_flux_ISMIP6_nonlocal_slope[i] = 2.06*10**6}
#  if (as.character(melt_param[i]) == "PICO"){
#      heat_flux_PICO[i] = runif(1, 0.1*10**-5, 10*10**-5)
#  } 
#  else if (as.character(melt_param[i]) != "PICO" & as.character(simoc[i]) == "PISM_Reese"){
#    heat_flux_PICO[i] = 7*10**-5}
#  else {heat_flux_PICO[i] = 4*10**-5}
#  if (as.character(melt_param[i]) == "Plume"){
#    heat_flux_Plume[i] = runif(1, 1*10**-4, 10*10**-4)
#  } 
#  else{ heat_flux_Plume[i] = 5.9*10**-4}
#}


message("** SSP119")

pred119 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                 heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred119 <- rbind(X[1, ] , pred119)
pred119 <- pred119[-1,]

emu119_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred119, type = "moments")}) 
SLE119_mean <- matrix( unlist(lapply(emu119_mom, function(j) j[c('mean')])), ncol=r)
SLE119_sd <- matrix( unlist(lapply(emu119_mom, function(j) j[c('sd')])), ncol=r)

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


message("** SSP126")

if (FAIR2){
  tmp <- FORpred[scenario == 'ssp126'] 
} else{
  tmp <- FORpred[scenario == 'SSP126'] 
}

GSAT_2300 <- sample(tmp$GSAT_2300, 5000, TRUE)
GSAT_126 <- GSAT_2300

pred126 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred126 <- rbind(X[1, ] , pred126)
pred126 <- pred126[-1,]
#do I want to predict moments or intervals?

emu126_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred126, type = "moments")})
SLE126_mean <- matrix( unlist(lapply(emu126_mom, function(j) j[c('mean')])), ncol=r)
SLE126_sd <- matrix( unlist(lapply(emu126_mom, function(j) j[c('sd')])), ncol=r)

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

if (FAIR2){
  tmp <- FORpred[scenario == 'ssp245']
} else{
  tmp <- FORpred[scenario == 'SSP245'] 
}

GSAT_2300 <- sample(tmp$GSAT_2300, 5000, TRUE)
GSAT_245 <- GSAT_2300

pred245 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred245 <- rbind(X[1, ] , pred245)
pred245 <- pred245[-1,]
emu245_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred245, type = "moments")})
SLE245_mean <- matrix( unlist(lapply(emu245_mom, function(j) j[c('mean')])), ncol=r)
SLE245_sd <- matrix( unlist(lapply(emu245_mom, function(j) j[c('sd')])), ncol=r)

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

if (FAIR2){
  tmp <- FORpred[scenario == 'ssp370']
} else{
  tmp <- FORpred[scenario == 'SSP370'] 
}

GSAT_2300 <- sample(tmp$GSAT_2300, 5000, TRUE)
GSAT_370 <- GSAT_2300

pred370 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred370 <- rbind(X[1, ] , pred370)
pred370 <- pred370[-1,]
emu370_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred370, type = "moments")})
SLE370_mean <- matrix( unlist(lapply(emu370_mom, function(j) j[c('mean')])), ncol=r)
SLE370_sd <- matrix( unlist(lapply(emu370_mom, function(j) j[c('sd')])), ncol=r)

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

if (FAIR2){
  tmp <- FORpred[scenario == 'ssp585']
} else{
  tmp <- FORpred[scenario == 'SSP585'] 
}

GSAT_2300 <- sample(tmp$GSAT_2300, 5000, TRUE)
GSAT_585 <- GSAT_2300

pred585 <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                              heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
pred585 <- rbind(X[1, ] , pred585)
pred585 <- pred585[-1,]
emu585_mom <- lapply(1L:r, function(j) {predict(emu[[j]], pred585, type = "moments")})
SLE585_mean <- matrix( unlist(lapply(emu585_mom, function(j) j[c('mean')])), ncol=r)
SLE585_sd <- matrix( unlist(lapply(emu585_mom, function(j) j[c('sd')])), ncol=r)

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


message("creating distributions")

#create distributions
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

colnames(distn119) <- paste("y", years, sep = "")
colnames(distn126) <- paste("y", years, sep = "")
colnames(distn245) <- paste("y", years, sep = "")
colnames(distn370) <- paste("y", years, sep = "")
colnames(distn585) <- paste("y", years, sep = "")


write.csv(distn119, "Distributions/distn119.csv", row.names = FALSE)
write.csv(distn126, "Distributions/distn126.csv", row.names = FALSE)
write.csv(distn245, "Distributions/distn245.csv", row.names = FALSE)
write.csv(distn370, "Distributions/distn370.csv", row.names = FALSE)
write.csv(distn585, "Distributions/distn585.csv", row.names = FALSE)


quantile(distn119[,70], c(0.05, 0.167, 0.5, 0.833, 0.95))
quantile(distn126[,70], c(0.05, 0.167, 0.5, 0.833, 0.95))
quantile(distn245[,70], c(0.05, 0.167, 0.5, 0.833, 0.95))
quantile(distn370[,70], c(0.05, 0.167, 0.5, 0.833, 0.95))
quantile(distn585[,70], c(0.05, 0.167, 0.5, 0.833, 0.95))


quant119 <- apply(distn119, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
quant126 <- apply(distn126, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
quant245 <- apply(distn245, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
quant370 <- apply(distn370, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
quant585 <- apply(distn585, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))

colnames(quant119) <- paste("y", years, sep = "")
colnames(quant126) <- paste("y", years, sep = "")
colnames(quant245) <- paste("y", years, sep = "")
colnames(quant370) <- paste("y", years, sep = "")
colnames(quant585) <- paste("y", years, sep = "")


write.csv(quant119, "Distributions/quant119.csv", row.names = TRUE)
write.csv(quant126, "Distributions/quant126.csv", row.names = TRUE)
write.csv(quant245, "Distributions/quant245.csv", row.names = TRUE)
write.csv(quant370, "Distributions/quant370.csv", row.names = TRUE)
write.csv(quant585, "Distributions/quant585.csv", row.names = TRUE)


