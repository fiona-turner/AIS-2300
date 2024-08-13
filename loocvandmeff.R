############################# LOOCV #################################


LOO_moments <- function(emu, subset = "all") {
  
  stopifnot(inherits(emu, "qemu"))
  X <- emu$Runs$X
  y <- emu$Runs$y
  n <- nrow(X)
  if (subset[1L] == "all") {
    subset <- 1L:n
  } else {
    stopifnot(subset %in% 1:n, !duplicated(subset))
  }
  
  args <- emu$args

    
    ## eval each one in series
    
    robj <- lapply(subset, function(i) {
      foo <- do.call("make_qemu", c(alist(
        X = X[-i, , drop=FALSE], y = y[-i], fmla = args$fmla,
        inlogs = args$inlogs, offset = args$offset,
        nthreads = args$nthreads, mtry = args$mtry,
        nodesize = args$nodesize), emu$moreargs))
      predict(foo, X[i, , drop=FALSE], type = "moments") # 1-row DF with mean and sd
    })
    robj <- do.call("rbind", robj) # one DF
    
  
  robj <- data.frame(
    actual = y[subset],
    mean = robj$mean,
    sd = robj$sd)
  attr(robj, "X") <- X[subset, , drop=FALSE]
  class(robj) <- c("LOO", class(robj))
  
  robj
}

message("running loocv")

oo <- order(X$GSAT_2300)[seq(from = 1, to = dim(y)[1], length.out = 100)]

loo <- lapply(1L:r, function(j) { LOO_moments(emu[[j]], subset = oo)})
cv_mean <- sapply(loo, "[[", "mean") 
cv_sd <- sapply(loo, "[[", "sd")

## save the actual time series values
sim <- ave[oo,]
##transform loo output to time series
cv_meanx <- sweep(cv_mean %*% Vt, 2L, cc, "+")
cv_sdx <- t(sapply(1L:nrow(cv_sd), function(i) {
  sqrt(colSums((cv_sd[i, ] * Vt)^2)) # n vector
}))

rmse <- lapply(1L:n, function(j) { sqrt(mean((cv_meanx[,j] - sim[,j])^2))})

## want loocv plots at 2100, 2150, 2200, 2300
## time slice index is years[:,c(30, 40, 50, 70)]
tidx <- c(30, 40, 50, 70)

for(t in tidx){
  wrong <- sim[order(sim[,t]),t] < (cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t]) | sim[order(sim[,t]),t] > (cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t])
  col_dots <- rep("deepskyblue4", length(sim[order(sim[,t]),t]))
  col_wrong <- rgb(243, 122, 107, maxColorValue = 255)
  col_dots[wrong] <- col_wrong
  plot(sim[order(sim[,t]),t],cv_meanx[order(sim[,t]),t], pch = 19, xlab =paste("Simulated values at ", years[t]," (SLE (m))",sep=""),
      ylab = paste("Emulated values at ", years[t]," (SLE (m))",sep=""), main = " ", col = col_dots, xlim=c(min(cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t]),max(cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t])), ylim=c(min(cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t]),max(cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t])), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  arrows(sim[order(sim[,t]),t], cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t], sim[order(sim[,t]),t], cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t], length=0.05, angle=90, code=3, col = col_dots)
  abline(a = 0, b = 1, lwd = 0.5)
  legend('topleft', legend=c(paste("Coverage: ",100-sum(wrong), "%", sep=""),paste("RMSE:  ", round(as.numeric(rmse[t]),2), "m", sep="")), cex=1.1, bty = "n")
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/svd_qemu_LOO_",years[t],".pdf",sep=""), width=6, height=6)
  }
}


############################# MEFF #################################

message("running meff")


## create nominal values for all variables
## GSAT is set to mean of GCMs
## simoc is set to Kori_ISMIP6_3D
## init_atmos is set to RACMO2.3p2
## lapse_rate is set to -8.2
## refreeze is set to 5
## refreeze frac is set to 0.5
## PDD_ice is set to 8
## PDD_snow is set to 3
## melt_param is set to PICO (only shared melt_param between simus)
## heat_flux_Burgard is set to 5*10**-4
## heat_flux_ISMIP6_nonlocal is set to 1.45*10**4
## heat_flux_ISMIP6_nonlocal_slope is set to 2.06*10**6
## heat_flux_PICO is set to 4*10*-5 (as simoc is Kori_ISMIP6_3D)
## heat_flux_Plume is set to 5.9*10**-4


##decide on size of sample and set here
GSAT_2300_nom <- rep(mean(X$GSAT_2300), 1000)
simoc_nom <- rep(unique(X$simoc)[1], length(GSAT_2300_nom))
init_atmos_nom <- rep(unique(X$init_atmos)[1], length(GSAT_2300_nom))
lapse_rate_nom <- rep(-8.2, length(GSAT_2300_nom))
refreeze_nom <- rep(5, length(GSAT_2300_nom))
refreeze_frac_nom <- rep(0.5, length(GSAT_2300_nom))
PDD_ice_nom <- rep(8, length(GSAT_2300_nom))
PDD_snow_nom <- rep(3, length(GSAT_2300_nom))
melt_param_nom <- rep(unique(X$melt_param)[2], length(GSAT_2300_nom))
heat_flux_Burgard_nom <- rep(5*10**-4, length(GSAT_2300_nom))
heat_flux_ISMIP6_nonlocal_nom <- rep(1.45*10**4, length(GSAT_2300_nom))
heat_flux_ISMIP6_nonlocal_slope_nom <- rep(2.06*10**6, length(GSAT_2300_nom))
heat_flux_PICO_nom <- rep(4*10**-5, length(GSAT_2300_nom))
heat_flux_Plume_nom <- rep(5.9*10**-4, length(GSAT_2300_nom))


## MEFF for GSAT
GSAT_2300_samp <- sort(X$GSAT_2300)[seq(from = 1, to = length(X$GSAT_2300), length.out = length(GSAT_2300_nom))]
predGSAT <- as.list(data.frame(GSAT_2300_samp, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                              heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predGSAT)){
  names(predGSAT)[l] <-  sub("_samp", "", names(predGSAT)[l])
  names(predGSAT)[l] <-  sub("_nom", "", names(predGSAT)[l])
}

MEFF_GSAT <- lapply(1L:r, function(j) {predict(emu[[j]], predGSAT, type = "moments")}) 
MEFF_GSAT_mean <- matrix( unlist(lapply(MEFF_GSAT, function(j) j[c('mean')])), ncol=r)
MEFF_GSAT_sd <- matrix( unlist(lapply(MEFF_GSAT, function(j) j[c('sd')])), ncol=r)

MEFF_GSAT_meanx <- sweep(MEFF_GSAT_mean %*% Vt, 2L, cc, "+")
MEFF_GSAT_sdx <- t(sapply(1L:nrow(MEFF_GSAT_sd), function(i) {
  sqrt(colSums((MEFF_GSAT_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_GSAT_varx <- lapply(1L:nrow(MEFF_GSAT_sd), function(i) {
  as.vector(crossprod(MEFF_GSAT_sd[i, ] * Vt)) 
})
MEFF_GSAT_varx <- do.call("cbind", MEFF_GSAT_varx) 
dim(MEFF_GSAT_varx) <- c(n, n, nrow(MEFF_GSAT_sd))
MEFF_GSAT_varx <- aperm(MEFF_GSAT_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_GSAT_upper <- MEFF_GSAT_meanx + 2*MEFF_GSAT_sdx
MEFF_GSAT_lower <- MEFF_GSAT_meanx - 2*MEFF_GSAT_sdx


## MEFF for simoc
simoc_samp <- sample(X$simoc, length(GSAT_2300_nom), TRUE)
predsimoc <- as.list(data.frame(GSAT_2300_nom, simoc_samp, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predsimoc)){
  names(predsimoc)[l] <-  sub("_samp", "", names(predsimoc)[l])
  names(predsimoc)[l] <-  sub("_nom", "", names(predsimoc)[l])
}

MEFF_simoc <- lapply(1L:r, function(j) {predict(emu[[j]], predsimoc, type = "moments")}) 
MEFF_simoc_mean <- matrix( unlist(lapply(MEFF_simoc, function(j) j[c('mean')])), ncol=r)
MEFF_simoc_sd <- matrix( unlist(lapply(MEFF_simoc, function(j) j[c('sd')])), ncol=r)

MEFF_simoc_meanx <- sweep(MEFF_simoc_mean %*% Vt, 2L, cc, "+")
MEFF_simoc_sdx <- t(sapply(1L:nrow(MEFF_simoc_sd), function(i) {
  sqrt(colSums((MEFF_simoc_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_simoc_varx <- lapply(1L:nrow(MEFF_simoc_sd), function(i) {
  as.vector(crossprod(MEFF_simoc_sd[i, ] * Vt)) 
})
MEFF_simoc_varx <- do.call("cbind", MEFF_simoc_varx) 
dim(MEFF_simoc_varx) <- c(n, n, nrow(MEFF_simoc_sd))
MEFF_simoc_varx <- aperm(MEFF_simoc_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_simoc_upper <- MEFF_simoc_meanx + 2*MEFF_simoc_sdx
MEFF_simoc_lower <- MEFF_simoc_meanx - 2*MEFF_simoc_sdx


## MEFF for init_atmos
init_atmos_samp <- sample(unique(X$init_atmos), length(GSAT_2300_nom), TRUE)
predatmos <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_samp, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predatmos)){
  names(predatmos)[l] <-  sub("_samp", "", names(predatmos)[l])
  names(predatmos)[l] <-  sub("_nom", "", names(predatmos)[l])
}

MEFF_atmos <- lapply(1L:r, function(j) {predict(emu[[j]], predatmos, type = "moments")}) 
MEFF_atmos_mean <- matrix( unlist(lapply(MEFF_atmos, function(j) j[c('mean')])), ncol=r)
MEFF_atmos_sd <- matrix( unlist(lapply(MEFF_atmos, function(j) j[c('sd')])), ncol=r)

MEFF_atmos_meanx <- sweep(MEFF_atmos_mean %*% Vt, 2L, cc, "+")
MEFF_atmos_sdx <- t(sapply(1L:nrow(MEFF_atmos_sd), function(i) {
  sqrt(colSums((MEFF_atmos_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_atmos_varx <- lapply(1L:nrow(MEFF_atmos_sd), function(i) {
  as.vector(crossprod(MEFF_atmos_sd[i, ] * Vt)) 
})
MEFF_atmos_varx <- do.call("cbind", MEFF_atmos_varx) 
dim(MEFF_atmos_varx) <- c(n, n, nrow(MEFF_atmos_sd))
MEFF_atmos_varx <- aperm(MEFF_atmos_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_atmos_upper <- MEFF_atmos_meanx + 2*MEFF_atmos_sdx
MEFF_atmos_lower <- MEFF_atmos_meanx - 2*MEFF_atmos_sdx


## MEFF for lapse_rate
lapse_rate_samp <- sort(runif(length(GSAT_2300_nom), -12, -5))
predlapse <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_samp, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predlapse)){
  names(predlapse)[l] <-  sub("_samp", "", names(predlapse)[l])
  names(predlapse)[l] <-  sub("_nom", "", names(predlapse)[l])
}

MEFF_lapse <- lapply(1L:r, function(j) {predict(emu[[j]], predlapse, type = "moments")}) 
MEFF_lapse_mean <- matrix( unlist(lapply(MEFF_lapse, function(j) j[c('mean')])), ncol=r)
MEFF_lapse_sd <- matrix( unlist(lapply(MEFF_lapse, function(j) j[c('sd')])), ncol=r)

MEFF_lapse_meanx <- sweep(MEFF_lapse_mean %*% Vt, 2L, cc, "+")
MEFF_lapse_sdx <- t(sapply(1L:nrow(MEFF_lapse_sd), function(i) {
  sqrt(colSums((MEFF_lapse_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_lapse_varx <- lapply(1L:nrow(MEFF_lapse_sd), function(i) {
  as.vector(crossprod(MEFF_lapse_sd[i, ] * Vt)) 
})
MEFF_lapse_varx <- do.call("cbind", MEFF_lapse_varx) 
dim(MEFF_lapse_varx) <- c(n, n, nrow(MEFF_lapse_sd))
MEFF_lapse_varx <- aperm(MEFF_lapse_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_lapse_upper <- MEFF_lapse_meanx + 2*MEFF_lapse_sdx
MEFF_lapse_lower <- MEFF_lapse_meanx - 2*MEFF_lapse_sdx


## MEFF for refreeze
refreeze_samp <- sort(runif(length(GSAT_2300_nom), 0, 15))
predrefreeze <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_samp, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predrefreeze)){
  names(predrefreeze)[l] <-  sub("_samp", "", names(predrefreeze)[l])
  names(predrefreeze)[l] <-  sub("_nom", "", names(predrefreeze)[l])
}

MEFF_refreeze <- lapply(1L:r, function(j) {predict(emu[[j]], predrefreeze, type = "moments")}) 
MEFF_refreeze_mean <- matrix( unlist(lapply(MEFF_refreeze, function(j) j[c('mean')])), ncol=r)
MEFF_refreeze_sd <- matrix( unlist(lapply(MEFF_refreeze, function(j) j[c('sd')])), ncol=r)

MEFF_refreeze_meanx <- sweep(MEFF_refreeze_mean %*% Vt, 2L, cc, "+")
MEFF_refreeze_sdx <- t(sapply(1L:nrow(MEFF_refreeze_sd), function(i) {
  sqrt(colSums((MEFF_refreeze_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_refreeze_varx <- lapply(1L:nrow(MEFF_refreeze_sd), function(i) {
  as.vector(crossprod(MEFF_refreeze_sd[i, ] * Vt)) 
})
MEFF_refreeze_varx <- do.call("cbind", MEFF_refreeze_varx) 
dim(MEFF_refreeze_varx) <- c(n, n, nrow(MEFF_refreeze_sd))
MEFF_refreeze_varx <- aperm(MEFF_refreeze_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_refreeze_upper <- MEFF_refreeze_meanx + 2*MEFF_refreeze_sdx
MEFF_refreeze_lower <- MEFF_refreeze_meanx - 2*MEFF_refreeze_sdx


## MEFF for refreeze_frac
refreeze_frac_samp <- sort(runif(length(GSAT_2300_nom), 0.2, 0.8))
predfrac <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_samp, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                   heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predfrac)){
  names(predfrac)[l] <-  sub("_samp", "", names(predfrac)[l])
  names(predfrac)[l] <-  sub("_nom", "", names(predfrac)[l])
}

MEFF_frac <- lapply(1L:r, function(j) {predict(emu[[j]], predfrac, type = "moments")}) 
MEFF_frac_mean <- matrix( unlist(lapply(MEFF_frac, function(j) j[c('mean')])), ncol=r)
MEFF_frac_sd <- matrix( unlist(lapply(MEFF_frac, function(j) j[c('sd')])), ncol=r)

MEFF_frac_meanx <- sweep(MEFF_frac_mean %*% Vt, 2L, cc, "+")
MEFF_frac_sdx <- t(sapply(1L:nrow(MEFF_frac_sd), function(i) {
  sqrt(colSums((MEFF_frac_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_frac_varx <- lapply(1L:nrow(MEFF_frac_sd), function(i) {
  as.vector(crossprod(MEFF_frac_sd[i, ] * Vt)) 
})
MEFF_frac_varx <- do.call("cbind", MEFF_frac_varx) 
dim(MEFF_frac_varx) <- c(n, n, nrow(MEFF_frac_sd))
MEFF_frac_varx <- aperm(MEFF_frac_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_frac_upper <- MEFF_frac_meanx + 2*MEFF_frac_sdx
MEFF_frac_lower <- MEFF_frac_meanx - 2*MEFF_frac_sdx


## MEFF for PDD_ice
PDD_ice_samp <- sort(runif(length(GSAT_2300_nom), 4, 12))
predice <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_samp, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predice)){
  names(predice)[l] <-  sub("_samp", "", names(predice)[l])
  names(predice)[l] <-  sub("_nom", "", names(predice)[l])
}

MEFF_ice <- lapply(1L:r, function(j) {predict(emu[[j]], predice, type = "moments")}) 
MEFF_ice_mean <- matrix( unlist(lapply(MEFF_ice, function(j) j[c('mean')])), ncol=r)
MEFF_ice_sd <- matrix( unlist(lapply(MEFF_ice, function(j) j[c('sd')])), ncol=r)

MEFF_ice_meanx <- sweep(MEFF_ice_mean %*% Vt, 2L, cc, "+")
MEFF_ice_sdx <- t(sapply(1L:nrow(MEFF_ice_sd), function(i) {
  sqrt(colSums((MEFF_ice_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_ice_varx <- lapply(1L:nrow(MEFF_ice_sd), function(i) {
  as.vector(crossprod(MEFF_ice_sd[i, ] * Vt)) 
})
MEFF_ice_varx <- do.call("cbind", MEFF_ice_varx) 
dim(MEFF_ice_varx) <- c(n, n, nrow(MEFF_ice_sd))
MEFF_ice_varx <- aperm(MEFF_ice_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_ice_upper <- MEFF_ice_meanx + 2*MEFF_ice_sdx
MEFF_ice_lower <- MEFF_ice_meanx - 2*MEFF_ice_sdx


## MEFF for PDD_snow
PDD_snow_samp <- sort(runif(length(GSAT_2300_nom), 0, 6))
predsnow <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_samp, melt_param_nom, 
                              heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predsnow)){
  names(predsnow)[l] <-  sub("_samp", "", names(predsnow[l]))
  names(predsnow)[l] <-  sub("_nom", "", names(predsnow[l]))
}

MEFF_snow <- lapply(1L:r, function(j) {predict(emu[[j]], predsnow, type = "moments")}) 
MEFF_snow_mean <- matrix( unlist(lapply(MEFF_snow, function(j) j[c('mean')])), ncol=r)
MEFF_snow_sd <- matrix( unlist(lapply(MEFF_snow, function(j) j[c('sd')])), ncol=r)

MEFF_snow_meanx <- sweep(MEFF_snow_mean %*% Vt, 2L, cc, "+")
MEFF_snow_sdx <- t(sapply(1L:nrow(MEFF_snow_sd), function(i) {
  sqrt(colSums((MEFF_snow_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_snow_varx <- lapply(1L:nrow(MEFF_snow_sd), function(i) {
  as.vector(crossprod(MEFF_snow_sd[i, ] * Vt)) 
})
MEFF_snow_varx <- do.call("cbind", MEFF_snow_varx) 
dim(MEFF_snow_varx) <- c(n, n, nrow(MEFF_snow_sd))
MEFF_snow_varx <- aperm(MEFF_snow_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_snow_upper <- MEFF_snow_meanx + 2*MEFF_snow_sdx
MEFF_snow_lower <- MEFF_snow_meanx - 2*MEFF_snow_sdx


## MEFF for melt_param
melt_param_samp <- sample(X$melt_param, length(GSAT_2300_nom), TRUE)
predmelt <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_samp, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predmelt)){
  names(predmelt)[l] <-  sub("_samp", "", names(predmelt)[l])
  names(predmelt)[l] <-  sub("_nom", "", names(predmelt)[l])
}

MEFF_melt <- lapply(1L:r, function(j) {predict(emu[[j]], predmelt, type = "moments")}) 
MEFF_melt_mean <- matrix( unlist(lapply(MEFF_melt, function(j) j[c('mean')])), ncol=r)
MEFF_melt_sd <- matrix( unlist(lapply(MEFF_melt, function(j) j[c('sd')])), ncol=r)

MEFF_melt_meanx <- sweep(MEFF_melt_mean %*% Vt, 2L, cc, "+")
MEFF_melt_sdx <- t(sapply(1L:nrow(MEFF_melt_sd), function(i) {
  sqrt(colSums((MEFF_melt_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_melt_varx <- lapply(1L:nrow(MEFF_melt_sd), function(i) {
  as.vector(crossprod(MEFF_melt_sd[i, ] * Vt)) 
})
MEFF_melt_varx <- do.call("cbind", MEFF_melt_varx) 
dim(MEFF_melt_varx) <- c(n, n, nrow(MEFF_melt_sd))
MEFF_melt_varx <- aperm(MEFF_melt_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_melt_upper <- MEFF_melt_meanx + 2*MEFF_melt_sdx
MEFF_melt_lower <- MEFF_melt_meanx - 2*MEFF_melt_sdx


## MEFF for heat_flux_PICO
heat_flux_PICO_samp <- sort(runif(length(GSAT_2300_nom), 0.1*10**-5, 10*10**-5))
predPICO <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_samp, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predPICO)){
  names(predPICO)[l] <-  sub("_samp", "", names(predPICO[l]))
  names(predPICO)[l] <-  sub("_nom", "", names(predPICO[l]))
}

MEFF_PICO <- lapply(1L:r, function(j) {predict(emu[[j]], predPICO, type = "moments")}) 
MEFF_PICO_mean <- matrix( unlist(lapply(MEFF_PICO, function(j) j[c('mean')])), ncol=r)
MEFF_PICO_sd <- matrix( unlist(lapply(MEFF_PICO, function(j) j[c('sd')])), ncol=r)

MEFF_PICO_meanx <- sweep(MEFF_PICO_mean %*% Vt, 2L, cc, "+")
MEFF_PICO_sdx <- t(sapply(1L:nrow(MEFF_PICO_sd), function(i) {
  sqrt(colSums((MEFF_PICO_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_PICO_varx <- lapply(1L:nrow(MEFF_PICO_sd), function(i) {
  as.vector(crossprod(MEFF_PICO_sd[i, ] * Vt)) 
})
MEFF_PICO_varx <- do.call("cbind", MEFF_PICO_varx) 
dim(MEFF_PICO_varx) <- c(n, n, nrow(MEFF_PICO_sd))
MEFF_PICO_varx <- aperm(MEFF_PICO_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_PICO_upper <- MEFF_PICO_meanx + 2*MEFF_PICO_sdx
MEFF_PICO_lower <- MEFF_PICO_meanx - 2*MEFF_PICO_sdx


## MEFF for heat_flux_Plume
heat_flux_Plume_samp <- sort(runif(length(GSAT_2300_nom), 1*10**-4, 10*10**-4))
predPlume <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_samp, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predPlume)){
  names(predPlume)[l] <-  sub("_samp", "", names(predPlume[l]))
  names(predPlume)[l] <-  sub("_nom", "", names(predPlume[l]))
}

MEFF_Plume <- lapply(1L:r, function(j) {predict(emu[[j]], predPlume, type = "moments")}) 
MEFF_Plume_mean <- matrix( unlist(lapply(MEFF_Plume, function(j) j[c('mean')])), ncol=r)
MEFF_Plume_sd <- matrix( unlist(lapply(MEFF_Plume, function(j) j[c('sd')])), ncol=r)

MEFF_Plume_meanx <- sweep(MEFF_Plume_mean %*% Vt, 2L, cc, "+")
MEFF_Plume_sdx <- t(sapply(1L:nrow(MEFF_Plume_sd), function(i) {
  sqrt(colSums((MEFF_Plume_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_Plume_varx <- lapply(1L:nrow(MEFF_Plume_sd), function(i) {
  as.vector(crossprod(MEFF_Plume_sd[i, ] * Vt)) 
})
MEFF_Plume_varx <- do.call("cbind", MEFF_Plume_varx) 
dim(MEFF_Plume_varx) <- c(n, n, nrow(MEFF_Plume_sd))
MEFF_Plume_varx <- aperm(MEFF_Plume_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_Plume_upper <- MEFF_Plume_meanx + 2*MEFF_Plume_sdx
MEFF_Plume_lower <- MEFF_Plume_meanx - 2*MEFF_Plume_sdx


## MEFF for heat_flux_Burgard
heat_flux_Burgard_samp <- sort(runif(length(GSAT_2300_nom), 1*10**-4, 10*10**-4))
predBurgard <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_samp, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predBurgard)){
  names(predBurgard)[l] <-  sub("_samp", "", names(predBurgard[l]))
  names(predBurgard)[l] <-  sub("_nom", "", names(predBurgard[l]))
}

MEFF_Burgard <- lapply(1L:r, function(j) {predict(emu[[j]], predBurgard, type = "moments")}) 
MEFF_Burgard_mean <- matrix( unlist(lapply(MEFF_Burgard, function(j) j[c('mean')])), ncol=r)
MEFF_Burgard_sd <- matrix( unlist(lapply(MEFF_Burgard, function(j) j[c('sd')])), ncol=r)

MEFF_Burgard_meanx <- sweep(MEFF_Burgard_mean %*% Vt, 2L, cc, "+")
MEFF_Burgard_sdx <- t(sapply(1L:nrow(MEFF_Burgard_sd), function(i) {
  sqrt(colSums((MEFF_Burgard_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_Burgard_varx <- lapply(1L:nrow(MEFF_Burgard_sd), function(i) {
  as.vector(crossprod(MEFF_Burgard_sd[i, ] * Vt)) 
})
MEFF_Burgard_varx <- do.call("cbind", MEFF_Burgard_varx) 
dim(MEFF_Burgard_varx) <- c(n, n, nrow(MEFF_Burgard_sd))
MEFF_Burgard_varx <- aperm(MEFF_Burgard_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_Burgard_upper <- MEFF_Burgard_meanx + 2*MEFF_Burgard_sdx
MEFF_Burgard_lower <- MEFF_Burgard_meanx - 2*MEFF_Burgard_sdx


## MEFF for heat_flux_ISMIP6_nonlocal
heat_flux_ISMIP6_nonlocal_samp <- sort(runif(length(GSAT_2300_nom), 1*10**4, 4*10**4))
predISMIP6 <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                  heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_samp, heat_flux_ISMIP6_nonlocal_slope_nom))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predISMIP6)){
  names(predISMIP6)[l] <-  sub("_samp", "", names(predISMIP6[l]))
  names(predISMIP6)[l] <-  sub("_nom", "", names(predISMIP6[l]))
}

MEFF_ISMIP6 <- lapply(1L:r, function(j) {predict(emu[[j]], predISMIP6, type = "moments")}) 
MEFF_ISMIP6_mean <- matrix( unlist(lapply(MEFF_ISMIP6, function(j) j[c('mean')])), ncol=r)
MEFF_ISMIP6_sd <- matrix( unlist(lapply(MEFF_ISMIP6, function(j) j[c('sd')])), ncol=r)

MEFF_ISMIP6_meanx <- sweep(MEFF_ISMIP6_mean %*% Vt, 2L, cc, "+")
MEFF_ISMIP6_sdx <- t(sapply(1L:nrow(MEFF_ISMIP6_sd), function(i) {
  sqrt(colSums((MEFF_ISMIP6_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_ISMIP6_varx <- lapply(1L:nrow(MEFF_ISMIP6_sd), function(i) {
  as.vector(crossprod(MEFF_ISMIP6_sd[i, ] * Vt)) 
})
MEFF_ISMIP6_varx <- do.call("cbind", MEFF_ISMIP6_varx) 
dim(MEFF_ISMIP6_varx) <- c(n, n, nrow(MEFF_ISMIP6_sd))
MEFF_ISMIP6_varx <- aperm(MEFF_ISMIP6_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_ISMIP6_upper <- MEFF_ISMIP6_meanx + 2*MEFF_ISMIP6_sdx
MEFF_ISMIP6_lower <- MEFF_ISMIP6_meanx - 2*MEFF_ISMIP6_sdx


## MEFF for heat_flux_ISMIP6_nonlocal_slope
heat_flux_ISMIP6_nonlocal_slope_samp <- sort(runif(length(GSAT_2300_nom), 1*10**6, 4*10**6))
predISMIP6_slope <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                 heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_Burgard_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_samp))
## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predISMIP6_slope)){
  names(predISMIP6_slope)[l] <-  sub("_samp", "", names(predISMIP6_slope[l]))
  names(predISMIP6_slope)[l] <-  sub("_nom", "", names(predISMIP6_slope[l]))
}

MEFF_ISMIP6_slope <- lapply(1L:r, function(j) {predict(emu[[j]], predISMIP6_slope, type = "moments")}) 
MEFF_ISMIP6_slope_mean <- matrix( unlist(lapply(MEFF_ISMIP6_slope, function(j) j[c('mean')])), ncol=r)
MEFF_ISMIP6_slope_sd <- matrix( unlist(lapply(MEFF_ISMIP6_slope, function(j) j[c('sd')])), ncol=r)

MEFF_ISMIP6_slope_meanx <- sweep(MEFF_ISMIP6_slope_mean %*% Vt, 2L, cc, "+")
MEFF_ISMIP6_slope_sdx <- t(sapply(1L:nrow(MEFF_ISMIP6_slope_sd), function(i) {
  sqrt(colSums((MEFF_ISMIP6_slope_sd[i, ] * Vt)^2)) # n vector
}))

MEFF_ISMIP6_slope_varx <- lapply(1L:nrow(MEFF_ISMIP6_slope_sd), function(i) {
  as.vector(crossprod(MEFF_ISMIP6_slope_sd[i, ] * Vt)) 
})
MEFF_ISMIP6_slope_varx <- do.call("cbind", MEFF_ISMIP6_slope_varx) 
dim(MEFF_ISMIP6_slope_varx) <- c(n, n, nrow(MEFF_ISMIP6_slope_sd))
MEFF_ISMIP6_slope_varx <- aperm(MEFF_ISMIP6_slope_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_ISMIP6_slope_upper <- MEFF_ISMIP6_slope_meanx + 2*MEFF_ISMIP6_slope_sdx
MEFF_ISMIP6_slope_lower <- MEFF_ISMIP6_slope_meanx - 2*MEFF_ISMIP6_slope_sdx



############################ plots ############################
d = length(X)
col = hcl.colors(d, palette = "Dark 3")

for(t in tidx){
  #par(mfrow = c(4, 4))
  plot(0,0,xlim = c(min(GSAT_2300_samp),max(GSAT_2300_samp)), ylim = c(-1,5),type = "n",xlab = "GSAT", ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  abline(v = 0, lwd = 0.5)
  abline(h = 0, lwd = 0.5)
  lines(GSAT_2300_samp, MEFF_GSAT_meanx[,t], col = col[1], type = 'l', lwd = 2)
  polygon(c(GSAT_2300_samp, rev(GSAT_2300_samp)), c(MEFF_GSAT_upper[,t], rev(MEFF_GSAT_lower[,t])), col = adjustcolor(col[1], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_GSAT_",years[t],".pdf",sep="")) 
  }
  
  vidx = c()
  for (i in 1:length(unique(simoc_samp))){
    vidx[i] = c(which(simoc_samp == unique(simoc_samp)[i])[1])
  }
  plot(simoc_samp[vidx], MEFF_simoc_meanx[vidx,t], ylim = c(min(MEFF_simoc_lower[,t]),max(MEFF_simoc_upper[,t])), col=col[2], border=col[2], xlab='Model/init ocean forcing', ylab = 'SLE relative to 2000 (m)', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  for (i in vidx){
    lines(c(simoc_samp[i], simoc_samp[i]), c(MEFF_simoc_lower[i,t], MEFF_simoc_upper[i,t]), col=col[2], lwd = 3)
  }
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_simoc_",years[t],".pdf",sep="")) 
  }
  
  plot(init_atmos_samp, MEFF_atmos_meanx[,t], ylim = c(min(MEFF_atmos_lower[,t]),max(MEFF_atmos_upper[,t])), col=col[3], border=col[3], xlab='Init atmosphere forcing', ylab = 'SLE relative to 2000 (m)', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  for (i in 1:length(unique(init_atmos_samp))){
    lines(c(unique(init_atmos_samp)[i], unique(init_atmos_samp)[i]), c(MEFF_atmos_lower[which(init_atmos_samp == unique(init_atmos_samp)[i])[1],t], MEFF_atmos_upper[which(init_atmos_samp == unique(init_atmos_samp)[i])[1],t]), col=col[3], lwd = 3)
  }
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_atmos_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(lapse_rate_samp),max(lapse_rate_samp)), ylim = c(min(MEFF_lapse_lower[,t]),max(MEFF_lapse_upper[,t])), type = "n", xlab = 'Lapse rate', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(lapse_rate_samp, MEFF_lapse_meanx[,t], col = col[4], type = 'l', lwd = 2)
  polygon(c(lapse_rate_samp, rev(lapse_rate_samp)), c(MEFF_lapse_upper[,t], rev(MEFF_lapse_lower[,t])), col = adjustcolor(col[4], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_lapse_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(refreeze_samp),max(refreeze_samp)), ylim = c(min(MEFF_refreeze_lower[,t]),max(MEFF_refreeze_upper[,t])), type = "n", xlab = 'Refreezing parameter', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(refreeze_samp, MEFF_refreeze_meanx[,t], col = col[5], type = 'l', lwd = 2)
  polygon(c(refreeze_samp, rev(refreeze_samp)), c(MEFF_refreeze_upper[,t], rev(MEFF_refreeze_lower[,t])), col = adjustcolor(col[5], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_refreeze_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(refreeze_frac_samp),max(refreeze_frac_samp)), ylim = c(min(MEFF_frac_lower[,t]),max(MEFF_frac_upper[,t])), type = "n", xlab = 'Refreezing fraction', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(refreeze_frac_samp, MEFF_frac_meanx[,t], col = col[6], type = 'l', lwd = 2)
  polygon(c(refreeze_frac_samp, rev(refreeze_frac_samp)), c(MEFF_frac_upper[,t], rev(MEFF_frac_lower[,t])), col = adjustcolor(col[6], alpha.f=0.5),  border = NA)
  #dev.print(pdf, paste("./Multi_year_plots/MEFF_refreeze_frac_",years[t],".pdf",sep="")) 
  
  plot(0,0,xlim = c(min(PDD_ice_samp),max(PDD_ice_samp)), ylim = c(min(MEFF_ice_lower[,t]),max(MEFF_ice_upper[,t])), type = "n", xlab = 'Ice melt parameter', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(PDD_ice_samp, MEFF_ice_meanx[,t], col = col[7], type = 'l', lwd = 2)
  polygon(c(PDD_ice_samp, rev(PDD_ice_samp)), c(MEFF_ice_upper[,t], rev(MEFF_ice_lower[,t])), col = adjustcolor(col[7], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_PDD_ice_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(PDD_snow_samp),max(PDD_snow_samp)), ylim = c(min(MEFF_snow_lower[,t]),max(MEFF_snow_upper[,t])), type = "n", xlab = 'Snow melt parameter', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(PDD_snow_samp, MEFF_snow_meanx[,t], col = col[8], type = 'l', lwd = 2)
  polygon(c(PDD_snow_samp, rev(PDD_snow_samp)), c(MEFF_snow_upper[,t], rev(MEFF_snow_lower[,t])), col = adjustcolor(col[8], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_PDD_snow_",years[t],".pdf",sep="")) 
  }
  
  vvidx = c()
  for (i in 1:length(unique(melt_param_samp))){
    vvidx[i] = c(which(melt_param_samp == unique(melt_param_samp)[i])[1])
  }
  plot(melt_param_samp[vvidx], MEFF_melt_meanx[vvidx,t], ylim = c(min(MEFF_melt_lower[,t]),max(MEFF_melt_upper[,t])), col=col[9], border=col[9], xlab='Melt parameterisation', ylab = 'SLE relative to 2000 (m)', cex = 1.1, cex.main = 1.5, cex.axis = 0.9, cex.lab = 1.5)
  for (i in vvidx){
    lines(c(melt_param_samp[i], melt_param_samp[i]), c(MEFF_melt_lower[i,t], MEFF_melt_upper[i,t]), col=col[9], lwd = 3)
  }
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_melt_param_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(heat_flux_PICO_samp),max(heat_flux_PICO_samp)), ylim = c(min(MEFF_PICO_lower[,t]),max(MEFF_PICO_upper[,t])), type = "n", xlab = 'PICO', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(heat_flux_PICO_samp, MEFF_PICO_meanx[,t], col = col[10], type = 'l', lwd = 2)
  polygon(c(heat_flux_PICO_samp, rev(heat_flux_PICO_samp)), c(MEFF_PICO_upper[,t], rev(MEFF_PICO_lower[,t])), col = adjustcolor(col[10], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_hf_PICO_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(heat_flux_Plume_samp),max(heat_flux_Plume_samp)), ylim = c(min(MEFF_Plume_lower[,t]),max(MEFF_Plume_upper[,t])), type = "n", xlab = 'Plume', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(heat_flux_Plume_samp, MEFF_Plume_meanx[,t], col = col[11], type = 'l', lwd = 2)
  polygon(c(heat_flux_Plume_samp, rev(heat_flux_Plume_samp)), c(MEFF_Plume_upper[,t], rev(MEFF_Plume_lower[,t])), col = adjustcolor(col[11], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_hf_Plume_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(heat_flux_Burgard_samp),max(heat_flux_Burgard_samp)), ylim = c(min(MEFF_Burgard_lower[,t]),max(MEFF_Burgard_upper[,t])), type = "n", xlab = 'Burgard', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(heat_flux_Burgard_samp, MEFF_Burgard_meanx[,t], col = col[12], type = 'l', lwd = 2)
  polygon(c(heat_flux_Burgard_samp, rev(heat_flux_Burgard_samp)), c(MEFF_Burgard_upper[,t], rev(MEFF_Burgard_lower[,t])), col = adjustcolor(col[12], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_hf_Burg_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(heat_flux_ISMIP6_nonlocal_samp),max(heat_flux_ISMIP6_nonlocal_samp)), ylim = c(min(MEFF_ISMIP6_lower[,t]),max(MEFF_ISMIP6_upper[,t])), type = "n", xlab = 'ISMIP6 non-local', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(heat_flux_ISMIP6_nonlocal_samp, MEFF_ISMIP6_meanx[,t], col = col[13], type = 'l', lwd = 2)
  polygon(c(heat_flux_ISMIP6_nonlocal_samp, rev(heat_flux_ISMIP6_nonlocal_samp)), c(MEFF_ISMIP6_upper[,t], rev(MEFF_ISMIP6_lower[,t])), col = adjustcolor(col[13], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_hf_ISMIP_",years[t],".pdf",sep="")) 
  }
  
  plot(0,0,xlim = c(min(heat_flux_ISMIP6_nonlocal_slope_samp),max(heat_flux_ISMIP6_nonlocal_slope_samp)), ylim = c(min(MEFF_ISMIP6_slope_lower[,t]),max(MEFF_ISMIP6_slope_upper[,t])), type = "n", xlab = 'ISMIP6 non-local slope', ylab = "SLE relative to 2000 (m)", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  lines(heat_flux_ISMIP6_nonlocal_slope_samp, MEFF_ISMIP6_slope_meanx[,t], col = col[14], type = 'l', lwd = 2)
  polygon(c(heat_flux_ISMIP6_nonlocal_slope_samp, rev(heat_flux_ISMIP6_nonlocal_slope_samp)), c(MEFF_ISMIP6_slope_upper[,t], rev(MEFF_ISMIP6_slope_lower[,t])), col = adjustcolor(col[14], alpha.f=0.5),  border = NA)
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/MEFF_hf_ISMIP_slope_",years[t],".pdf",sep="")) 
  }
  #if (save_valid){
  #  dev.print(pdf, paste("./Multi_year_plots/MEFF_",years[t],".pdf",sep="")) 
  #}
}

