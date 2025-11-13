############################# MEFF #############################################
# evaluate the main effects of the emulator by varying a single parameter while 
# keeping all others fixed. Do this first for nominal fixed values and then for
# posterior means

message("running meff")


## create nominal values for all variables. These are values which are all 
#except one held fixed while a single variable is varied to understand 
#sensitivity of emulator to that input variable.
#
## GSAT :       mean of GCMs
## simoc :      Kori_ISMIP6_3D
## init_atmos : RACMO2.3p2
## lapse_rate : -8.2
## refreeze :      5
## refreeze frac:  0.5
## PDD_ice : 8
## PDD_snow : 3
## melt_param : PICO (bc it's the only shared melt_param between simus)
## heat_flux_ISMIP6_nonlocal : 1.45*10**4
## heat_flux_ISMIP6_nonlocal_slope : 2.06*10**6
## heat_flux_PICO : 1§§ 4*10*-5 (as simoc is Kori_ISMIP6_3D)
## heat_flux_Plume : 5.9*10**-4


##decide on size of sample and set here
n_meff <- 1000 
GSAT_2300_nom         <- rep(mean(X$GSAT_2300), n_meff) #make an array of length n_meff with all values set to nominal value
simoc_nom             <- rep(unique(X$simoc)[1], length(GSAT_2300_nom)) #repeat for all the other variable 
init_atmos_nom        <- rep(unique(X$init_atmos)[1], length(GSAT_2300_nom))
lapse_rate_nom        <- rep(-8.2, length(GSAT_2300_nom))
refreeze_nom          <- rep(5, length(GSAT_2300_nom))
refreeze_frac_nom     <- rep(0.5, length(GSAT_2300_nom))
PDD_ice_nom           <- rep(8, length(GSAT_2300_nom))
PDD_snow_nom          <- rep(3, length(GSAT_2300_nom))
melt_param_nom        <- rep(unique(X$melt_param)[2], length(GSAT_2300_nom))
heat_flux_ISMIP6_nonlocal_nom <- rep(1.45*10**4, length(GSAT_2300_nom))
heat_flux_ISMIP6_nonlocal_slope_nom <- rep(2.06*10**6, length(GSAT_2300_nom))
heat_flux_PICO_nom    <- rep(4*10**-5, length(GSAT_2300_nom))
heat_flux_Plume_nom   <- rep(5.9*10**-4, length(GSAT_2300_nom))

########################## MEFF for continuous variables #######################

######## MEFF for GSAT
GSAT_2300_samp <- sort(X$GSAT_2300)[seq(from = 1, to = length(X$GSAT_2300), length.out = length(GSAT_2300_nom))] #get n_meff samples from GSAT, based on evenly spaced indices

#alternatively, get an array of evenly spaced values between the min and maximum
GSAT_2300_samp <- seq(min(X$GSAT), max(X$GSAT), length.out = length(GSAT_2300_nom))


predGSAT <- as.list(data.frame(GSAT_2300_samp, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))

## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predGSAT)){
  names(predGSAT)[l] <-  sub("_samp", "", names(predGSAT)[l])
  names(predGSAT)[l] <-  sub("_nom", "", names(predGSAT)[l])
}

MEFF_GSAT <- lapply(1L:r, function(j) {predict(emu[[j]], predGSAT, type = "moments")})  #retrurn emulator predictions of components
MEFF_GSAT_mean <- matrix( unlist(lapply(MEFF_GSAT, function(j) j[c('mean')])), ncol=r)  #get the mean column
MEFF_GSAT_sd <- matrix( unlist(lapply(MEFF_GSAT, function(j) j[c('sd')])), ncol=r)      #get the sd column

MEFF_GSAT_meanx <- sweep(MEFF_GSAT_mean %*% Vt, 2L, cc, "+") #convert predictions back into SLR values n_meff values at 70 timeslices
MEFF_GSAT_sdx <- t(sapply(1L:nrow(MEFF_GSAT_sd), function(i) {
  sqrt(colSums((MEFF_GSAT_sd[i, ] * Vt)^2)) # n vector
}))

#compute the variances if we want these
MEFF_GSAT_varx <- lapply(1L:nrow(MEFF_GSAT_sd), function(i) {
  as.vector(crossprod(MEFF_GSAT_sd[i, ] * Vt)) 
})
MEFF_GSAT_varx <- do.call("cbind", MEFF_GSAT_varx) 
dim(MEFF_GSAT_varx) <- c(n, n, nrow(MEFF_GSAT_sd))
MEFF_GSAT_varx <- aperm(MEFF_GSAT_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_GSAT_upper <- MEFF_GSAT_meanx + 2*MEFF_GSAT_sdx
MEFF_GSAT_lower <- MEFF_GSAT_meanx - 2*MEFF_GSAT_sdx

# save this info
write.csv(MEFF_GSAT_meanx, "outputs/meff/GSAT_2300/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_GSAT_sdx,  "outputs/meff/GSAT_2300/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(GSAT_2300_samp,  "outputs/meff/GSAT_2300/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/GSAT_2300/years.csv", row.names = FALSE)

########## MEFF for lapse_rate
lapse_rate_samp <- sort(runif(length(GSAT_2300_nom), -12, -5))
predlapse <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_samp, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_lapse_meanx, "outputs/meff/lapse_rate/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_lapse_sdx,  "outputs/meff/lapse_rate/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(lapse_rate_samp,  "outputs/meff/lapse_rate/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/lapse_rate/years.csv", row.names = FALSE)


########## MEFF for refreeze
refreeze_samp <- sort(runif(length(GSAT_2300_nom), 0, 15))
predrefreeze <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_samp, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                   heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_refreeze_meanx, "outputs/meff/refreeze/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_refreeze_sdx,  "outputs/meff/refreeze/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(refreeze_samp,  "outputs/meff/refreeze/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/refreeze/years.csv", row.names = FALSE)



########## MEFF for refreeze_frac
refreeze_frac_samp <- sort(runif(length(GSAT_2300_nom), 0.2, 0.8))
predfrac <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_samp, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_frac_meanx, "outputs/meff/refreeze_frac/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_frac_sdx,  "outputs/meff/refreeze_frac/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(refreeze_frac_samp,  "outputs/meff/refreeze_frac/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/refreeze_frac/years.csv", row.names = FALSE)



########## MEFF for PDD_ice
PDD_ice_samp <- sort(runif(length(GSAT_2300_nom), 4, 12))
predice <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_samp, PDD_snow_nom, melt_param_nom, 
                              heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_ice_meanx, "outputs/meff/PDD_ice/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_ice_sdx,  "outputs/meff/PDD_ice/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(PDD_ice_samp,  "outputs/meff/PDD_ice/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/PDD_ice/years.csv", row.names = FALSE)


########## MEFF for PDD_snow
PDD_snow_samp <- sort(runif(length(GSAT_2300_nom), 0, 6))
predsnow <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_samp, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_snow_meanx, "outputs/meff/PDD_snow/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_snow_sdx,  "outputs/meff/PDD_snow/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(PDD_snow_samp,  "outputs/meff/PDD_snow/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/PDD_snow/years.csv", row.names = FALSE)


########## MEFF for heat_flux_PICO
heat_flux_PICO_samp <- sort(runif(length(GSAT_2300_nom), 0.1*10**-5, 10*10**-5))
predPICO <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_samp, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_PICO_meanx, "outputs/meff/heat_flux_PICO/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_PICO_sdx,  "outputs/meff/heat_flux_PICO/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(heat_flux_PICO_samp,  "outputs/meff/heat_flux_PICO/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/heat_flux_PICO/years.csv", row.names = FALSE)


########## MEFF for heat_flux_Plume
heat_flux_Plume_samp <- sort(runif(length(GSAT_2300_nom), 1*10**-4, 10*10**-4))
predPlume <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_samp, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_Plume_meanx, "outputs/meff/heat_flux_plume/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_Plume_sdx,  "outputs/meff/heat_flux_plume/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(heat_flux_Plume_samp,  "outputs/meff/heat_flux_plume/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/heat_flux_plume/years.csv", row.names = FALSE)



########## MEFF for heat_flux_ISMIP6_nonlocal
heat_flux_ISMIP6_nonlocal_samp <- sort(runif(length(GSAT_2300_nom), 1*10**4, 4*10**4))
predISMIP6 <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                 heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_samp, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_ISMIP6_meanx, "outputs/meff/heat_flux_ISMIP6/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_ISMIP6_sdx,  "outputs/meff/heat_flux_ISMIP6/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(heat_flux_ISMIP6_nonlocal_samp,  "outputs/meff/heat_flux_ISMIP6/samples.csv", row.names = FALSE)
write.csv(years,  "outputs/meff/heat_flux_ISMIP6/years.csv", row.names = FALSE)

########## MEFF for heat_flux_ISMIP6_nonlocal_slope
heat_flux_ISMIP6_nonlocal_slope_samp <- sort(runif(length(GSAT_2300_nom), 1*10**6, 4*10**6))
predISMIP6_slope <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                       heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_samp))
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

# save this info
write.csv(MEFF_ISMIP6_slope_meanx, "outputs/meff/heat_flux_ISMIP6_slope/nominal_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_ISMIP6_slope_sdx,  "outputs/meff/heat_flux_ISMIP6_slope/nominal_meff_sdx.csv", row.names = FALSE)
write.csv(heat_flux_ISMIP6_nonlocal_slope_samp,  "outputs/meff/heat_flux_ISMIP6_slope/samples.csv", row.names = FALSE)
write.csv(years,"outputs/meff/heat_flux_ISMIP6_slope/years.csv", row.names = FALSE)


################### MEFF for categorical/factor variable #######################
# For categorical variables, we just evaluate the emulator at each of the possible factor values
######## MEFF for simoc
simoc_samp <- sample(X$simoc, length(GSAT_2300_nom), TRUE)
predsimoc <- as.list(data.frame(GSAT_2300_nom, simoc_samp, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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



########## MEFF for init_atmos
init_atmos_samp <- sample(unique(X$init_atmos), length(GSAT_2300_nom), TRUE)
predatmos <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_samp, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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


########## MEFF for melt_param
melt_param_samp <- sample(X$melt_param, length(GSAT_2300_nom), TRUE)
predmelt <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_samp, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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



################################################################################
######################### Repeat for posterior means ###########################
################################################################################

##decide on size of sample and set here
GSAT_2300_nom         <- rep(median(mh$posterior_samples$GSAT_2300), n_meff) #make an array of length n_meff with all values set to nominal value
lapse_rate_nom        <- rep(median(mh$posterior_samples$lapse_rate), length(GSAT_2300_nom))
refreeze_nom          <- rep(median(mh$posterior_samples$refreeze), length(GSAT_2300_nom))
refreeze_frac_nom     <- rep(median(mh$posterior_samples$refreeze_frac), length(GSAT_2300_nom))
PDD_ice_nom           <- rep(median(mh$posterior_samples$PDD_ice), length(GSAT_2300_nom))
PDD_snow_nom          <- rep(median(mh$posterior_samples$PDD_snow), length(GSAT_2300_nom))
heat_flux_ISMIP6_nonlocal_nom <- rep(median(mh$posterior_samples$heat_flux_ISMIP6_nonlocal), length(GSAT_2300_nom))
heat_flux_ISMIP6_nonlocal_slope_nom <- rep(median(mh$posterior_samples$heat_flux_ISMIP6_nonlocal_slope), length(GSAT_2300_nom))
heat_flux_PICO_nom    <- rep(median(mh$posterior_samples$heat_flux_PICO), length(GSAT_2300_nom))
heat_flux_Plume_nom   <- rep(median(mh$posterior_samples$heat_flux_Plume), length(GSAT_2300_nom))

#take the same as the nominal for the categorical variables
simoc_post             <- rep(unique(X$simoc)[1], length(GSAT_2300_nom)) #repeat for all the other variable 
init_atmos_post        <- rep(unique(X$init_atmos)[1], length(GSAT_2300_nom))
melt_param_post        <- rep(unique(X$melt_param)[2], length(GSAT_2300_nom))

########################## MEFF for continuous variables #######################

######## MEFF for GSAT
GSAT_2300_samp <- sort(X$GSAT_2300)[seq(from = 1, to = length(X$GSAT_2300), length.out = length(GSAT_2300_nom))] #get n_meff samples from GSAT, based on evenly spaced indices

#alternatively, get an array of evenly spaced values between the min and maximum
GSAT_2300_samp <- seq(min(X$GSAT), max(X$GSAT), length.out = length(GSAT_2300_nom))


predGSAT <- as.list(data.frame(GSAT_2300_samp, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))

## remove _samp and _nom or the predict function won't recognise the variables
for ( l in 1:length(predGSAT)){
  names(predGSAT)[l] <-  sub("_samp", "", names(predGSAT)[l])
  names(predGSAT)[l] <-  sub("_nom", "", names(predGSAT)[l])
}

MEFF_GSAT <- lapply(1L:r, function(j) {predict(emu[[j]], predGSAT, type = "moments")})  #retrurn emulator predictions of components
MEFF_GSAT_mean <- matrix( unlist(lapply(MEFF_GSAT, function(j) j[c('mean')])), ncol=r)  #get the mean column
MEFF_GSAT_sd <- matrix( unlist(lapply(MEFF_GSAT, function(j) j[c('sd')])), ncol=r)      #get the sd column

MEFF_GSAT_meanx <- sweep(MEFF_GSAT_mean %*% Vt, 2L, cc, "+") #convert predictions back into SLR values n_meff values at 70 timeslices
MEFF_GSAT_sdx <- t(sapply(1L:nrow(MEFF_GSAT_sd), function(i) {
  sqrt(colSums((MEFF_GSAT_sd[i, ] * Vt)^2)) # n vector
}))

#compute the variances if we want these
MEFF_GSAT_varx <- lapply(1L:nrow(MEFF_GSAT_sd), function(i) {
  as.vector(crossprod(MEFF_GSAT_sd[i, ] * Vt)) 
})
MEFF_GSAT_varx <- do.call("cbind", MEFF_GSAT_varx) 
dim(MEFF_GSAT_varx) <- c(n, n, nrow(MEFF_GSAT_sd))
MEFF_GSAT_varx <- aperm(MEFF_GSAT_varx, c(3, 1, 2)) 

#create upper and lower values (95% CI)
MEFF_GSAT_upper <- MEFF_GSAT_meanx + 2*MEFF_GSAT_sdx
MEFF_GSAT_lower <- MEFF_GSAT_meanx - 2*MEFF_GSAT_sdx

# save this info
write.csv(MEFF_GSAT_meanx, "outputs/meff/GSAT_2300/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_GSAT_sdx,  "outputs/meff/GSAT_2300/nominalposterior_meff_sdx.csv", row.names = FALSE)


########## MEFF for lapse_rate
lapse_rate_samp <- sort(runif(length(GSAT_2300_nom), -12, -5))
predlapse <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_samp, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_lapse_meanx, "outputs/meff/lapse_rate/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_lapse_sdx,  "outputs/meff/lapse_rate/nominalposterior_meff_sdx.csv", row.names = FALSE)



########## MEFF for refreeze
refreeze_samp <- sort(runif(length(GSAT_2300_nom), 0, 15))
predrefreeze <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_samp, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                   heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_refreeze_meanx, "outputs/meff/refreeze/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_refreeze_sdx,  "outputs/meff/refreeze/nominalposterior_meff_sdx.csv", row.names = FALSE)




########## MEFF for refreeze_frac
refreeze_frac_samp <- sort(runif(length(GSAT_2300_nom), 0.2, 0.8))
predfrac <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_samp, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_frac_meanx, "outputs/meff/refreeze_frac/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_frac_sdx,  "outputs/meff/refreeze_frac/nominalposterior_meff_sdx.csv", row.names = FALSE)



########## MEFF for PDD_ice
PDD_ice_samp <- sort(runif(length(GSAT_2300_nom), 4, 12))
predice <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_samp, PDD_snow_nom, melt_param_nom, 
                              heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_ice_meanx, "outputs/meff/PDD_ice/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_ice_sdx,  "outputs/meff/PDD_ice/nominalposterior_meff_sdx.csv", row.names = FALSE)


########## MEFF for PDD_snow
PDD_snow_samp <- sort(runif(length(GSAT_2300_nom), 0, 6))
predsnow <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_samp, melt_param_nom, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_snow_meanx, "outputs/meff/PDD_snow/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_snow_sdx,  "outputs/meff/PDD_snow/nominalposterior_meff_sdx.csv", row.names = FALSE)


########## MEFF for heat_flux_PICO
heat_flux_PICO_samp <- sort(runif(length(GSAT_2300_nom), 0.1*10**-5, 10*10**-5))
predPICO <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                               heat_flux_PICO_samp, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_PICO_meanx, "outputs/meff/heat_flux_PICO/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_PICO_sdx,  "outputs/meff/heat_flux_PICO/nominalposterior_meff_sdx.csv", row.names = FALSE)


########## MEFF for heat_flux_Plume
heat_flux_Plume_samp <- sort(runif(length(GSAT_2300_nom), 1*10**-4, 10*10**-4))
predPlume <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_samp, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_Plume_meanx, "outputs/meff/heat_flux_plume/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_Plume_sdx,  "outputs/meff/heat_flux_plume/nominalposterior_meff_sdx.csv", row.names = FALSE)

########## MEFF for heat_flux_ISMIP6_nonlocal
heat_flux_ISMIP6_nonlocal_samp <- sort(runif(length(GSAT_2300_nom), 1*10**4, 4*10**4))
predISMIP6 <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                 heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_samp, heat_flux_ISMIP6_nonlocal_slope_nom))
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

# save this info
write.csv(MEFF_ISMIP6_meanx, "outputs/meff/heat_flux_ISMIP6/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_ISMIP6_sdx,  "outputs/meff/heat_flux_ISMIP6/nominalposterior_meff_sdx.csv", row.names = FALSE)


########## MEFF for heat_flux_ISMIP6_nonlocal_slope
heat_flux_ISMIP6_nonlocal_slope_samp <- sort(runif(length(GSAT_2300_nom), 1*10**6, 4*10**6))
predISMIP6_slope <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                       heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_samp))
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

# save this info
write.csv(MEFF_ISMIP6_slope_meanx, "outputs/meff/heat_flux_ISMIP6_slope/nominalposterior_meff_meanx.csv", row.names = FALSE)
write.csv(MEFF_ISMIP6_slope_sdx,  "outputs/meff/heat_flux_ISMIP6_slope/nominalposterior_meff_sdx.csv", row.names = FALSE)


################### MEFF for categorical/factor variable #######################
# For categorical variables, we just evaluate the emulator at each of the possible factor values
######## MEFF for simoc
simoc_samp <- sample(X$simoc, length(GSAT_2300_nom), TRUE)
predsimoc <- as.list(data.frame(GSAT_2300_nom, simoc_samp, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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



########## MEFF for init_atmos
init_atmos_samp <- sample(unique(X$init_atmos), length(GSAT_2300_nom), TRUE)
predatmos <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_samp, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_nom, 
                                heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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


########## MEFF for melt_param
melt_param_samp <- sample(X$melt_param, length(GSAT_2300_nom), TRUE)
predmelt <- as.list(data.frame(GSAT_2300_nom, simoc_nom, init_atmos_nom, lapse_rate_nom, refreeze_nom, refreeze_frac_nom, PDD_ice_nom, PDD_snow_nom, melt_param_samp, 
                               heat_flux_PICO_nom, heat_flux_Plume_nom, heat_flux_ISMIP6_nonlocal_nom, heat_flux_ISMIP6_nonlocal_slope_nom))
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
