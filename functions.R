setwd("~/Documents/Emulators/qemu/")

## set threshold for svd
thresh <- 0.999
source("jointemu_svd.R")

## save validation plots?
save_valid <- TRUE
source("loocvandmeff.R")

## how many predictions to make?
s <- 10000
## save predictions?
save_pred <- TRUE
## use updated FAIR GSAT values?
FAIR2 <- FALSE
##equally sample factors?
equal_samp <- FALSE
##nest the heat flux parameters?
nest_flux <- FALSE
source("predictemu_svd.R")

## save plots?
save_plot <- TRUE
source("plots_svd.R")

#let's calibrate
source("load_IMBIE.R")
#direct weighting
source("calibrate.R")
#mcmc
source("mh.R")

#lapse_rate has range [-12, -5]
#refreeze has range [0, 15]
#refreeze_frac has range [0.2, 0.8]
#PDD_ice has range [4, 12]
#PDD_snow has range [0, 6]
## heat_flux_Burgard has range [1*10**-4, 10*10**-4]
## heat_flux_ISMIP6_nonlocal has range [1*10**4, 4*10**4]
## heat_flux_ISMIP6_nonlocal_slope has range [1*10**6, 4*10**6]
## heat_flux_PICO has range [0.1*10**-5, 10*10**-5]
## heat_flux_Plume has range [1*10**-4, 10*10**-4]

mh <- mh_calib(obs, sig, 10, c(0.1, 0.1, 0.01, 0.1, 0.1, 1*10**-05, 1*10**3, 1*10**5, 1*10**-6, 1*10**-05), 10000, 1000)

#need to add in string names as output is currently levels
for(i in 1:length(unique(X$simoc))){
  mh$simoc[mh$simoc == i] = levels(X$simoc)[[i]]
}
for(i in 1:length(unique(X$init_atmos))){
  mh$init_atmos[mh$init_atmos == i] = levels(X$init_atmos)[[i]]
}
for(i in 1:length(unique(X$melt_param))){
  mh$melt_param[mh$melt_param == i] = levels(X$melt_param)[[i]]
}

write.csv(mh, "Distributions/mh_output.csv", row.names = FALSE)

