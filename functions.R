setwd("~/Documents/Emulators/qemu/")

## set threshold for svd
thresh <- 0.999
source("jointemu_svd.R")

## save validation plots?
save_valid <- FALSE
source("loocvandmeff.R")

## save predictions?
save_pred <- FALSE
## use updated FAIR GSAT values?
FAIR2 <- FALSE
source("predictemu_svd.R")

## save plots?
save_plot <- FALSE
source("plots_svd.R")

## if you've run the plots functions, clear all existing plots before running below to reset the formatting
source("load_IMBIE.R")
#source("calibrate.R")
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

#at first was running for each scenario, with GSAT sampled for each proposed state
#mh_119 <- mh_calib(obs, sig, 15, 'SSP119', c(0.1, 0.1, 0.01, 0.1, 0.1, 1*10**-05, 1*10**3, 1*10**5, 1*10**-6, 1*10**-05), 10000, 1000)
#[1] "Acceptance ratio is 0.7484"
#mh_126 <- mh_calib(obs, sig, 15, 'SSP126', c(0.1, 0.1, 0.01, 0.1, 0.1, 1*10**-05, 1*10**3, 1*10**5, 1*10**-6, 1*10**-05), 10000, 1000)
#[1] "Acceptance ratio is 0.6704"
#mh_245 <- mh_calib(obs, sig, 15, 'SSP245', c(0.1, 0.1, 0.01, 0.1, 0.1, 1*10**-05, 1*10**3, 1*10**5, 1*10**-6, 1*10**-05), 10000, 1000)
#[1] "Acceptance ratio is 0.7217"
#mh_370 <- mh_calib(obs, sig, 15, 'SSP370', c(0.1, 0.1, 0.01, 0.1, 0.1, 1*10**-05, 1*10**3, 1*10**5, 1*10**-6, 1*10**-05), 10000, 1000)
#[1] "Acceptance ratio is 0.6884"
#mh_585 <- mh_calib(obs, sig, 15, 'SSP585', c(0.1, 0.1, 0.01, 0.1, 0.1, 1*10**-05, 1*10**3, 1*10**5, 1*10**-6, 1*10**-05), 10000, 1000)
#[1] "Acceptance ratio is 0.6203"

#try running without scenario being an input at all
mh <- mh_calib(obs, sig, 15, c(0.1, 0.1, 0.01, 0.1, 0.1, 1*10**-05, 1*10**3, 1*10**5, 1*10**-6, 1*10**-05), 10000, 1000)

for(i in 1:length(unique(X$simoc))){
  mh$simoc[mh$simoc == i] = levels(X$simoc)[[i]]
#  mh_119$simoc[mh_119$simoc == i] = levels(X$simoc)[[i]]
#  mh_126$simoc[mh_126$simoc == i] = levels(X$simoc)[[i]]
#  mh_245$simoc[mh_245$simoc == i] = levels(X$simoc)[[i]]
#  mh_370$simoc[mh_370$simoc == i] = levels(X$simoc)[[i]]
#  mh_585$simoc[mh_585$simoc == i] = levels(X$simoc)[[i]]
}
for(i in 1:length(unique(X$init_atmos))){
  mh$init_atmos[mh$init_atmos == i] = levels(X$init_atmos)[[i]]
#  mh_119$init_atmos[mh_119$init_atmos == i] = levels(X$init_atmos)[[i]]
#  mh_126$init_atmos[mh_126$init_atmos == i] = levels(X$init_atmos)[[i]]
#  mh_245$init_atmos[mh_245$init_atmos == i] = levels(X$init_atmos)[[i]]
#  mh_370$init_atmos[mh_370$init_atmos == i] = levels(X$init_atmos)[[i]]
#  mh_585$init_atmos[mh_585$init_atmos == i] = levels(X$init_atmos)[[i]]
}
for(i in 1:length(unique(X$melt_param))){
  mh$melt_param[mh$melt_param == i] = levels(X$melt_param)[[i]]
#  mh_119$melt_param[mh_119$melt_param == i] = levels(X$melt_param)[[i]]
#  mh_126$melt_param[mh_126$melt_param == i] = levels(X$melt_param)[[i]]
#  mh_245$melt_param[mh_245$melt_param == i] = levels(X$melt_param)[[i]]
#  mh_370$melt_param[mh_370$melt_param == i] = levels(X$melt_param)[[i]]
#  mh_585$melt_param[mh_585$melt_param == i] = levels(X$melt_param)[[i]]
}

write.csv(mh, "Distributions/mh_output.csv", row.names = FALSE)
#write.csv(mh_119, "Distributions/mh_119.csv", row.names = FALSE)
#write.csv(mh_126, "Distributions/mh_126.csv", row.names = FALSE)
#write.csv(mh_245, "Distributions/mh_245.csv", row.names = FALSE)
#write.csv(mh_370, "Distributions/mh_370.csv", row.names = FALSE)
#write.csv(mh_585, "Distributions/mh_585.csv", row.names = FALSE)

