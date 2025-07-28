## load all packages
source("./shared.R")

## plot controls
plot_sea_level_timeseries <- TRUE #plot timeseries of sea level rise, relative to 2000, for all simulations, coloured by scenario
plot_loocv                <- FALSE #make plots of loocv at timeslices 2100, 2150, 2200, 2300
plot_meff                 <- FALSE #make plots of the main effects curves at 2300

## Preprocess the data, do the SVD and build the emulators
thresh <- 0.999
source("preprocess_SVD_emulate.R") #pre-process the data, do the SVD, and build the emulators. Sometimes you have to run this twice (doesn't work first time around?) and I have no idea why...

## Do the loocv and main effects
save_valid <- FALSE #flag to save the validation plots
n_loocv <- 100      #number of leave one out cross validation points to output
output_loocv_data <- TRUE #output the leave out of out cross validation data 
source("loocv.R") #need to run this to bring the nominal values into scope (used to initialise the mcmc)

## load in the calibration data
source("load_IMBIE.R")


## MCMC

# set step size and bounds on variables

# step sizes
lapse_rate_step_size <- 0.1
refreeze_step_size   <- 0.1
refreeze_frac_step_size <- 0.01
PDD_ice_step_size    <- 0.1
PDD_snow_step_size   <- 0.1
heat_flux_Burgard_step_size <- 1*10**-05
heat_flux_ISMIP6_nonlocal_step_size <- 1*10**3
heat_flux_ISMIP6_nonlocal_slope_step_size <-  1*10**5
heat_flux_PICO_step_size   <-  1*10**-6
heat_flux_Plume_step_size  <-  1*10**-05
GSAT_step_size             <-  1.0

step_size <- c(lapse_rate_step_size, 
               refreeze_step_size, 
               refreeze_frac_step_size, 
               PDD_ice_step_size, 
               PDD_snow_step_size,
               heat_flux_Burgard_step_size,
               heat_flux_ISMIP6_nonlocal_step_size,
               heat_flux_ISMIP6_nonlocal_slope_step_size,
               heat_flux_PICO_step_size,
               heat_flux_Plume_step_size,
               GSAT_step_size)


# other mcmc parameters
fac <- 5 #how many times larger is the model error than obs error
obs_sig <- sig #just rename bc why not
chain_length <- 31000 #length of the MCMC
burn_in <- 1001 #burn in period
write_as_you_go <- TRUE #flag to write the output every 100 steps

source("run_mcmc.R") #brings the function mh_calib to run the mcmc into scope

mh <- mh_calib(obs, obs_sig, fac, step_size, chain_length, burn_in,write_as_you_go)

#output the results of the calibration 
write.csv(mh$posterior_samples, "outputs/mcmc_output_data/mcmc_output_posteriorsamples.csv", row.names = FALSE)
write.csv(mh$posterior_trajectories, "outputs/mcmc_output_data/mcmc_output_posteriortrajectories.csv", row.names = FALSE)
write.csv(mh$posterior_trajectories_ssp119, "outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp119.csv", row.names = FALSE)
write.csv(mh$posterior_trajectories_ssp126, "outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp126.csv", row.names = FALSE)
write.csv(mh$posterior_trajectories_ssp245, "outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp245.csv", row.names = FALSE)
write.csv(mh$posterior_trajectories_ssp370, "outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp370.csv", row.names = FALSE)
write.csv(mh$posterior_trajectories_ssp585, "outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp585.csv", row.names = FALSE)

#generate same number of samples of SLR from the prior
nprior <- chain_length - burn_in + 1
GSAT_2300_prior  <- runif(nprior, min =  min(X$GSAT_2300, na.rm = TRUE), max = max(X$GSAT_2300, na.rm = TRUE))
lapse_rate_prior <- runif(nprior, min = -12, max = -5)
refreeze_prior   <- runif(nprior, min = 0, max = 15)
refreeze_frac_prior <- runif(nprior, min = 0.2, max = 0.8)
PDD_ice_prior    <- runif(nprior, min = 4, max = 12)
PDD_snow_prior   <- runif(nprior, min = 0, max = 6)
heat_flux_Burgard_prior <- runif(nprior, min = 1*10**-4, max = 10*10**-4)
heat_flux_ISMIP6_nonlocal_prior <- runif(nprior,min =  1*10**4, max = 4*10**4)
heat_flux_ISMIP6_nonlocal_slope_prior <- runif(nprior, min = 1*10**6, max = 4*10**6)
heat_flux_PICO_prior <- runif(nprior, min = 0.1*10**-5, max = 10*10**-5)
heat_flux_Plume_prior <- runif(nprior, min = 1*10**-4, max = 10*10**-4)
simoc_prior           <- sample(unique(X$simoc), size = nprior, replace = TRUE)
init_atmos_prior      <- sample(unique(X$init_atmos), size = nprior, replace = TRUE)
melt_param_prior      <-  sample(unique(X$melt_param), size = nprior, replace = TRUE)
prior_params <- data.frame(GSAT_2300_prior, simoc_prior, init_atmos_prior, lapse_rate_prior, refreeze_prior, refreeze_frac_prior, PDD_ice_prior, PDD_snow_prior, melt_param_prior, 
                           heat_flux_PICO_prior, heat_flux_Plume_prior, heat_flux_Burgard_prior, heat_flux_ISMIP6_nonlocal_prior, heat_flux_ISMIP6_nonlocal_slope_prior)

colnames(prior_params) <- c("GSAT_2300", "simoc", "init_atmos", "lapse_rate", "refreeze", "refreeze_frac", "PDD_ice", "PDD_snow", "melt_param", "heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard", "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope")

#loop over entries
prior_slr_trajectories <- data.frame(matrix(vector(), nprior, 70))
for (i in 1:nprior){
  prior_current_pred <- lapply(1L:r, function(j) {predict(emu[[j]], prior_params[i,], type = "moments")}) 
  prior_current_mean <- matrix( unlist(lapply(prior_current_pred, function(j) j[c('mean')])), ncol=r)
  prior_current_sd <- matrix( unlist(lapply(prior_current_pred, function(j) j[c('sd')])), ncol=r)
  prior_current_meanx <- sweep(prior_current_mean %*% Vt, 2L, cc, "+")
  prior_current_varx <- lapply(1L:nrow(prior_current_sd), function(i) {
    as.vector(crossprod(prior_current_sd[i, ] * Vt)) 
  })
  prior_current_varx <- do.call("cbind", prior_current_varx) 
  dim(prior_current_varx) <- c(n, n)
  
  #store
  prior_slr_trajectories[i,] <- prior_current_meanx
}
write.csv(prior_slr_trajectories, "outputs/mcmc_output_data/priortrajectories.csv", row.names = FALSE)
write.csv(prior_params, "outputs/mcmc_output_data/priorparameters.csv", row.names = FALSE)


#run the MEFF
source("meff.R")



