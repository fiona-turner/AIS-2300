## Run the mcmc. Define a function mh_calib which returns posterior samples and then call this function 

mh_calib <- function(obs, obs_sig, fac, step_size, chain_length, burn_in, bounds){
  ## a function to run Metropolis-Hastings on the random forest emulator
  # inputs:
  # obs = the observations to be used in the likelihood calculation
  # obs_sig = the observational error
  # fac = the factor we use to calculate model error, assuming mod_sig = fac*obs_sig
  # step_size = the size of the noise component used to generate new states
  # chain_length = length of markov chain we wish to sample
  # burn_in = number of samples we remove from beginning of chain
  
  
#  #get a GSAT value according to scenario
#  if(FAIR2){
#    ## load new FAIR data
#    f2path = './FAIR_data/'
#    ssp <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")
#    FORpred = NULL
#    for( s in ssp){
      #ncin <- ncdf4::nc_open(Sys.glob(filef.path(f2path, s, "*.nc")))
#      ncin <- ncdf4::nc_open(paste0(f2path,s,".temperature.fair.temperature_climate.nc"))
#      st <- as.data.frame(ncdf4::ncvar_get(ncin, paste0(s, "/surface_temperature")))
#      st  <- cbind(rep(s, dim(st)[1]), st)
#      rbind(FORpred,data.frame(st))->FORpred
#    }
#    year <- ncdf4::ncvar_get(ncin,"year")
#    colnames(FORpred) <- c('scenario',paste0("y", year))
#    FORpred$ystart <- rowMeans(subset(FORpred,select=y2015:y2044))
#    FORpred$yend <- rowMeans(subset(FORpred,select=y2271:y2300))
#    FORpred$GSAT_2300 <- (FORpred$yend-FORpred$ystart)
#    #tmp <- FORpred[scenario == scenario] 
#  } else {
#    ## or IPCC FAIR data
#    fpath <- "./"
#    FORpred <- fread(file.path(fpath, "CLIMATE_FORCING_IPCC_AR6_230706.csv"))
#    FORpred$ystart <- rowMeans(subset(FORpred,select=y2015:y2044))
#    FORpred$yend <- rowMeans(subset(FORpred,select=y2271:y2300))
#    FORpred$GSAT_2300 <- (FORpred$yend-FORpred$ystart)
#    #tmp <- FORpred[scenario == scenario]
#  }
# #set GSAT to mean of FAIR simulations
#  GSAT_2300 <- mean(FORpred$GSAT_2300) #
#  #if using scenario then would set to a random sample from tmp
  
  #set variables to initial values
  simoc <- unique(X$simoc)[1]
  init_atmos <- unique(X$init_atmos)[1]
  lapse_rate <- lapse_rate_nom[1]
  refreeze <- refreeze_nom[1]
  refreeze_frac <- refreeze_frac_nom[1]
  PDD_ice <- PDD_ice_nom[1]
  PDD_snow <- PDD_snow_nom[1]
  melt_param <- unique(X$melt_param)[2]
  heat_flux_Burgard = heat_flux_Burgard_nom[1]
  heat_flux_ISMIP6_nonlocal = heat_flux_ISMIP6_nonlocal_nom[1]
  heat_flux_ISMIP6_nonlocal_slope = heat_flux_ISMIP6_nonlocal_slope_nom[1]
  heat_flux_PICO = heat_flux_PICO_nom[1]
  heat_flux_Plume = heat_flux_Plume_nom[1]
  GSAT_2300 = 2.68829 #mean from FAIR2

  #save variables in data frame as current state
  current_state <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                               heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
  
  
  #function to create a proposed state, by taking a step away from current state.
  #Previously, had limits in the parameters coded in here, but that should be in the prior and come in in the likelihood
  sample_proposal <- function(current_state) {
    
    
    ### resample randomly for the categorical variables
    simoc = sample(unique(X$simoc), 1, TRUE)
    init_atmos = sample(unique(X$init_atmos), 1, TRUE)
    melt_param = sample(unique(X$melt_param), 1, TRUE)
    
    ### perturb values for the continuous variables
    lapse_rate = as.numeric(current_state['lapse_rate']) + rnorm(1, mean = 0, sd = step_size[1])
#    if (lapse_rate < -12){
#      lapse_rate = -12
#    } else if (lapse_rate > -5){
#      lapse_rate = -5
#    }
    
    refreeze = as.numeric(current_state['refreeze']) + rnorm(1, mean = 0, sd = step_size[2])
#    if (refreeze < 0){
#      refreeze = 0
#   } else if (refreeze > 15){
#      refreeze = 15
#    }
    
    refreeze_frac = as.numeric(current_state['refreeze_frac']) + rnorm(1, mean = 0, sd = step_size[3])
#    if (refreeze_frac < 0.2){
#      refreeze_frac = 0.2
#    } else if (refreeze_frac > 0.8){
#      refreeze_frac = 0.8
#    }

    PDD_ice = as.numeric(current_state['PDD_ice']) + rnorm(1, mean = 0, sd = step_size[4])
#    if (PDD_ice  < 4){
#      PDD_ice  = 4
#    } else if (PDD_ice  > 12){
#      PDD_ice  = 12
#    }
    
    PDD_snow = as.numeric(current_state['PDD_snow']) + rnorm(1, mean = 0, sd = step_size[5])
#    if (PDD_snow  < 0){
#      PDD_snow  = 0
#    } else if (PDD_snow  > 6){
#      PDD_snow  = 6
#    }
    

    heat_flux_Burgard = as.numeric(current_state['heat_flux_Burgard']) + rnorm(1, mean = 0, sd = step_size[6])
#    if (heat_flux_Burgard  < 1*10**-4){
#      heat_flux_Burgard  = 1*10**-4
#    } else if (heat_flux_Burgard  > 10*10**-4){
#      heat_flux_Burgard  = 10*10**-4
#    }
    
    heat_flux_ISMIP6_nonlocal = as.numeric(current_state['heat_flux_ISMIP6_nonlocal']) + rnorm(1, mean = 0, sd = step_size[7])
#    if (heat_flux_ISMIP6_nonlocal  < 1*10**4){
#      heat_flux_ISMIP6_nonlocal  = 1*10**4
#    } else if (heat_flux_ISMIP6_nonlocal  > 4*10**4){
#      heat_flux_ISMIP6_nonlocal  = 4*10**4
#    }
 
    heat_flux_ISMIP6_nonlocal_slope = as.numeric(current_state['heat_flux_ISMIP6_nonlocal_slope']) + rnorm(1, mean = 0, sd = step_size[8])
#    if (heat_flux_ISMIP6_nonlocal_slope  < 1*10**6){
#      heat_flux_ISMIP6_nonlocal_slope  = 1*10**6
#    } else if (heat_flux_ISMIP6_nonlocal_slope  > 4*10**6){
#      heat_flux_ISMIP6_nonlocal_slope  = 4*10**6
#    }
    
    heat_flux_PICO = as.numeric(current_state['heat_flux_PICO']) + rnorm(1, mean = 0, sd = step_size[9])
#    if (heat_flux_PICO  < 0.1*10**-5){
#      heat_flux_PICO  = 0.1*10**-5
#    } else if (heat_flux_PICO  > 10*10**-5){
#      heat_flux_PICO  = 10*10**-5
#    }
    
    heat_flux_Plume = as.numeric(current_state['heat_flux_Plume']) + rnorm(1, mean = 0, sd = step_size[10])
#    if (heat_flux_Plume  < 1*10**-4){
#      heat_flux_Plume  = 1*10**-4
#    } else if (heat_flux_Plume  > 10*10**-4){
#      heat_flux_Plume  = 10*10**-4
#    }

    #if using scenario would resample from FAIR again
    GSAT_2300 = current_state$GSAT_2300
    GSAT_2300 = as.numeric(current_state['GSAT_2300']) + rnorm(1, mean = 0, sd = step_size[11])
    
    
    proposed_state <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                                      heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
    return(proposed_state)  
  }
  
  #function to calculate acceptance probability
  acceptance_prob <- function(current_state, proposed_state, obs, obs_var, fac) {
    
    #get prediction from current state
    #transform to time series
    current_pred <- lapply(1L:r, function(j) {predict(emu[[j]], current_state, type = "moments")}) 
    current_mean <- matrix( unlist(lapply(current_pred, function(j) j[c('mean')])), ncol=r)
    current_sd <- matrix( unlist(lapply(current_pred, function(j) j[c('sd')])), ncol=r)
    current_meanx <- sweep(current_mean %*% Vt, 2L, cc, "+")
    current_varx <- lapply(1L:nrow(current_sd), function(i) {
      as.vector(crossprod(current_sd[i, ] * Vt)) 
    })
    current_varx <- do.call("cbind", current_varx) 
    dim(current_varx) <- c(n, n)
    
    #now do it with proposed state
    proposed_pred <- lapply(1L:r, function(j) {predict(emu[[j]], proposed_state, type = "moments")}) 
    proposed_mean <- matrix( unlist(lapply(proposed_pred, function(j) j[c('mean')])), ncol=r)
    proposed_sd <- matrix( unlist(lapply(proposed_pred, function(j) j[c('sd')])), ncol=r)
    proposed_meanx <- sweep(proposed_mean %*% Vt, 2L, cc, "+")    
    proposed_varx <- lapply(1L:nrow(proposed_sd), function(i) {
      as.vector(crossprod(proposed_sd[i, ] * Vt)) 
    })
    proposed_varx <- do.call("cbind", proposed_varx) 
    dim(proposed_varx) <- c(n, n)
    
    #check whether proposed state is within the bounds (effectively implementing the prior -- set acceptable probability to zero if not)
    #print(proposed_state)
    isinbound_GSAT                             = (proposed_state["GSAT_2300"] >= min(X$GSAT_2300, na.rm = TRUE) & proposed_state["GSAT_2300"] <=   max(X$GSAT_2300, na.rm = TRUE)) #set ranges of GSAT to be min and max of simulations 
    isinbound_lapse_rate                       = (proposed_state["lapse_rate"] >= -12 & proposed_state["lapse_rate"] <=  -5)
    isinbound_refreeze                         = (proposed_state["refreeze"] >= 0 & proposed_state["refreeze"] <=  15)
    isinbound_refreeze_frac                    = (proposed_state["refreeze_frac"] >= 0.2 & proposed_state["refreeze_frac"] <=  0.8)
    isinbound_PDD_ice                          = (proposed_state["PDD_ice"] >= 4 & proposed_state["PDD_ice"] <=  12)
    isinbound_PDD_snow                         = (proposed_state["PDD_snow"] >= 0 & proposed_state["PDD_snow"] <=  6)
    isinbound_heat_flux_Burgard                = (proposed_state["heat_flux_Burgard"] >= 1*10**-4 & proposed_state["heat_flux_Burgard"] <=  10*10**-4)
    isinbound_heat_flux_ISMIP6_nonlocal        = (proposed_state["heat_flux_ISMIP6_nonlocal"] >= 1*10**4 & proposed_state["heat_flux_ISMIP6_nonlocal"] <= 4*10**4)
    isinbound_heat_flux_ISMIP6_nonlocal_slope  = (proposed_state["heat_flux_ISMIP6_nonlocal_slope"] >= 1*10**6 & proposed_state["heat_flux_ISMIP6_nonlocal_slope"] <=  4*10**6)
    isinbound_heat_flux_PICO                   = (proposed_state["heat_flux_PICO"] >= 0.1*10**-5 & proposed_state["heat_flux_PICO"] <=   10*10**-5)
    isinbound_heat_flux_Plume                  = (proposed_state["heat_flux_Plume"] >= 1*10**-4 & proposed_state["heat_flux_Plume"] <=  10*10**-4)
    
    
    isinbound <- c(isinbound_GSAT,isinbound_lapse_rate, isinbound_refreeze,isinbound_refreeze_frac,isinbound_PDD_ice,isinbound_PDD_snow,
                   isinbound_heat_flux_Burgard,isinbound_heat_flux_ISMIP6_nonlocal,isinbound_heat_flux_ISMIP6_nonlocal_slope,
                   isinbound_heat_flux_PICO,isinbound_heat_flux_Plume)
    #print(isinbound)
  
    all_within_bounds <- all(isinbound)
    
    #print(all_within_bounds)
    
    #readline()
    #current likelihood using obs
    #variance set to obs error, obs_sig, plus a model error, set to fac*obs_sig
    
    #old way
    #current_likelihood <- exp(-0.5*sum((obs - current_meanx[6:13])**2/((obs_sig + fac*obs_sig + diag(current_varx)[6:13]))))
    
    #new way
    M_current = diag(obs_sig) + fac*diag(obs_sig) + current_varx[6:13, 6:13] #covariance matrix 
    current_obs_emu_diff <- (obs - current_meanx[6:13])
    current_obs_emu_diff <- matrix(current_obs_emu_diff, nrow = 1, ncol = length(current_obs_emu_diff))
    current_loglikelihood <- current_obs_emu_diff %*% solve(M_current) %*% t(current_obs_emu_diff)
   current_likelihood <- exp(-0.5*current_loglikelihood - 0.5*log(det(M_current))) 
   # current_likelihood <- exp(-0.5*current_loglikelihood) 
    
    #take out all emulator uncertainty
    #current_likelihood <- exp(-0.5*sum((obs - current_meanx[6:13])**2/((obs_sig + fac*obs_sig ))))
    
    #proposed likelihood
    #proposed_likelihood <- exp(-0.5*sum((obs - proposed_meanx[6:13])**2/((obs_sig + fac*obs_sig + diag(proposed_varx)[6:13]))))
    
    
    M_proposed = diag(obs_sig) + fac*diag(obs_sig) + proposed_varx[6:13, 6:13] #covariance matrix 
    proposed_obs_emu_diff <- (obs - proposed_meanx[6:13])
    proposed_obs_emu_diff <- matrix(proposed_obs_emu_diff, nrow = 1, ncol = length(proposed_obs_emu_diff))
    proposed_loglikelihood <- proposed_obs_emu_diff %*% solve(M_proposed) %*% t(proposed_obs_emu_diff)
    proposed_likelihood <- exp(-0.5*proposed_loglikelihood - 0.5*log(det(M_proposed)))
   # proposed_likelihood <- exp(-0.5*proposed_loglikelihood)
    
    #take out all emulator uncertainty
    #proposed_likelihood <- exp(-0.5*sum((obs -  proposed_meanx[6:13])**2/((obs_sig + fac*obs_sig ))))
    
    #metropolis ratio
    alpha <- min(1, proposed_likelihood / current_likelihood)
    
    if (!(all_within_bounds)){
      alpha <- 0 #if you're outside the bounds, return zero
      #print("rejected")
    }
    #print(alpha)
    return(alpha)
  }
  
  #empty data frame for accepted sample
  samples <- data.frame(matrix(vector(), chain_length, length(current_state)))
  slr_trajectories <- data.frame(matrix(vector(), chain_length, 70))
  slr_trajectories_ssp119 <- data.frame(matrix(vector(), chain_length, 70))
  slr_trajectories_ssp126 <- data.frame(matrix(vector(), chain_length, 70))
  slr_trajectories_ssp245 <- data.frame(matrix(vector(), chain_length, 70))
  slr_trajectories_ssp370 <- data.frame(matrix(vector(), chain_length, 70))
  slr_trajectories_ssp585 <- data.frame(matrix(vector(), chain_length, 70))
  
  colnames(samples) <- colnames(X)
  accept <- c()
  
  # load the FAIR data
  f2path = './FAIR_data/'
  ssp <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")
  FAIRsamples = NULL
  for( s in ssp){
    #ncin <- ncdf4::nc_open(Sys.glob(filef.path(f2path, s, "*.nc")))
    ncin <- ncdf4::nc_open(paste0(f2path,s,".temperature.fair.temperature_climate.nc"))
    st <- as.data.frame(ncdf4::ncvar_get(ncin, paste0(s, "/surface_temperature")))
    st  <- cbind(rep(s, dim(st)[1]), st)
    rbind(FAIRsamples,data.frame(st))->FAIRsamples
  }
  year <- ncdf4::ncvar_get(ncin,"year")
  colnames(FAIRsamples) <- c('scenario',paste0("y", year))
  FAIRsamples$ystart <- rowMeans(subset(FAIRsamples,select=y2015:y2044))
  FAIRsamples$yend <- rowMeans(subset(FAIRsamples,select=y2271:y2300))
  FAIRsamples$GSAT_2300 <- (FAIRsamples$yend-FAIRsamples$ystart)
  
  
  #run Metropolis-Hastings iterations
  for (i in 1:chain_length) {
    # Propose new state
    proposed_state <- sample_proposal(current_state)
    
    #print(i)
    
    # Calculate acceptance probability
    alpha <- acceptance_prob(current_state, proposed_state, obs, obs_var, fac)
    
    # Accept or reject based on uniform random draw
    u <- runif(1)
    if (u < alpha) {
      current_state <- proposed_state
      accept <- c(accept, 1)
      #print("accept")
    } else{
      accept <- c(accept, 0)
      #print("reject")
    }
    
    # Store accepted state
    samples[i,] <- unlist(current_state)
    accept_ratio <- length(accept[accept == 1])/length(accept)
    
    # Compute the trajectory
    current_pred <- lapply(1L:r, function(j) {predict(emu[[j]], current_state, type = "moments")}) 
    current_mean <- matrix( unlist(lapply(current_pred, function(j) j[c('mean')])), ncol=r)
    current_meanx <- sweep(current_mean %*% Vt, 2L, cc, "+")
    slr_trajectories[i,] <- current_meanx
    
    # repeat this, with the GSAT replaced by a sample from FAIR from SSPs
    
    # ssp119
    current_state_ssp119 <- current_state
    current_state_ssp119$GSAT_2300 <- sample(FAIRsamples$GSAT_2300[FAIRsamples$scenario == "ssp119"], 1)
    current_pred_ssp119 <- lapply(1L:r, function(j) {predict(emu[[j]], current_state_ssp119, type = "moments")}) 
    current_mean_ssp119 <- matrix( unlist(lapply(current_pred_ssp119, function(j) j[c('mean')])), ncol=r)
    current_meanx_ssp119 <- sweep(current_mean_ssp119 %*% Vt, 2L, cc, "+")
    slr_trajectories_ssp119[i,] <- current_meanx_ssp119
    
    # ssp126
    current_state_ssp126 <- current_state
    current_state_ssp126$GSAT_2300 <- sample(FAIRsamples$GSAT_2300[FAIRsamples$scenario == "ssp126"], 1)
    current_pred_ssp126 <- lapply(1L:r, function(j) {predict(emu[[j]], current_state_ssp126, type = "moments")}) 
    current_mean_ssp126 <- matrix( unlist(lapply(current_pred_ssp126, function(j) j[c('mean')])), ncol=r)
    current_meanx_ssp126 <- sweep(current_mean_ssp126 %*% Vt, 2L, cc, "+")
    slr_trajectories_ssp126[i,] <- current_meanx_ssp126
    
    # ssp245
    current_state_ssp245 <- current_state
    current_state_ssp245$GSAT_2300 <- sample(FAIRsamples$GSAT_2300[FAIRsamples$scenario == "ssp245"], 1)
    current_pred_ssp245 <- lapply(1L:r, function(j) {predict(emu[[j]], current_state_ssp245, type = "moments")}) 
    current_mean_ssp245 <- matrix( unlist(lapply(current_pred_ssp245, function(j) j[c('mean')])), ncol=r)
    current_meanx_ssp245 <- sweep(current_mean_ssp245 %*% Vt, 2L, cc, "+")
    slr_trajectories_ssp245[i,] <- current_meanx_ssp245
    
    # ssp370
    current_state_ssp370 <- current_state
    current_state_ssp370$GSAT_2300 <- sample(FAIRsamples$GSAT_2300[FAIRsamples$scenario == "ssp370"], 1)
    current_pred_ssp370 <- lapply(1L:r, function(j) {predict(emu[[j]], current_state_ssp370, type = "moments")}) 
    current_mean_ssp370 <- matrix( unlist(lapply(current_pred_ssp370, function(j) j[c('mean')])), ncol=r)
    current_meanx_ssp370 <- sweep(current_mean_ssp370 %*% Vt, 2L, cc, "+")
    slr_trajectories_ssp370[i,] <- current_meanx_ssp370
    
    # ssp585
    current_state_ssp585 <- current_state
    current_state_ssp585$GSAT_2300 <- sample(FAIRsamples$GSAT_2300[FAIRsamples$scenario == "ssp585"], 1)
    current_pred_ssp585 <- lapply(1L:r, function(j) {predict(emu[[j]], current_state_ssp585, type = "moments")}) 
    current_mean_ssp585 <- matrix( unlist(lapply(current_pred_ssp585, function(j) j[c('mean')])), ncol=r)
    current_meanx_ssp585 <- sweep(current_mean_ssp585 %*% Vt, 2L, cc, "+")
    slr_trajectories_ssp585[i,] <- current_meanx_ssp585
    
    
    if (i %% 100 == 0) {
      print(i)
    }
    
    
  }
  
  #remove burn-in period
  posterior_samples <- samples[burn_in:dim(samples)[1],]
  posterior_trajectories <- slr_trajectories[burn_in:dim(slr_trajectories)[1],]
  posterior_trajectories_ssp119 <- slr_trajectories_ssp119[burn_in:dim(slr_trajectories)[1],]
  posterior_trajectories_ssp126 <- slr_trajectories_ssp126[burn_in:dim(slr_trajectories)[1],]
  posterior_trajectories_ssp245 <- slr_trajectories_ssp245[burn_in:dim(slr_trajectories)[1],]
  posterior_trajectories_ssp370 <- slr_trajectories_ssp370[burn_in:dim(slr_trajectories)[1],]
  posterior_trajectories_ssp585 <- slr_trajectories_ssp585[burn_in:dim(slr_trajectories)[1],]

  
  #output <- list(posterior, accept_ratio)
  print(paste('Acceptance ratio is', accept_ratio))
  # Return list of samples
  return(list(posterior_samples = posterior_samples, posterior_trajectories = posterior_trajectories, posterior_trajectories_ssp119 = posterior_trajectories_ssp119, posterior_trajectories_ssp126 = posterior_trajectories_ssp126,posterior_trajectories_ssp245 = posterior_trajectories_ssp245,posterior_trajectories_ssp370 = posterior_trajectories_ssp370,posterior_trajectories_ssp585 = posterior_trajectories_ssp585  ))
  return(posterior)
}
