mh_calib <- function(obs, obs_sig, fac, step_size, chain_length, burn_in){
  ## a function to run Metropolis-Hastings on the random forest emulator
  # inputs:
  # obs = the observations to be used in the likelihood calculation
  # obs_sig = the observational error
  # fac = the factor we use to calculate model error, assuming mod_sig = fac*obs_sig
  # step_size = the size of the noise component used to generate new states
  # chain_length = length of markov chain we wish to sample
  # burn_in = number of samples we remove from beginning of chain
  
  #get a GSAT value according to scenario
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
    #tmp <- FORpred[scenario == scenario] 
  } else {
    ## or IPCC FAIR data
    fpath <- "~/Documents/Emulators/Data/"
    FORpred <- fread(file.path(fpath, "CLIMATE_FORCING_IPCC_AR6_230706.csv"))
    FORpred$ystart <- rowMeans(subset(FORpred,select=y2015:y2044))
    FORpred$yend <- rowMeans(subset(FORpred,select=y2271:y2300))
    FORpred[, GSAT_2300 := (FORpred$yend-FORpred$ystart)]
    #tmp <- FORpred[scenario == scenario]
  }
  #set GSAT to mean of FAIR simulations
  GSAT_2300 <- mean(FORpred$GSAT_2300)
  #if using scenario then would set to a random sample from tmp
  
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

  #save variables in data frame as current state
  current_state <- as.list(data.frame(GSAT_2300, simoc, init_atmos, lapse_rate, refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param, 
                               heat_flux_PICO, heat_flux_Plume, heat_flux_Burgard, heat_flux_ISMIP6_nonlocal, heat_flux_ISMIP6_nonlocal_slope))
  
  
  #function to create a proposed state, by taking a step away from current state
  sample_proposal <- function(current_state) {
    #if using scenario would resample from FAIR again
    GSAT_2300 = current_state$GSAT_2300
    simoc = sample(unique(X$simoc), 1, TRUE)
    init_atmos = sample(unique(X$init_atmos), 1, TRUE)
    lapse_rate = as.numeric(current_state['lapse_rate']) + rnorm(1, mean = 0, sd = step_size[1])
    if (lapse_rate < -12){
      lapse_rate = -12
    } else if (lapse_rate > -5){
      lapse_rate = -5
    }
    refreeze = as.numeric(current_state['refreeze']) + rnorm(1, mean = 0, sd = step_size[2])
    if (refreeze < 0){
      refreeze = 0
    } else if (refreeze > 15){
      refreeze = 15
    }
    refreeze_frac = as.numeric(current_state['refreeze_frac']) + rnorm(1, mean = 0, sd = step_size[3])
    if (refreeze_frac < 0.2){
      refreeze_frac = 0.2
    } else if (refreeze_frac > 0.8){
      refreeze_frac = 0.8
    }
    PDD_ice = as.numeric(current_state['PDD_ice']) + rnorm(1, mean = 0, sd = step_size[4])
    if (PDD_ice  < 4){
      PDD_ice  = 4
    } else if (PDD_ice  > 12){
      PDD_ice  = 12
    }
    PDD_snow = as.numeric(current_state['PDD_snow']) + rnorm(1, mean = 0, sd = step_size[5])
    if (PDD_snow  < 0){
      PDD_snow  = 0
    } else if (PDD_snow  > 6){
      PDD_snow  = 6
    }
    melt_param = sample(unique(X$melt_param), 1, TRUE)
    heat_flux_Burgard = as.numeric(current_state['heat_flux_Burgard']) + rnorm(1, mean = 0, sd = step_size[6])
    if (heat_flux_Burgard  < 1*10**-4){
      heat_flux_Burgard  = 1*10**-4
    } else if (heat_flux_Burgard  > 10*10**-4){
      heat_flux_Burgard  = 10*10**-4
    }
    heat_flux_ISMIP6_nonlocal = as.numeric(current_state['heat_flux_ISMIP6_nonlocal']) + rnorm(1, mean = 0, sd = step_size[7])
    if (heat_flux_ISMIP6_nonlocal  < 1*10**4){
      heat_flux_ISMIP6_nonlocal  = 1*10**4
    } else if (heat_flux_ISMIP6_nonlocal  > 4*10**4){
      heat_flux_ISMIP6_nonlocal  = 4*10**4
    }
    heat_flux_ISMIP6_nonlocal_slope = as.numeric(current_state['heat_flux_ISMIP6_nonlocal_slope']) + rnorm(1, mean = 0, sd = step_size[8])
    if (heat_flux_ISMIP6_nonlocal_slope  < 1*10**6){
      heat_flux_ISMIP6_nonlocal_slope  = 1*10**6
    } else if (heat_flux_ISMIP6_nonlocal_slope  > 4*10**6){
      heat_flux_ISMIP6_nonlocal_slope  = 4*10**6
    }
    heat_flux_PICO = as.numeric(current_state['heat_flux_PICO']) + rnorm(1, mean = 0, sd = step_size[9])
    if (heat_flux_PICO  < 0.1*10**-5){
      heat_flux_PICO  = 0.1*10**-5
    } else if (heat_flux_PICO  > 10*10**-5){
      heat_flux_PICO  = 10*10**-5
    }
    heat_flux_Plume = as.numeric(current_state['heat_flux_Plume']) + rnorm(1, mean = 0, sd = step_size[10])
    if (heat_flux_Plume  < 1*10**-4){
      heat_flux_Plume  = 1*10**-4
    } else if (heat_flux_Plume  > 10*10**-4){
      heat_flux_Plume  = 10*10**-4
    }

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
    #current likelihood using obs
    #variance set to obs error, obs_sig, plus a model error, set to fac*obs_sig
    current_likelihood <- exp(-0.5*sum((obs - current_meanx[6:13])**2/((obs_sig + fac*obs_sig + diag(current_varx)[6:13]))))
    
    #proposed likelihood
    proposed_likelihood <- exp(-0.5*sum((obs - proposed_meanx[6:13])**2/((obs_sig + fac*obs_sig + diag(proposed_varx)[6:13]))))
    
    #metropolis ratio
    alpha <- min(1, proposed_likelihood / current_likelihood)
    return(alpha)
  }
  
  #empty data frame for accepted sample
  samples = data.frame(matrix(vector(), chain_length, length(current_state)))
  colnames(samples) <- colnames(X)
  accept <- c()
  
  #run Metropolis-Hastings iterations
  for (i in 1:chain_length) {
    # Propose new state
    proposed_state <- sample_proposal(current_state)
    
    # Calculate acceptance probability
    alpha <- acceptance_prob(current_state, proposed_state, obs, obs_var, fac)
    
    # Accept or reject based on uniform random draw
    u <- runif(1)
    if (u < alpha) {
      current_state <- proposed_state
      accept <- c(accept, 1)
    } else{
      accept <- c(accept, 0)
    }
    
    # Store accepted state
    samples[i,] <- unlist(current_state)
    accept_ratio <- length(accept[accept == 1])/length(accept)
  }
  
  #remove burn-in period
  posterior <- samples[burn_in:dim(samples)[1],]
  
  #output <- list(posterior, accept_ratio)
  print(paste('Acceptance ratio is', accept_ratio))
  # Return list of samples
  return(posterior)
}
