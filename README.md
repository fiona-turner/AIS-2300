# AIS-2300
## **Random forest emulator for AIS contribution to sea level rise 1950 - 2300**

Given 1400 simulations of two AIS models, Kori and PISM, we build a random forest emulator, creating a linear regression using perturbed parameters and a RF for the residual.
Output of model is time series of SLE, 1950 - 2300. In order to emulate whole time series at once, we use singular value decomposition to transform 350-year data (made of 70 5-year timeslices) to *r* components, were *r* is the number of components that represent >99.9% of variance within the data. We perform validation tests and sensitivity analysis through LOOCV and MEFF, and then build projections for five SSPs. These are calibrated using two different methods: direct weighting and MCMC. 


Install required packages:
install.packages('data.table') - extension of 'data.frame'
install.packages('quantregForest') - quantile regression forests
install.packages(qemu.tar.gz, repos = NULL, type =‘‘source’’) - quick emulator, built on top of 'quantregForest' package

1. Master script that calls functions

   functions.R

   Set parameters to choose threshold for SVD, whether to save output, and how to sample for predictions. Then run the scripts to build and test the emulator, build projections and calibrate with two different methods.

2. Build the emulator
   
   jointemu_svd.R
   
   Processes the simulation data and builds emulators for the entire time series.

3. Validate and test

   loocvandmeff.R

   Runs leave-one-out cross validation (LOOCV) on a subset of the data, and creates main effects (MEFF) plots. 

4. Predict distirbutions for 5 SSPs
    
   predictemu_svd.R
   
   Builds datasets of samples of GSAT under 5 SSPs, and the model parameters, and predicts SLE projections using the RF emulators. Heat flux can be nested into the melt parameterisation parameter. Factor variables can be sampled according to the weighting in the experimental design, or equally sampled. Raw emulator output is transformed to a predicted mean and variance. Distributions of SLE under each SSP are then made by sampling from a MVN distribution using the predicted means and variances. Outputs sampled distributions.

5. Plotting the predicted distributions
   
   plots_svd.R
   
   Creates a series of plots, including:
   + Time series of mean predictions +- 2 standard deviations
   + Time series of median predictions with [5, 95]% intervals shaded
   + SLE at 2300 vs GSAT
   + SLE at 2300 vs model parameters
   + Density plots of SLE at 2300 for each SSP, with [5, 95]% quantiles marked
   + Histograms of SLE at 2300 for each SSP
  
6. Load observations for calibration

   load_IMBIE.R

   Loads the raw IMBIE data for calibration, calculate 5-yearly values for the means and errors that correspond to our timeslices.

7. Calibrate the projections through direct weighting

   calibrate.R

   Uses the IMBIE data to weight the projections using a score built on a Gaussian likelihood.

8. Calibrate through MCMC

   mh.R

   Uses Gibbs sampling to perform MCMC, creating posterior distributions of the parameters.

After running these, we then build projections from the MCMC output and plot results.

9. Build SLE projections using posterior distributions of parameters.

   mh_projections.R

   Samples from FAIR for GSAT values, and using posterior samples from MCMC to build SLE projections.

10. Plot prior/posterior distributions
    
    mh_density.R

    Plot the prior and posterior distributions of the model parameters

11. Projections plot

    all_pred_plot.R

    Plot time series of the uncalibrated projections, the projections calibrated with MCMC, and the calibrated values to 2100.

How to cite this material:
10.5281/zenodo.10639736

