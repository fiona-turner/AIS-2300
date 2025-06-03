# AIS-2300
## **Random forest emulator for AIS contribution to sea level rise 1950 - 2300**

Given 1400 simulations of two AIS models, Kori and PISM, we build a random forest emulator, creating a linear regression using perturbed parameters and a RF for the residual.
Output of model is time series of SLE, 1950 - 2300. In order to emulate whole time series at once, we use singular value decomposition to transform 350-year data (made of 70 5-year timeslices) to *r* components, were *r* is the number of components that represent >99% of variance within the data.

## Prerequisites 
Install required packages:

1) data.table. This is an extension of 'data.frame'. Install this using the command: 
install.packages('data.table')  

2) quantile regression forests. Install this using the command:  
install.packages('quantregForest')  

3) - quick emulator, built on top of 'quantregForest' package. This has to be installed from the qemu folder, included in this repo. To install, first ensure you are in the main directory, and then run:  
install.packages("./qemu/",  repos = NULL,  type = "source")  

## Running 
1. Build the emulator
   
   jointemu_svd.R
   
   Processes the simulation data and builds emulators for the entire time series. Runs leave-one-out cross validation (LOOCV) on a subset of the data, and creates main effects (MEFF) plots. **At the moment, the LOOCV and MEFF plots are for the raw emulator output, giving *r* plots. Later editions of this will calculate LOOCV and MEFF for the transformed output, for the years 2100, 2200 and 2300.**

2. Predict distirbutions for 5 SSPs
    
   predictemu_svd.R
   
   Builds datasets of samples of GSAT under 5 SSPs, and the model parameters, and predicts SLE projections using the RF emulators. Raw emulator is transformed to a predicted mean and variance. Distributions of SLE under each SSP are then made by sampling from a MVN distribution using the predicted means and variances. Outputs sampled distributions.

3. Plotting the predicted distributions
   
   plots_svd.R
   
   Creates a series of plots, including:
   + Time series of mean predictions +- 3 standard deviations
   + Time series of median predictions with [5, 95]% intervals shaded
   + SLE at 2300 vs GSAT
   + SLE at 2300 vs model parameters
   + Density plots of SLE at 2300 for each SSP, with [5, 95]% quantiles marked
   + Histograms of SLE at 2300 for each SSP

How to cite this material:
10.5281/zenodo.10639736

