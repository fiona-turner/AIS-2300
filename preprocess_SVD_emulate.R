# Preprocess the data, perform the SVD, and emulate the inmportant components of the SVD


options(mc.cores = 4L) #set cores to be 4 logical cores
par(mfrow = c(1, 1))  # set graphics layout to display one plot per window or figure

############################ load the raw data #################################

fpath <- "./" #set the file path
SLE <- fread(file.path(fpath, "SLE_SIMULATIONS_AIS_final_230725.csv")) #data on the simulations, 22 columns of metadata, followed by 351 years of model output yearly from 1950 to 2300 (=373 columns total). 2200 simulations in total, one for each row.
SLE <- fread(file.path(fpath, "AIS_SIMULATIONS_ZWALLY00_cm_SLE_2014_250729.csv")) #data on the simulations, 30 columns of metadata, followed by 351 years of model output yearly from 1950 to 2300 (=373 columns total). 2200 simulations in total, one for each row.

FOR <- fread(file.path(fpath, "CLIMATE_FORCING_240127.csv")) #data on the GCMS, size 86 x 456. First five columns are metadata, then 451 year observations of global surface air temperature from 1850 to 2300 (= 56 columns total). 86 GCMs in total. Note that only a subset of 12 run to 2300 

############################ data preprocessing ################################

setnames(SLE, "model", "simu")  #renames the column "model" to "simu"
SLE[, simu := factor(simu, c("Kori", "PISM"))] #makes the simu column into a factor format

#just working with phase 2, to speed things up, i.e. remove any simulations in simulation phase 1 from the simulation data
#SLE <- SLE[Phase == "2", ]  #don't need this with updated simulations


#SLE[, melt_param := factor(melt_param, c("PICO", "Plume", "Burgard",
#                                         "ISMIP6_nonlocal", "ISMIP6_nonlocal_slope"))] #make the melt parametrization in the simulation data into a factor variable

SLE[, melt_param := factor(melt_param, c("PICO", "Plume", "ISMIP6_local",
                                         "ISMIP6_nonlocal", "ISMIP6_nonlocal_slope"))] #make the melt parametrization in the simulation data into a factor variable, no Burgard in the new


SLE[, init_atmos := factor(init_atmos, c("MARv3.11", "RACMO2.3p2"))] #make the atmospheric forcing in the simulation data into a factor variable


SLE[, init_ocean := factor(init_ocean, c("ISMIP6_3D", "Schmidtko_2D", "Reese"))] #make the ocean forcing in the simulation data into a factor variable

#SLE[, sliding_exponent := factor(sliding_exponent,unique(SLE$sliding_exponent))] #make the sliding exponent in the simulation data into a factor variable


SLE[, simoc := factor(paste0(SLE$simu, "_", SLE$init_ocean))] #make a new column, called simoc, which is the model and the ocean combination

SLE$scenario <- gsub("\\s|\\.00", "", SLE$scenario) #change the format of the scenario column so that it matches the forcing scenario -- need this for GSAT calculation below
SLE$scenario <- gsub("[^A-Za-z0-9]", "", SLE$scenario)


## add global surface air temperature change to 2300 to the simulation data (we use GSAT_2299-- the temp at 2299 -- for CESM2-WACCM:SSP585 these have NaN at 2300)

tmp <- SLE[, .(GCM, scenario)] #temporary array, storing the GCM and scenario
tmp[, hash := paste0(GCM, ":", scenario)] #add a column, "hash", which has model and scenario lumped

FOR[, hash := paste0(GCM, ":", scenario)] #add this hash column to the forcing array
FOR$y2300 <- ifelse(is.na(FOR$y2300), FOR$y2299, FOR$y2300) #if the 2300 temp is nan, set it to 2299
FOR$ystart <- rowMeans(subset(FOR,select=y2015:y2044)) #computes the row-wise mean of the columns from y2015 to y2044 in the FOR data frame, and stores the result in a new column called ystart
FOR$yend <- rowMeans(subset(FOR,select=y2271:y2300)) #computes the row-wise mean of the columns from y2271 to y2300 in the FOR data frame, and stores the result in a new column called yend

tmp[, GSAT_2300 := (FOR$yend[match(hash, FOR$hash)]-FOR$ystart[match(hash, FOR$hash)])] #makes a new column in tmp, called GSAT_2300, which is the difference of the corresponding start and end temp, computed from FOR
SLE[, GSAT_2300 := tmp$GSAT_2300] #make a column in SLE, equal to tmp


ycols <- grep("^y[[:digit:]]{4}", names(SLE), value = TRUE) #extracts the names of columns in the SLE data frame that start with y followed by exactly 4 digits (e.g., y2015, y2300, etc.), and stores them in the vector ycols.

SLE$y1950[is.na(SLE$y1950)] <- SLE$y1951[is.na(SLE$y1950)] #set the 1950 entry equal to the 1951 entry if it's a nan (PISM)


#set SLE respective to 2000 and extract 5 year means
#SLE[,23:373] <- sweep(SLE[,23:373], 1, SLE$y2000) #uses the sweep() function to subtract the y2000 column values from each row of columns 23 (corresponding to 1950) to 373 (corresponding to 2300) in the SLE data frame.
SLE[,31:381] <- sweep(SLE[,31:381], 1, SLE$y2000) #uses the sweep() function to subtract the y2000 column values from each row of columns 23 (corresponding to 1950) to 373 (corresponding to 2300) in the SLE data frame.
Z <- data.frame(SLE[, ycols, with=FALSE]) #new data frame with only sea level changes, i.e. no metadata


fence <- 1900 + c(50, seq(from = 55, to = 400, by = 5)) #array of 5 year intervals from 1950 to 2300, equivalent to 1950:5:2300
k <- length(fence) - 1 # number of bins

yy <- as.numeric(gsub("^y", "", colnames(Z))) #all year values, obtained by removing the "y" from the year column names
bin <- findInterval(yy, fence, rightmost.closed = TRUE) #say which five year bin you are in in yy
ave <- sapply(1L:k, function(i) {
  rowMeans(Z[, bin == i, drop=FALSE])
}) #computes row-wise averages of subsets of columns in Z, based on groupings defined by the vector bin, and stores the result in ave. ave is 1400 (simulations) x 70 (number of 5 year chunks)
n <- ncol(ave) # number of time slices, i.e. 790
years <- seq(1955, 2300, 5) #years that the 5 year timeslices begin

##################### Plot the timeseries of sea level #########################
if (plot_sea_level_timeseries){
plot(0,0,xlim = c(1955,2300),ylim = c(-1,7),type = "n",xlab = "Year", ylab = paste("Sea level contribution relative to 2000 (m)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 2000, lwd = 0.5)
abline(h = 0, lwd = 0.5)

for (i in 1:length(Z[SLE$scenario == 'SSP585',])){
  lines(yy, SLE[SLE$scenario == 'SSP585'][i,31:381], col = rgb(132, 11, 34, maxColorValue = 255, alpha = 100), lwd = 2)
}
for (i in 1:length(Z[SLE$scenario == 'SSP126',])){
  lines(yy, SLE[SLE$scenario == 'SSP126'][i,31:381], col = rgb(29, 51, 84, maxColorValue = 255, alpha = 100), lwd = 2)
}

legend("topleft", legend=c("SSP1-2.6", "SSP5-8.5"),
       text.col=c(rgb(29, 51, 84, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1, bty = "n")

#dev.print(pdf, width = 11.69, height = 8.27, "../Data/AIS/SLE_time.png")  
}

# here

###################### Do the SVD and make the emulators #######################
cc <- colMeans(ave) #average over the 1400 simulations ("centroid") at each timeslice

# Use sweep to centre data (subtract column means from columns of 5 year averages), then do SVD 
decentroid_simtimeslice <- sweep(ave, 2L, cc, "-")
decomp <- svd(decentroid_simtimeslice) #returns SVD object, with decentroid_simtimeslice = UDV'
dd2 <- decomp$d^2 #decomp$d is the singular values of decentroid_simtimeslice
scree <- cumsum(dd2) / sum(dd2) 
r <- which.max(scree >= thresh) #find where the cumulative sum of the singular values is above the threshold (set in function.R)
U <- decomp$u[, 1L:r, drop=FALSE]  #decomp$u has size 1400 x 70. Take only the first r rows, we'll emulate these rows 
Vt <- (decomp$d * t(decomp$v))[1L:r, , drop=FALSE] 

#collect inputs to be used and create heat flux parameterisations
#set output to SVD components
X <- SLE[, .(GSAT_2300, simoc, init_atmos, lapse_rate, 
             refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param,sliding_exponent,overturning_PICO)] #take only the parameter columns in simulation data
heat_flux <- grep("^heat_flux_", names(SLE), value = TRUE) 
X <- cbind(X, SLE[, heat_flux, with = FALSE]) #add the heat flux names
y <- U #assign this bc why not

tmp <- X[, c("melt_param", heat_flux), with=FALSE]
for (i in 1:5) {
  nm <- paste0("heat_flux_", levels(SLE$melt_param)[i])
  nom <- unique(tmp[melt_param != i, nm, with=FALSE])
  show(nom)
}


message("building the emulator")

#initialize storage for the emulators
qemu0 <- list()
tune <- list()
emu <- list()


for (j in 1L:r) {
  
  qemu0[[j]] <- make_qemu(X, U[ , j]) #make the qemu on the jth component of U
  tune[[j]] <- tune_qemu(qemu0[[j]], nrep = 2, plotit = FALSE)
  emu[[j]] <- tune[[j]]$tuned_qemu
  
  }

