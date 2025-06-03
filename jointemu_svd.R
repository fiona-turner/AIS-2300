options(mc.cores = 4L)
par(mfrow = c(1, 1))

### load the raw data and process

fpath <- "./"
SLE <- fread(file.path(fpath, "SLE_SIMULATIONS_AIS_final_230725.csv")) #data on the simulations, 22 columns of metadata, followed by 351 years of model output yearly from 1950 to 2300. 2200 simulations in total.
FOR <- fread(file.path(fpath, "CLIMATE_FORCING_240127.csv")) #data on the GCMS, size 86 x 456. First five columns are metadata, then 450 year observations of temperature. 86 GCMs in total.

## identify factors and levels

setnames(SLE, "model", "simu")
SLE[, simu := factor(simu, c("Kori", "PISM"))]

#just working with phase 2, to speed things up
SLE <- SLE[Phase == "2", ]


SLE[, melt_param := factor(melt_param, c("PICO", "Plume", "Burgard",
                                         "ISMIP6_nonlocal", "ISMIP6_nonlocal_slope"))]


SLE[, init_atmos := factor(init_atmos, c("MARv3.11", "RACMO2.3p2"))]


SLE[, init_ocean := factor(init_ocean, c("ISMIP6_3D", "Schmidtko_2D", "Reese"))]


SLE[, simoc := factor(paste0(SLE$simu, "_", SLE$init_ocean))]

 
## add GSAT_2300 (using GSAT_2299 owing to NA in CESM2-WACCM:SSP585)
## turn GSAT time series into 30 year time slices
tmp <- SLE[, .(GCM, scenario)]
tmp[, hash := paste0(GCM, ":", scenario)]

FOR[, hash := paste0(GCM, ":", scenario)]
FOR$y2300 <- ifelse(is.na(FOR$y2300), FOR$y2299, FOR$y2300)
FOR$ystart <- rowMeans(subset(FOR,select=y2015:y2044))
FOR$yend <- rowMeans(subset(FOR,select=y2271:y2300))
tmp[, GSAT_2300 := (FOR$yend[match(hash, FOR$hash)]-FOR$ystart[match(hash, FOR$hash)])]
SLE[, GSAT_2300 := tmp$GSAT_2300]


ycols <- grep("^y[[:digit:]]{4}", names(SLE), value = TRUE)

#set SLE respective to 2000
#then create 5 year averages
SLE[,23:373] <- sweep(SLE[,23:373], 1, SLE$y2000)
Z <- data.frame(SLE[, ycols, with=FALSE])

fence <- 1900 + c(50, seq(from = 55, to = 400, by = 5))
k <- length(fence) - 1 # number of bins

yy <- as.numeric(gsub("^y", "", colnames(Z)))
bin <- findInterval(yy, fence, rightmost.closed = TRUE)
ave <- sapply(1L:k, function(i) {
  rowMeans(Z[, bin == i, drop=FALSE])
})
n <- ncol(ave)
years <- seq(1955, 2300, 5)

## plotting time series to see it
plot(0,0,xlim = c(1955,2300),ylim = c(-1,7),type = "n",xlab = "Year", ylab = paste("Sea level contribution relative to 2000 (m SLE)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 2000, lwd = 0.5)
abline(h = 0, lwd = 0.5)

for (i in 1:length(Z[SLE$scenario == 'SSP585',])){
  lines(yy, SLE[SLE$scenario == 'SSP585'][i,23:373], col = rgb(132, 11, 34, maxColorValue = 255, alpha = 100), lwd = 2)
}
for (i in 1:length(Z[SLE$scenario == 'SSP126',])){
  lines(yy, SLE[SLE$scenario == 'SSP126'][i,23:373], col = rgb(29, 51, 84, maxColorValue = 255, alpha = 100), lwd = 2)
}

legend("topleft", legend=c("SSP1-2.6", "SSP5-8.5"),
       text.col=c(rgb(29, 51, 84, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1, bty = "n")

#dev.print(pdf, width = 11.69, height = 8.27, "../Data/AIS/SLE_time.png")  



## SVD
cc <- colMeans(ave)
# Use sweep to centre data (subtract column means from columns of 5 year averages)
# then do SVD 
decomp <- svd(sweep(ave, 2L, cc, "-"))
dd2 <- decomp$d^2
scree <- cumsum(dd2) / sum(dd2)
r <- which.max(scree >= thresh)
U <- decomp$u[, 1L:r, drop=FALSE]
Vt <- (decomp$d * t(decomp$v))[1L:r, , drop=FALSE]

#collect inputs to be used and create heat flux parameterisations
#set output to SVD components
X <- SLE[, .(GSAT_2300, simoc, init_atmos, lapse_rate, 
             refreeze, refreeze_frac, PDD_ice, PDD_snow, melt_param)]
heat_flux <- grep("^heat_flux_", names(SLE), value = TRUE)
X <- cbind(X, SLE[, heat_flux, with = FALSE])
y <- U

tmp <- X[, c("melt_param", heat_flux), with=FALSE]
for (i in 1:5) {
  nm <- paste0("heat_flux_", levels(SLE$melt_param)[i])
  nom <- unique(tmp[melt_param != i, nm, with=FALSE])
  show(nom)
}


message("building the emulator")

qemu0 <- list()
tune <- list()
emu <- list()


for (j in 1L:r) {
  qemu0[[j]] <- make_qemu(X, U[ , j])
  tune[[j]] <- tune_qemu(qemu0[[j]], nrep = 2, plotit = FALSE)
  emu[[j]] <- tune[[j]]$tuned_qemu}

