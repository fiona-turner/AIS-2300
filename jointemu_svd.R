## set working directory
setwd("~/")

library(data.table)
library(qemu)

## to save plots of LOOCV and MEFF set this to TRUE
save_test <- FALSE

## load the raw data and process
## set fpath to whereever you store the data files
fpath <- "~/Data/"
SLE <- fread(file.path(fpath, "SLE_SIMULATIONS_AIS_final_230725.csv"))
FOR <- fread(file.path(fpath, "CLIMATE_FORCING_230618.csv"))

## identify factors and levels

setnames(SLE, "model", "simu")
SLE[, simu := factor(simu, c("Kori", "PISM"))]

## just working with phase 2, to speed things up
SLE <- SLE[Phase == "2", ]


SLE[, melt_param := factor(melt_param, c("PICO", "Plume", "Burgard",
                                         "ISMIP6_nonlocal", "ISMIP6_nonlocal_slope"))]


SLE[, init_atmos := factor(init_atmos, c("MARv3.11", "RACMO2.3p2"))]


SLE[, init_ocean := factor(init_ocean, c("ISMIP6_3D", "Schmidtko_2D", "Reese"))]


SLE[, simoc := factor(paste0(SLE$simu, "_", SLE$init_ocean))]

 
## turn GSAT time series into 30 year time slices
## add GSAT_2300 (using GSAT_2299 owing to NA in CESM2-WACCM:SSP585)
## make it relative to 2015-2044
tmp <- SLE[, .(GCM, scenario)]
tmp[, hash := paste0(GCM, ":", scenario)]

FOR[, hash := paste0(GCM, ":", scenario)]
FOR$y2300 <- ifelse(is.na(FOR$y2300), FOR$y2299, FOR$y2300)
FOR$ystart <- rowMeans(subset(FOR,select=y2015:y2044))
FOR$yend <- rowMeans(subset(FOR,select=y2271:y2300))
tmp[, GSAT_2300 := (FOR$yend[match(hash, FOR$hash)]-FOR$ystart[match(hash, FOR$hash)])]
SLE[, GSAT_2300 := tmp$GSAT_2300]


## set SLE respective to 2000
## then create 5 year averages
SLE[,23:373] <- sweep(SLE[,23:373], 1, SLE$y2000)
ycols <- grep("^y[[:digit:]]{4}", names(SLE), value = TRUE)
Z <- data.frame(SLE[, ycols, with=FALSE])

fence <- 1900 + c(50, seq(from = 55, to = 400, by = 5))
k <- length(fence) - 1 # number of bins

yy <- as.numeric(gsub("^y", "", colnames(Z)))
bin <- findInterval(yy, fence, rightmost.closed = TRUE)
ave <- sapply(1L:k, function(i) {
  rowMeans(Z[, bin == i, drop=FALSE])
})
n <- ncol(ave)

## SVD
cc <- colMeans(ave)
## use sweep to centre data (subtract column means from columns of ave)
## then do SVD
decomp <- svd(sweep(ave, 2L, cc, "-"))
dd2 <- decomp$d^2
scree <- cumsum(dd2) / sum(dd2)
##  save first r components that represent more than 99% of variance in the data
r <- which.max(scree >= 0.99)  
U <- decomp$u[, 1L:r, drop=FALSE]
Vt <- (decomp$d * t(decomp$v))[1L:r, , drop=FALSE] 

## collect inputs to be used and create heat flux parameterisations
## set output to SVD components
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



message("** building emulator")

qemu0 <- list()
tune <- list()
emu <- list()

## for each of the r components, make the emulator, then  use tune_qemu to optimise ntry
## and nodesize, the hyperparameters of the RF
## then save the tuned emulator
message("** tuning the emulator")
for (j in 1L:r) {
  qemu0[[j]] <- make_qemu(X, U[ , j])
  tune[[j]] <- tune_qemu(qemu0[[j]], nrep = 2, plotit = FALSE)
  emu[[j]] <- tune[[j]]$tuned_qemu}



## run loocv - although this is on the raw emulator output, not transformed back to
## time series of SLE
message("** running loo script")
oo <- order(X$GSAT_2300)[seq(from = 1, to = dim(y)[1], length.out = 100)]
#loo <- loo_QEMU(emuB, subset = oo, plotit = FALSE)
loo <- lapply(1L:r, function(j) { LOO_qemu(emu[[j]], subset = oo, plotit = FALSE)})
mu <- sapply(loo, "[[", "predict") # m_out x r
up <- sapply(loo, "[[", "upper")
low <- sapply(loo, "[[", "lower")
sim <- sapply(loo, "[[", "actual")

for (j in 1L:r) {
    wrong <- sim[order(sim[,j]),j] < low[order(sim[,j]),j] | sim[order(sim[,j]),j] > up[order(sim[,j]),j]
    col_dots <- rep("deepskyblue4", length(sim[order(sim[,j]),j]))
    col_wrong <- rgb(243, 122, 107, maxColorValue = 255)
    col_dots[wrong] <- col_wrong
    plot(sim[order(sim[,j]),j],mu[order(sim[,j]),j], pch = 19, xlab = "Simulated values",
     ylab = "Emulated values", main = " ", col = col_dots, ylim=c(min(low[,j]),max(up[,j])), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
    arrows(sim[order(sim[,j]),j], low[order(sim[,j]),j], sim[order(sim[,j]),j], up[order(sim[,1]),1], length=0.05, angle=90, code=3, col = col_dots)
    abline(a = 0, b = 1, lwd = 0.5)
    if (save_test){
        dev.print(pdf, paste("./Kori+PISM_svd_qemu_LOO_", j,".pdf",sep=""))
    }
}



## run MEFF script, although again this is on raw emulator output
message("** running meff script")

meff <- list()
for (j in 1L:r){
  meff[[j]] <- MEFF_qemu(emu[[j]])
  if (save_test){
    dev.print(pdf, paste("./MEFF_emu", j,".pdf",sep=""))
  }
}

