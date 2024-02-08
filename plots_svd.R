setwd("~/")
## run the prediction code
source("predictemu_svd.R")

## create vector containing year values
years <- seq(1955, 2300, 5)

## to save plots, set this to TRUE
save_plot <- TRUE

## Colour scales used in Edwards et al. (2021):
## SSP/SCP scenario colours
## SSP1-19 rgb(30, 150, 132, maxColorValue = 255); for 60% transparency alpha = 153; for 20% transparency alpha = 51
## SSP1-26 rgb(29, 51, 84, maxColorValue = 255)
## SSP2-45 rgb(234, 221, 61, maxColorValue = 255)
## SSP3-70 rgb(242, 17, 17, maxColorValue = 255)
## SSP5-85 rgb(132, 11, 34, maxColorValue = 255)

## plot mean +/- 3 sd predictions for each SSP
plot(0,0,xlim = c(1955,2300),ylim = c(-1,5),type = "n",xlab = "Year", ylab = paste("Sea level contribution relative to 2000 (m SLE)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 1995, lwd = 0.5)
abline(h = 0, lwd = 0.5)

lines(years, colMeans(SLE585_meanx), col = rgb(132, 11, 34, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(colMeans(SLE585_meanx + 3*SLE585_sdx), rev(colMeans(SLE585_meanx - 3*SLE585_sdx))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 51), border = NA)

lines(years, colMeans(SLE370_meanx), col = rgb(242, 17, 17, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(colMeans(SLE370_meanx + 3*SLE370_sdx), rev(colMeans(SLE370_meanx - 3*SLE370_sdx))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 51), border = NA)

lines(years, colMeans(SLE245_meanx), col = rgb(234, 221, 61, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(colMeans(SLE245_meanx + 3*SLE245_sdx), rev(colMeans(SLE245_meanx - 3*SLE245_sdx))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 51), border = NA)

lines(years, colMeans(SLE126_meanx), col = rgb(29, 51, 84, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(colMeans(SLE126_meanx + 3*SLE126_sdx), rev(colMeans(SLE126_meanx - 3*SLE126_sdx))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 51), border = NA)

lines(years, colMeans(SLE119_meanx), col = rgb(30, 150, 132, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(colMeans(SLE119_meanx + 3*SLE119_sdx), rev(colMeans(SLE119_meanx - 3*SLE119_sdx))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 51), border = NA)

legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "mean+3sd_predictions.pdf")
}


## plot the median predictions with [5, 95]% intervals shaded, for each SSP
plot(0,0,xlim = c(1955,2300),ylim = c(-1,5),type = "n",xlab = "Year", ylab = paste("Sea level contribution relative to 2000 (m SLE)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 1995, lwd = 0.5)
abline(h = 0, lwd = 0.5)

lines(years, apply(distn585, 2, quantile, probs = 0.5), col = rgb(132, 11, 34, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(apply(distn585, 2, quantile, probs = 0.05), rev(apply(distn585, 2, quantile, probs = 0.95))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 51), border = NA)

lines(years, apply(distn370, 2, quantile, probs = 0.5), col = rgb(242, 17, 17, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(apply(distn370, 2, quantile, probs = 0.05), rev(apply(distn370, 2, quantile, probs = 0.95))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 51), border = NA)

lines(years, apply(distn245, 2, quantile, probs = 0.5), col = rgb(234, 221, 61, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(apply(distn245, 2, quantile, probs = 0.05), rev(apply(distn245, 2, quantile, probs = 0.95))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 51), border = NA)

lines(years, apply(distn126, 2, quantile, probs = 0.5), col = rgb(29, 51, 84, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(apply(distn126, 2, quantile, probs = 0.05), rev(apply(distn126, 2, quantile, probs = 0.95))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 51), border = NA)

lines(years, apply(distn119, 2, quantile, probs = 0.5), col = rgb(30, 150, 132, maxColorValue = 255), type = 'l', lwd = 2)
polygon(c(years, rev(years)), c(apply(distn119, 2, quantile, probs = 0.05), rev(apply(distn119, 2, quantile, probs = 0.95))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 51), border = NA)

legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "med_predictions.pdf")
}


## plot predictions against GSAT at 2300
plot(GSAT_585, SLE585_meanx[,70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlim = c(-1,17), xlab='GSAT change relative to 2015-2044', ylab = 'SLE relative to 2000', ylim=c(-2.2, 9), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5 )
points(GSAT_370, SLE370_meanx[,70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(GSAT_245, SLE245_meanx[,70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(GSAT_126, SLE126_meanx[,70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(GSAT_119, SLE119_meanx[,70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126']$GSAT_2300, SLE[SLE$scenario=='SSP126']$SLE_2300, pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585']$GSAT_2300, SLE[SLE$scenario=='SSP585']$SLE_2300, pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_GSAT_SLE_2300.pdf")
}



## plot predictions against lapse rate at 2300
plot(lapse_rate, SLE585_meanx[,70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Lapse rate (\u00B0C/km)', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 10), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(lapse_rate, SLE370_meanx[,70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(lapse_rate, SLE245_meanx[,70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(lapse_rate, SLE126_meanx[,70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(lapse_rate, SLE119_meanx[,70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126']$lapse_rate, ave[SLE$scenario=='SSP126',70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585']$lapse_rate, ave[SLE$scenario=='SSP585',70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_lapse_SLE_2300.pdf")
}


## plot predictions against heat flux, Burgard scheme at 2300
plot(heat_flux_Burgard[heat_flux_Burgard != 5*10**-4], SLE585_meanx[c(which(heat_flux_Burgard != 5*10**-4)),70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Heat flux, Burgard scheme', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(heat_flux_Burgard[heat_flux_Burgard != 5*10**-4], SLE370_meanx[c(which(heat_flux_Burgard != 5*10**-4)),70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(heat_flux_Burgard[heat_flux_Burgard != 5*10**-4], SLE245_meanx[c(which(heat_flux_Burgard != 5*10**-4)),70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(heat_flux_Burgard[heat_flux_Burgard != 5*10**-4], SLE126_meanx[c(which(heat_flux_Burgard != 5*10**-4)),70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(heat_flux_Burgard[heat_flux_Burgard != 5*10**-4], SLE119_meanx[c(which(heat_flux_Burgard != 5*10**-4)),70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126' & SLE$heat_flux_Burgard != 5*10**-4]$heat_flux_Burgard, ave[SLE$scenario=='SSP126'& SLE$heat_flux_Burgard != 5*10**-4,70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585'& SLE$heat_flux_Burgard != 5*10**-4]$heat_flux_Burgard, ave[SLE$scenario=='SSP585'& SLE$heat_flux_Burgard != 5*10**-4,70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_Burgard_SLE_2300.pdf")
}


## plot predictions against heat flux, ISMIP6 non-local scheme at 2300
plot(heat_flux_ISMIP6_nonlocal[heat_flux_ISMIP6_nonlocal != 1.45*10**4], SLE585_meanx[c(which(heat_flux_ISMIP6_nonlocal != 1.45*10**4)),70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Heat flux, ISMIP6 non-local scheme', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(heat_flux_ISMIP6_nonlocal[heat_flux_ISMIP6_nonlocal != 1.45*10**4], SLE370_meanx[c(which(heat_flux_ISMIP6_nonlocal != 1.45*10**4)),70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(heat_flux_ISMIP6_nonlocal[heat_flux_ISMIP6_nonlocal != 1.45*10**4], SLE245_meanx[c(which(heat_flux_ISMIP6_nonlocal != 1.45*10**4)),70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(heat_flux_ISMIP6_nonlocal[heat_flux_ISMIP6_nonlocal != 1.45*10**4], SLE126_meanx[c(which(heat_flux_ISMIP6_nonlocal != 1.45*10**4)),70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(heat_flux_ISMIP6_nonlocal[heat_flux_ISMIP6_nonlocal != 1.45*10**4], SLE119_meanx[c(which(heat_flux_ISMIP6_nonlocal != 1.45*10**4)),70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126' & SLE$heat_flux_ISMIP6_nonlocal != 1.45*10**4]$heat_flux_ISMIP6_nonlocal, ave[SLE$scenario=='SSP126'& SLE$heat_flux_ISMIP6_nonlocal != 1.45*10**4,70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585'& SLE$heat_flux_ISMIP6_nonlocal != 1.45*10**4]$heat_flux_ISMIP6_nonlocal, ave[SLE$scenario=='SSP585'& SLE$heat_flux_ISMIP6_nonlocal != 1.45*10**4,70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_ISMIP6_SLE_2300.pdf")
}



## plot predictions against heat flux, ISMIP6 non-local slope scheme at 2300
plot(heat_flux_ISMIP6_nonlocal_slope[heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6], SLE585_meanx[c(which(heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6)),70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Heat flux, ISMIP6 non-local slope scheme', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(heat_flux_ISMIP6_nonlocal_slope[heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6], SLE370_meanx[c(which(heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6)),70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(heat_flux_ISMIP6_nonlocal_slope[heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6], SLE245_meanx[c(which(heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6)),70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(heat_flux_ISMIP6_nonlocal_slope[heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6], SLE126_meanx[c(which(heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6)),70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(heat_flux_ISMIP6_nonlocal_slope[heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6], SLE119_meanx[c(which(heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6)),70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126' & SLE$heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6]$heat_flux_ISMIP6_nonlocal_slope, ave[SLE$scenario=='SSP126'& SLE$heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6,70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585'& SLE$heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6]$heat_flux_ISMIP6_nonlocal_slope, ave[SLE$scenario=='SSP585'& SLE$heat_flux_ISMIP6_nonlocal_slope != 2.06*10**6,70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_ISMIP6slope_SLE_2300.pdf")
}


## plot predictions against heat flux, PICO scheme at 2300
plot(heat_flux_PICO[(heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5)], SLE585_meanx[c(which(heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5)),70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Heat flux, PICO scheme', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9.2), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(heat_flux_PICO[heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5], SLE370_meanx[c(which(heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5)),70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(heat_flux_PICO[heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5], SLE245_meanx[c(which(heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5)),70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(heat_flux_PICO[heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5], SLE126_meanx[c(which(heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5)),70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(heat_flux_PICO[heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5], SLE119_meanx[c(which(heat_flux_PICO != 4*10**-5 & heat_flux_PICO != 7*10**-5)),70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126' & (SLE$heat_flux_PICO != 4*10**-5 & SLE$heat_flux_PICO != 7*10**-5)]$heat_flux_PICO, ave[SLE$scenario=='SSP126'& (SLE$heat_flux_PICO != 4*10**-5 & SLE$heat_flux_PICO != 7*10**-5),70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585'& (SLE$heat_flux_PICO != 4*10**-5 & SLE$heat_flux_PICO != 7*10**-5)]$heat_flux_PICO, ave[SLE$scenario=='SSP585'& (SLE$heat_flux_PICO != 4*10**-5 & SLE$heat_flux_PICO != 7*10**-5),70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_PICO_SLE_2300.pdf")
}


## plot predictions against heat flux, Plume scheme at 2300
plot(heat_flux_Plume[heat_flux_Plume != 5.9*10**-4], SLE585_meanx[c(which(heat_flux_Plume != 5.9*10**-4)),70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Heat flux, Plume scheme', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9.2), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(heat_flux_Plume[heat_flux_Plume != 5.9*10**-4], SLE370_meanx[c(which(heat_flux_Plume != 5.9*10**-4)),70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(heat_flux_Plume[heat_flux_Plume != 5.9*10**-4], SLE245_meanx[c(which(heat_flux_Plume != 5.9*10**-4)),70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(heat_flux_Plume[heat_flux_Plume != 5.9*10**-4], SLE126_meanx[c(which(heat_flux_Plume != 5.9*10**-4)),70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(heat_flux_Plume[heat_flux_Plume != 5.9*10**-4], SLE119_meanx[c(which(heat_flux_Plume != 5.9*10**-4)),70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126' & SLE$heat_flux_Plume != 5.9*10**-4]$heat_flux_Plume, ave[SLE$scenario=='SSP126'& SLE$heat_flux_Plume != 5.9*10**-4,70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585'& SLE$heat_flux_Plume != 5.9*10**-4]$heat_flux_Plume, ave[SLE$scenario=='SSP585'& SLE$heat_flux_Plume != 5.9*10**-4,70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_Plume_SLE_2300.pdf")
}


## plot predictions against refreezing parameter at 2300
plot(refreeze, SLE585_meanx[,70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Refreezing parameter (m)', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(refreeze, SLE370_meanx[,70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(refreeze, SLE245_meanx[,70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(refreeze, SLE126_meanx[,70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(refreeze, SLE119_meanx[,70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126' & SLE$refreeze != 5]$refreeze, ave[SLE$scenario=='SSP126'& SLE$refreeze != 5,70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585' & SLE$refreeze != 5]$refreeze, ave[SLE$scenario=='SSP585'& SLE$refreeze != 5,70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_refreeze_SLE_2300.pdf")
}


## plot predictions against refreezing fraction parameter at 2300
plot(refreeze_frac, SLE585_meanx[,70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Refreezing fraction', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9.2), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(refreeze_frac, SLE370_meanx[,70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(refreeze_frac, SLE245_meanx[,70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(refreeze_frac, SLE126_meanx[,70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(refreeze_frac, SLE119_meanx[,70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126' & SLE$refreeze_frac != 0.6]$refreeze_frac, ave[SLE$scenario=='SSP126'& SLE$refreeze_frac != 0.6,70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585' & SLE$refreeze_frac != 0.6]$refreeze_frac, ave[SLE$scenario=='SSP585'& SLE$refreeze_frac != 0.6,70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_refreezefrac_SLE_2300.pdf")
}


## plot predictions against ice melt parameter, positive degree day scheme at 2300
plot(PDD_ice, SLE585_meanx[,70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Ice melt parameter', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9.2), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(PDD_ice, SLE370_meanx[,70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(PDD_ice, SLE245_meanx[,70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(PDD_ice, SLE126_meanx[,70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(PDD_ice, SLE119_meanx[,70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126']$PDD_ice, ave[SLE$scenario=='SSP126',70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585']$PDD_ice, ave[SLE$scenario=='SSP585',70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_PDDice_SLE_2300.pdf")
}


## plot predictions against snow melt parameter, positive degree day scheme at 2300
plot(PDD_snow, SLE585_meanx[,70], col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), xlab='Snow melt parameter', ylab = 'SLE relative to 2000 (m)', ylim=c(-2.2, 9.2), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
points(PDD_snow, SLE370_meanx[,70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153))
points(PDD_snow, SLE245_meanx[,70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153))
points(PDD_snow, SLE126_meanx[,70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(PDD_snow, SLE119_meanx[,70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP126']$PDD_snow, ave[SLE$scenario=='SSP126',70], pch=18, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153))
points(SLE[SLE$scenario=='SSP585']$PDD_snow, ave[SLE$scenario=='SSP585',70], pch=18, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153))
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_PDDsnow_SLE_2300.pdf")
}


## plot predictions against choice of basal melt parameterisation scheme at 2300
plot(melt_param, SLE119_meanx[,70], at = 0:4*7, col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153), xlab='Melt parameterisation', ylab = 'SLE relative to 2000 (m)', xlim = c(0, 33.5), ylim=c(-2.2, 9.2), yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.lab = 1.5)
plot(melt_param, SLE126_meanx[,70], at = 0:4*7 + 1, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.lab = 1.5)
plot(melt_param, SLE245_meanx[,70], at = 0:4*7 + 2, col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153), add = TRUE, cex = 1.1, cex.main = 1.5, cex.lab = 1.5)
plot(melt_param, SLE370_meanx[,70], at = 0:4*7 + 3, col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.lab = 1.5)
plot(melt_param, SLE585_meanx[,70], at = 0:4*7 + 4, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.lab = 1.5)
plot(SLE[SLE$scenario=='SSP126']$melt_param, ave[SLE$scenario=='SSP126',70], at = 0:4*7 + 5, lwd=2, outpch=18, border = rgb(29, 51, 84, maxColorValue = 255), col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.lab = 1.5)
plot(SLE[SLE$scenario=='SSP585']$melt_param, ave[SLE$scenario=='SSP585',70], at = 0:4*7 + 6, lwd=2, outpch=18, border = rgb(132, 11, 34, maxColorValue = 255), col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.lab = 1.5)
abline(v = 6.5, lty=2)
abline(v = 13.5, lty=2)
abline(v = 20.5, lty=2)
abline(v = 27.5, lty=2)
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_meltparam_SLE_2300.pdf")
}


## plot predictions against choice of initial atmosphere forcing at 2300
plot(init_atmos, SLE119_meanx[,70], at = 0:1*7, col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153), xlab='Initial atmosphere forcing', ylab = 'SLE relative to 2000 (m)', xlim = c(-0.1, 13.1), ylim=c(-2.2, 9.2), yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(init_atmos, SLE126_meanx[,70], at = 0:1*7 + 1, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(init_atmos, SLE245_meanx[,70], at = 0:1*7 + 2, col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153), add = TRUE, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(init_atmos, SLE370_meanx[,70], at = 0:1*7 + 3, col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(init_atmos, SLE585_meanx[,70], at = 0:1*7 + 4, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SLE[SLE$scenario=='SSP126']$init_atmos, ave[SLE$scenario=='SSP126',70], at = 0:1*7 + 5, lwd=2, outpch=18, border = rgb(29, 51, 84, maxColorValue = 255), col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SLE[SLE$scenario=='SSP585']$init_atmos, ave[SLE$scenario=='SSP585',70], at = 0:1*7 + 6, lwd=2, outpch=18, border = rgb(132, 11, 34, maxColorValue = 255), col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 6.5, lty=2)
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_atmos_SLE_2300.pdf")
}


## plot predictions against simoc, the combined simulator and ocean forcing parameter at 2300
plot(simoc, SLE119_meanx[,70], at = 0:2*7, col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153), xlab='Ice sheet model', ylab = 'SLE relative to 2000 (m)', xlim = c(-0.1, 20.1), ylim=c(-2.2, 9.2), yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(simoc, SLE126_meanx[,70], at = 0:2*7 + 1, col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(simoc, SLE245_meanx[,70], at = 0:2*7 + 2, col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153), add = TRUE, cex = 1.1, cex.main = 1.5, cex.axis = 1.3, cex.lab = 1.5)
plot(simoc, SLE370_meanx[,70], at = 0:2*7 + 3, col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(simoc, SLE585_meanx[,70], at = 0:2*7 + 4, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SLE[SLE$scenario=='SSP126']$simoc, ave[SLE$scenario=='SSP126',70], at = 0:2*7 + 5, lwd=2, outpch=18, border = rgb(29, 51, 84, maxColorValue = 255), col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
plot(SLE[SLE$scenario=='SSP585']$simoc, ave[SLE$scenario=='SSP585',70], at = 0:2*7 + 6, lwd=2, outpch=18, border = rgb(132, 11, 34, maxColorValue = 255), col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), add = TRUE, yaxt='n', xaxt='n', cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 6.5, lty=2)
abline(v = 13.5, lty=2)
legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pred_simoc_SLE_2300.pdf")
}


## plot pdfs of SSPs at 2300, with [5, 95]% intervals
plot(density(distn585[,70]), col=rgb(132, 11, 34, maxColorValue = 255), xlab = "SLE at 2300 relative to 2000 (m)", main = " ", ylim=c(0,0.35), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(distn370[,70]), col=rgb(242, 17, 17, maxColorValue = 255), lwd=2)
lines(density(distn245[,70]), col=rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
lines(density(distn126[,70]), col=rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
lines(density(distn119[,70]), col=rgb(30, 150, 132, maxColorValue = 255), lwd = 2)

abline(v = quantile(distn585[,70], probs = 0.95), col=rgb(132, 11, 34, maxColorValue = 255), lty = 2)
text(quantile(distn585[,70], probs = 0.95)+0.15, 0.35, round(quantile(distn585[,70], probs = 0.95),2), col=rgb(132, 11, 34, maxColorValue = 255))
abline(v = quantile(distn585[,70], probs = 0.05), col=rgb(132, 11, 34, maxColorValue = 255), lty = 2)
text(quantile(distn585[,70], probs = 0.05)+0.15, 0.35, round(quantile(distn585[,70], probs = 0.05),2), col=rgb(132, 11, 34, maxColorValue = 255))

abline(v = quantile(distn370[,70], probs = 0.95), col=rgb(242, 17, 17, maxColorValue = 255), lty = 2)
text(quantile(distn370[,70], probs = 0.95)+0.15, 0.335, round(quantile(distn370[,70], probs = 0.95),2), col=rgb(242, 17, 17, maxColorValue = 255))
abline(v = quantile(distn370[,70], probs = 0.05), col=rgb(242, 17, 17, maxColorValue = 255), lty = 2)
text(quantile(distn370[,70], probs = 0.05)+0.15, 0.335, round(quantile(distn370[,70], probs = 0.05),2), col=rgb(242, 17, 17, maxColorValue = 255))

abline(v = quantile(distn245[,70], probs = 0.95), col=rgb(234, 221, 61, maxColorValue = 255), lty = 2)
text(quantile(distn245[,70], probs = 0.95)+0.15, 0.35, round(quantile(distn245[,70], probs = 0.95),2), col=rgb(234, 221, 61, maxColorValue = 255))
abline(v = quantile(distn245[,70], probs = 0.05), col=rgb(234, 221, 61, maxColorValue = 255), lty = 2)
text(quantile(distn245[,70], probs = 0.05)+0.15, 0.35, round(quantile(distn245[,70], probs = 0.05),2), col=rgb(234, 221, 61, maxColorValue = 255))

abline(v = quantile(distn126[,70], probs = 0.95), col=rgb(29, 51, 84, maxColorValue = 255), lty = 2)
text(quantile(distn126[,70], probs = 0.95)+0.15, 0.335, round(quantile(distn126[,70], probs = 0.95),2), col=rgb(29, 51, 84, maxColorValue = 255))
abline(v = quantile(distn126[,70], probs = 0.05), col=rgb(29, 51, 84, maxColorValue = 255), lty = 2)
text(quantile(distn126[,70], probs = 0.05)+0.15, 0.335, round(quantile(distn126[,70], probs = 0.05),2), col=rgb(29, 51, 84, maxColorValue = 255))

abline(v = quantile(distn119[,70], probs = 0.95), col=rgb(30, 150, 132, maxColorValue = 255), lty = 2)
text(quantile(distn119[,70], probs = 0.95)+0.15, 0.35, round(quantile(distn119[,70], probs = 0.95),2), col=rgb(30, 150, 132, maxColorValue = 255))
abline(v = quantile(distn119[,70], probs = 0.05), col=rgb(30, 150, 132, maxColorValue = 255), lty = 2)
text(quantile(distn119[,70], probs = 0.05)+0.15, 0.35, round(quantile(distn119[,70], probs = 0.05),2), col=rgb(30, 150, 132, maxColorValue = 255))

legend("topright", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "pdfs_SLE_2300.pdf")
}


## histograms of SSPs at 2300
hist(SLE585_meanx[,70], breaks = 20, col=rgb(132, 11, 34, maxColorValue = 255, alpha = 153), ylim=c(0, 900), xlab = "SLE relative to 2000 (m)", main = " ", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
hist(SLE370_meanx[,70], col=rgb(242, 17, 17, maxColorValue = 255, alpha = 153), xlab = "SLE relative to 2000 (m)", main = " ", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, add = T)
hist(SLE245_meanx[,70], col=rgb(234, 221, 61, maxColorValue = 255, alpha = 153), xlab = "SLE relative to 2000 (m)", main = " ", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, add = T)
hist(SLE126_meanx[,70], col=rgb(29, 51, 84, maxColorValue = 255, alpha = 153), xlab = "SLE relative to 2000 (m)", main = " ", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, add = T)
hist(SLE119_meanx[,70], col=rgb(30, 150, 132, maxColorValue = 255, alpha = 153), xlab = "SLE relative to 2000 (m)", main = " ", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, add = T)
legend("topright", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1)
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "hists_2300.pdf")
}

