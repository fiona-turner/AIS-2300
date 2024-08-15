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

dev.off()
plot(density(lapse_rate_samp), xlab = "Lapse rate parameter", main = " ", ylim=c(0, max(density(mh$lapse_rate)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$lapse_rate),  col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_lapserate.pdf") 

plot(density(refreeze_samp), xlab = "Refreeze parameter", main = " ", ylim=c(0, max(density(mh$refreeze)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$refreeze), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_refreeze.pdf") 

plot(density(refreeze_frac_samp), xlab = "Refreeze fraction", main = " ", ylim=c(0, max(density(mh$refreeze_frac)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$refreeze_frac), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_refreeze_frac.pdf") 

plot(density(PDD_ice_samp), xlab = "PDD ice", main = " ", ylim=c(0, max(density(mh$PDD_ice)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$PDD_ice), col='red',  lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_PDD_ice.pdf") 

plot(density(PDD_snow_samp), xlab = "PDD snow", main = " ", ylim=c(0, max(density(mh$PDD_snow)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$PDD_snow), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_PDD_snow.pdf")

plot(density(heat_flux_PICO_samp), xlab = "Heat flux, PICO", main = " ", ylim=c(0, max(density(mh$heat_flux_PICO)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$heat_flux_PICO), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_PICO.pdf")

plot(density(heat_flux_Plume_samp), xlab = "Heat flux, Plume", main = " ", ylim=c(0, max(density(mh$heat_flux_Plume)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$heat_flux_Plume), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_Plume.pdf")

plot(density(heat_flux_Burgard_samp), xlab = "Heat flux, Burgard", main = " ", ylim=c(0, max(density(mh$heat_flux_Burgard)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$heat_flux_Burgard), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_Burgard.pdf")

plot(density(heat_flux_ISMIP6_nonlocal_samp), xlab = "Heat flux, ISMIP6", main = " ", ylim=c(0, max(density(mh$heat_flux_ISMIP6_nonlocal)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$heat_flux_ISMIP6_nonlocal), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_ISMIP6.pdf")

plot(density(heat_flux_ISMIP6_nonlocal_slope_samp), xlab = "Heat flux, ISMIP6 slope", main = " ", ylim=c(0, max(density(mh$heat_flux_ISMIP6_nonlocal_slope)$y)), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(mh$heat_flux_ISMIP6_nonlocal_slope), col='red', lwd = 2)
legend("topleft", legend=c(as.expression(bquote(bold("Prior"))), as.expression(bquote(bold("Posterior")))),
       text.col=c('black', 'red'), cex=1.5, bty = "n")
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_ISMIP6slope.pdf")

barplot(table(mh$simoc), width = 0.1, ylab = "Frequency", xlab = "Simulator/init ocean", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_simoc.pdf")

barplot(table(mh$init_atmos), ylab = "Frequency", xlab = "Init atmos", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_initatmos.pdf")

barplot(table(mh$melt_param), ylab = "Frequency", xlab = "Melt param", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_meltparam.pdf")

