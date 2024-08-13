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

plot(density(mh$lapse_rate), col='red', xlab = "Lapse rate parameter", ylim = c(0,0.4), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(-12, 0, -12, 0.3, lwd = 2)
segments(-5, 0, -5, 0.3, lwd = 2)
segments(-12, 0.3, -5, 0.3, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_lapserate.pdf") 

plot(density(mh$refreeze), col='red', xlab = "Refreeze parameter", ylim = c(0,0.3), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(0, 0, 0, 0.2, lwd = 2)
segments(15, 0, 15, 0.2, lwd = 2)
segments(0, 0.2, 15, 0.2, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_refreeze.pdf") 

plot(density(mh$refreeze_frac), col='red', xlab = "Refreeze fraction", main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(0.2, 0, 0.2, 2, lwd = 2)
segments(0.8, 0, 0.8, 2, lwd = 2)
segments(0.2, 2, 0.8, 2, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_refreeze_frac.pdf") 

plot(density(mh$PDD_ice), col='red', xlab = "PDD ice", ylim = c(0, 0.4), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(4, 0, 4, 0.25, lwd = 2)
segments(12, 0, 12, 0.25, lwd = 2)
segments(4, 0.25, 12, 0.25, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_PDD_ice.pdf") 

plot(density(mh$PDD_snow), col='red', xlab = "PDD snow", ylim = c(0, 0.45), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(0, 0, 0, 0.3, lwd = 2)
segments(6, 0, 6, 0.3, lwd = 2)
segments(0, 0.3, 6, 0.3, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_PDD_snow.pdf")

plot(density(mh$heat_flux_PICO), col='red', xlab = "Heat flux, PICO", ylim = c(0, 40000), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(0.1*10**-5, 0, 0.1*10**-5, 3*10**4, lwd = 2)
segments(10*10**-5, 0, 10*10**-5, 3*10**4, lwd = 2)
segments(0.1*10**-5, 3*10**4, 10*10**-5, 3*10**4, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_PICO.pdf")

plot(density(mh$heat_flux_Plume), col='red', xlab = "Heat flux, Plume", xlim=c(1*10**-4, 10*10**-4), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(1*10**-4, 0, 1*10**-4, 2.5*10**3, lwd = 2)
segments(10*10**-4, 0, 10*10**-4, 2.5*10**3, lwd = 2)
segments(1*10**-4, 2.5*10**3, 10*10**-4, 2.5*10**3, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_Plume.pdf")

plot(density(mh$heat_flux_Burgard), col='red', xlab = "Heat flux, Burgard", xlim=c(1*10**-4, 10*10**-4), ylim=c(0, 4000), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(1*10**-4, 0, 1*10**-4, 3*10**3, lwd = 2)
segments(10*10**-4, 0, 10*10**-4, 3*10**3, lwd = 2)
segments(1*10**-4, 3*10**3, 10*10**-4, 3*10**3, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_Burgard.pdf")

plot(density(mh$heat_flux_ISMIP6_nonlocal), col='red', xlab = "Heat flux, ISMIP6", xlim=c(1*10**4, 4*10**4), ylim=c(0, 0.00008), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(1*10**4, 0, 1*10**4, 5*10**-5, lwd = 2)
segments(4*10**4, 0, 4*10**4, 5*10**-5, lwd = 2)
segments(1*10**4, 5*10**-5, 4*10**4, 5*10**-5, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_ISMIP6.pdf")

plot(density(mh$heat_flux_ISMIP6_nonlocal_slope), col='red', xlab = "Heat flux, ISMIP6 slope", xlim=c(1*10**6, 4*10**6), ylim=c(0, 0.000001), main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
segments(1*10**6, 0, 1*10**6, 6*10**-7, lwd = 2)
segments(4*10**6, 0, 4*10**6, 6*10**-7, lwd = 2)
segments(1*10**6, 6*10**-7, 4*10**6, 6*10**-7, lwd = 2)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_heat_flux_ISMIP6slope.pdf")

barplot(table(mh$simoc), width = 0.1, ylab = "Frequency", xlab = "Simulator/init ocean", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_simoc.pdf")

barplot(table(mh$init_atmos), ylab = "Frequency", xlab = "Init atmos", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_initatmos.pdf")

barplot(table(mh_119$melt_param), ylab = "Frequency", xlab = "Melt param", cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_meltparam.pdf")



