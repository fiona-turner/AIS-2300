fpath <- "~/Documents/Emulators/qemu/Distributions"
param_119 <- fread(file.path(fpath, "mh_output.csv"))
param_126 <- fread(file.path(fpath, "mh_output.csv"))
param_245 <- fread(file.path(fpath, "mh_output.csv"))
param_370 <- fread(file.path(fpath, "mh_output.csv"))
param_585 <- fread(file.path(fpath, "mh_output.csv"))

param_119$GSAT_2300 <- sample(FORpred[scenario == 'SSP119']$GSAT_2300, dim(param_119)[1], replace = TRUE)
param_126$GSAT_2300 <- sample(FORpred[scenario == 'SSP126']$GSAT_2300, dim(param_126)[1], replace = TRUE)
param_245$GSAT_2300 <- sample(FORpred[scenario == 'SSP245']$GSAT_2300, dim(param_245)[1], replace = TRUE)
param_370$GSAT_2300 <- sample(FORpred[scenario == 'SSP370']$GSAT_2300, dim(param_370)[1], replace = TRUE)
param_585$GSAT_2300 <- sample(FORpred[scenario == 'SSP585']$GSAT_2300, dim(param_585)[1], replace = TRUE)

mh119_mom <- lapply(1L:r, function(j) {predict(emu[[j]], param_119, type = "moments")}) 
mh119_mean <- matrix( unlist(lapply(mh119_mom, function(j) j[c('mean')])), ncol=r)
mh119_sd <- matrix( unlist(lapply(mh119_mom, function(j) j[c('sd')])), ncol=r)

mh119_meanx <- sweep(mh119_mean %*% Vt, 2L, cc, "+")
mh119_sdx <- t(sapply(1L:nrow(mh119_sd), function(i) {
  sqrt(colSums((mh119_sd[i, ] * Vt)^2)) # n vector
}))
mh119_varx <- lapply(1L:nrow(mh119_sd), function(i) {
  as.vector(crossprod(mh119_sd[i, ] * Vt)) 
})
mh119_varx <- do.call("cbind", mh119_varx) 
dim(mh119_varx) <- c(n, n, nrow(mh119_sd))
mh119_varx <- aperm(mh119_varx, c(3, 1, 2))


mh126_mom <- lapply(1L:r, function(j) {predict(emu[[j]], param_126, type = "moments")}) 
mh126_mean <- matrix( unlist(lapply(mh126_mom, function(j) j[c('mean')])), ncol=r)
mh126_sd <- matrix( unlist(lapply(mh126_mom, function(j) j[c('sd')])), ncol=r)

mh126_meanx <- sweep(mh126_mean %*% Vt, 2L, cc, "+")
mh126_sdx <- t(sapply(1L:nrow(mh126_sd), function(i) {
  sqrt(colSums((mh126_sd[i, ] * Vt)^2)) # n vector
}))
mh126_varx <- lapply(1L:nrow(mh126_sd), function(i) {
  as.vector(crossprod(mh126_sd[i, ] * Vt)) 
})
mh126_varx <- do.call("cbind", mh126_varx) 
dim(mh126_varx) <- c(n, n, nrow(mh126_sd))
mh126_varx <- aperm(mh126_varx, c(3, 1, 2))

mh245_mom <- lapply(1L:r, function(j) {predict(emu[[j]], param_245, type = "moments")}) 
mh245_mean <- matrix( unlist(lapply(mh245_mom, function(j) j[c('mean')])), ncol=r)
mh245_sd <- matrix( unlist(lapply(mh245_mom, function(j) j[c('sd')])), ncol=r)

mh245_meanx <- sweep(mh245_mean %*% Vt, 2L, cc, "+")
mh245_sdx <- t(sapply(1L:nrow(mh245_sd), function(i) {
  sqrt(colSums((mh245_sd[i, ] * Vt)^2)) # n vector
}))
mh245_varx <- lapply(1L:nrow(mh245_sd), function(i) {
  as.vector(crossprod(mh245_sd[i, ] * Vt)) 
})
mh245_varx <- do.call("cbind", mh245_varx) 
dim(mh245_varx) <- c(n, n, nrow(mh245_sd))
mh245_varx <- aperm(mh245_varx, c(3, 1, 2))

mh370_mom <- lapply(1L:r, function(j) {predict(emu[[j]], param_370, type = "moments")}) 
mh370_mean <- matrix( unlist(lapply(mh370_mom, function(j) j[c('mean')])), ncol=r)
mh370_sd <- matrix( unlist(lapply(mh370_mom, function(j) j[c('sd')])), ncol=r)

mh370_meanx <- sweep(mh370_mean %*% Vt, 2L, cc, "+")
mh370_sdx <- t(sapply(1L:nrow(mh370_sd), function(i) {
  sqrt(colSums((mh370_sd[i, ] * Vt)^2)) # n vector
}))
mh370_varx <- lapply(1L:nrow(mh370_sd), function(i) {
  as.vector(crossprod(mh370_sd[i, ] * Vt)) 
})
mh370_varx <- do.call("cbind", mh370_varx) 
dim(mh370_varx) <- c(n, n, nrow(mh370_sd))
mh370_varx <- aperm(mh370_varx, c(3, 1, 2))



mh585_mom <- lapply(1L:r, function(j) {predict(emu[[j]], param_585, type = "moments")}) 
mh585_mean <- matrix( unlist(lapply(mh585_mom, function(j) j[c('mean')])), ncol=r)
mh585_sd <- matrix( unlist(lapply(mh585_mom, function(j) j[c('sd')])), ncol=r)

mh585_meanx <- sweep(mh585_mean %*% Vt, 2L, cc, "+")
mh585_sdx <- t(sapply(1L:nrow(mh585_sd), function(i) {
  sqrt(colSums((mh585_sd[i, ] * Vt)^2)) # n vector
}))
mh585_varx <- lapply(1L:nrow(mh585_sd), function(i) {
  as.vector(crossprod(mh585_sd[i, ] * Vt)) 
})
mh585_varx <- do.call("cbind", mh585_varx) 
dim(mh585_varx) <- c(n, n, nrow(mh585_sd))
mh585_varx <- aperm(mh585_varx, c(3, 1, 2))


mh_distn119 <- matrix(rep(0, dim(param_119)[1]*dim(ave)[2]), nrow = dim(param_119)[1], ncol = dim(ave)[2])
mh_distn126 <- matrix(rep(0, dim(param_119)[1]*dim(ave)[2]), nrow = dim(param_119)[1], ncol = dim(ave)[2])
mh_distn245 <- matrix(rep(0, dim(param_119)[1]*dim(ave)[2]), nrow = dim(param_119)[1], ncol = dim(ave)[2])
mh_distn370 <- matrix(rep(0, dim(param_119)[1]*dim(ave)[2]), nrow = dim(param_119)[1], ncol = dim(ave)[2])
mh_distn585 <- matrix(rep(0, dim(param_119)[1]*dim(ave)[2]), nrow = dim(param_119)[1], ncol = dim(ave)[2])

for(i in 1L:dim(mh119_meanx)[1]){
  mh_distn119[i,] <- mvrnorm(1, mh119_meanx[i,], mh119_varx[i,,])  
}
for(i in 1L:dim(mh126_meanx)[1]){
  mh_distn126[i,] <- mvrnorm(1, mh126_meanx[i,], mh126_varx[i,,])  
}
for(i in 1L:dim(mh245_meanx)[1]){
  mh_distn245[i,] <- mvrnorm(1, mh245_meanx[i,], mh245_varx[i,,])  
}
for(i in 1L:dim(mh370_meanx)[1]){
  mh_distn370[i,] <- mvrnorm(1, mh370_meanx[i,], mh370_varx[i,,])  
}
for(i in 1L:dim(mh585_meanx)[1]){
  mh_distn585[i,] <- mvrnorm(1, mh585_meanx[i,], mh585_varx[i,,])  
}

colnames(mh_distn119) <- paste("y", years, sep = "")
colnames(mh_distn126) <- paste("y", years, sep = "")
colnames(mh_distn245) <- paste("y", years, sep = "")
colnames(mh_distn370) <- paste("y", years, sep = "")
colnames(mh_distn585) <- paste("y", years, sep = "")

write.csv(mh_distn119, "Distributions/mh_distn119.csv", row.names = FALSE)
write.csv(mh_distn126, "Distributions/mh_distn126.csv", row.names = FALSE)
write.csv(mh_distn245, "Distributions/mh_distn245.csv", row.names = FALSE)
write.csv(mh_distn370, "Distributions/mh_distn370.csv", row.names = FALSE)
write.csv(mh_distn585, "Distributions/mh_distn585.csv", row.names = FALSE)

round(quantile(mh_distn119[,70], c(0.05, 0.167, 0.5, 0.833, 0.95)),2)
round(quantile(mh_distn126[,70], c(0.05, 0.167, 0.5, 0.833, 0.95)),2)
round(quantile(mh_distn245[,70], c(0.05, 0.167, 0.5, 0.833, 0.95)),2)
round(quantile(mh_distn370[,70], c(0.05, 0.167, 0.5, 0.833, 0.95)),2)
round(quantile(mh_distn585[,70], c(0.05, 0.167, 0.5, 0.833, 0.95)),2)


mh_quant119 <- apply(mh_distn119, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
mh_quant126 <- apply(mh_distn126, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
mh_quant245 <- apply(mh_distn245, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
mh_quant370 <- apply(mh_distn370, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
mh_quant585 <- apply(mh_distn585, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))

colnames(mh_quant119) <- paste("y", years, sep = "")
colnames(mh_quant126) <- paste("y", years, sep = "")
colnames(mh_quant245) <- paste("y", years, sep = "")
colnames(mh_quant370) <- paste("y", years, sep = "")
colnames(mh_quant585) <- paste("y", years, sep = "")

if (save_pred){
  write.csv(mh_quant119, "Distributions/mh_quant119.csv", row.names = TRUE)
  write.csv(mh_quant126, "Distributions/mh_quant126.csv", row.names = TRUE)
  write.csv(mh_quant245, "Distributions/mh_quant245.csv", row.names = TRUE)
  write.csv(mh_quant370, "Distributions/mh_quant370.csv", row.names = TRUE)
  write.csv(mh_quant585, "Distributions/mh_quant585.csv", row.names = TRUE)
}


layout_mat <- matrix(c(1:3), nrow = 1, ncol = 3,
                     byrow = TRUE)

my_lay <- layout(mat = layout_mat, 
                 heights = c(1, 1, 1),
                 widths = c(3, 0.5, 0.5), respect =TRUE)

par(mar = c(5.1, 4.1, 0.1, 0.1))
plot(0,0,xlim = c(1955,2300),ylim = c(-2,5),type = "n", xlab = "Year", ylab = paste("SLE (m)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 2000, lwd = 0.5)
abline(h = 0, lwd = 0.5)


lines(years, apply(mh_distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(mh_distn585, 2, function(x) quantile(x,0.05)), rev(apply(mh_distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(mh_distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(mh_distn370, 2, function(x) quantile(x,0.05)), rev(apply(mh_distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(mh_distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(mh_distn245, 2, function(x) quantile(x,0.05)), rev(apply(mh_distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(mh_distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(mh_distn126, 2, function(x) quantile(x,0.05)), rev(apply(mh_distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(mh_distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(mh_distn119, 2, function(x) quantile(x,0.05)), rev(apply(mh_distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 51), border = NA)

legend(1925, 5.8, legend=as.expression(bquote(bold("a"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


legend("topleft", legend=c(as.expression(bquote(bold("SSP1-1.9"))), as.expression(bquote(bold("SSP1-2.6"))), as.expression(bquote(bold("SSP2-4.5"))), as.expression(bquote(bold("SSP3-7.0"))), as.expression(bquote(bold("SSP5-8.5")))),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1, bty = "n")

par(mar = c(5.1, 0.1, 0.1, 0.1))

boxplot(cbind(mh_quant119[c(2,3,5,7,8),70],mh_quant126[c(2,3,5,7,8),70],mh_quant245[c(2,3,5,7,8),70],mh_quant370[c(2,3,5,7,8),70],mh_quant585[c(2,3,5,7,8),70]), frame = FALSE, axes = FALSE,  col=c(rgb(30, 150, 132, maxColorValue = 255, alpha = 153),rgb(29, 51, 84, maxColorValue = 255),rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)),ylim = c(-2,5))

legend(-1, 5.8, legend=as.expression(bquote(bold("b"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


par(mar = c(5.1, 0.1, 0.1, 0.1))
plot(density(mh_distn585[,70]), col=rgb(132, 11, 34, maxColorValue = 255), xlab = "SLE (m)", main = " ", ylim=c(0,0.35), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, frame = FALSE, yaxt='n')
lines(density(mh_distn370[,70]), col=rgb(242, 17, 17, maxColorValue = 255), lwd=2)
lines(density(mh_distn245[,70]), col=rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
lines(density(mh_distn126[,70]), col=rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
lines(density(mh_distn119[,70]), col=rgb(30, 150, 132, maxColorValue = 255), lwd = 2)

legend(-5, 0.39, legend=as.expression(bquote(bold("c"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")

dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/mh_projections.pdf") 
