

mod_119 <- matrix(0, nrow = length(obs), ncol = nrow(SLE119_meanx))  # initialize mod matrix
mod_126 <- matrix(0, nrow = length(obs), ncol = nrow(SLE126_meanx))
mod_245 <- matrix(0, nrow = length(obs), ncol = nrow(SLE245_meanx))
mod_370 <- matrix(0, nrow = length(obs), ncol = nrow(SLE370_meanx))
mod_585 <- matrix(0, nrow = length(obs), ncol = nrow(SLE585_meanx))

for (ii in 1:length(obs)){
  mod_119[ii, ] <- distn119[,which(years==post[ii])]
  mod_126[ii, ] <- distn126[,which(years==post[ii])]
  mod_245[ii, ] <- distn245[,which(years==post[ii])]
  mod_370[ii, ] <- distn370[,which(years==post[ii])]
  mod_585[ii, ] <- distn585[,which(years==post[ii])]
}


sig_obs <- sig  # observational error
fac <- 15 # define factor for structural error
sig_mod <- fac * sig_obs  # structural error
var <- sig_obs^2 + sig_mod^2  # discrepancy variance

s_119 <- rep(0, nrow(distn119))  # initialize score vector
s_126 <- rep(0, nrow(distn126))
s_245 <- rep(0, nrow(distn245))
s_370 <- rep(0, nrow(distn370))
s_585 <- rep(0, nrow(distn585))

for (i in 1:nrow(distn119)) {
  s_119[i] <- exp(-0.5 * sum(((mod_119[, i] - obs)^2) / var))
  s_126[i] <- exp(-0.5 * sum(((mod_126[, i] - obs)^2) / var))
  s_245[i] <- exp(-0.5 * sum(((mod_245[, i] - obs)^2) / var))
  s_370[i] <- exp(-0.5 * sum(((mod_370[, i] - obs)^2) / var))
  s_585[i] <- exp(-0.5 * sum(((mod_585[, i] - obs)^2) / var))
}

w_119 <- s_119 / sum(s_119)  # normalize scores to get weights

w_126 <- s_126 / sum(s_126)

w_245 <- s_245 / sum(s_245)

w_370 <- s_370 / sum(s_370)

w_585 <- s_585 / sum(s_585)

## sample from the weighted predictions
weighted_distn585 <- matrix(rep(0, dim(distn585)[1]*dim(distn585)[2]), nrow = dim(distn585)[1])
weighted_distn370 <- matrix(rep(0, dim(distn585)[1]*dim(distn585)[2]), nrow = dim(distn585)[1])
weighted_distn245 <- matrix(rep(0, dim(distn585)[1]*dim(distn585)[2]), nrow = dim(distn585)[1])
weighted_distn126 <- matrix(rep(0, dim(distn585)[1]*dim(distn585)[2]), nrow = dim(distn585)[1])
weighted_distn119 <- matrix(rep(0, dim(distn585)[1]*dim(distn585)[2]), nrow = dim(distn585)[1])


for( i in 1L:dim(weighted_distn585)[2]){
  weighted_distn585[,i] <- sample(distn585[,i], dim(distn585)[1], replace = TRUE, prob = w_585)
  weighted_distn370[,i] <- sample(distn370[,i], dim(distn370)[1], replace = TRUE, prob = w_370)
  weighted_distn245[,i] <- sample(distn245[,i], dim(distn245)[1], replace = TRUE, prob = w_245)
  weighted_distn126[,i] <- sample(distn126[,i], dim(distn126)[1], replace = TRUE, prob = w_126)
  weighted_distn119[,i] <- sample(distn119[,i], dim(distn119)[1], replace = TRUE, prob = w_119)
} 


quantile(distn119[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975))
round(quantile(weighted_distn119[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975)),2)
quantile(distn126[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975))
round(quantile(weighted_distn126[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975)),2)
quantile(distn245[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975))
round(quantile(weighted_distn245[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975)),2)
quantile(distn370[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975))
round(quantile(weighted_distn370[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975)),2)
quantile(distn585[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975))
round(quantile(weighted_distn585[,70], c(0.025, 0.05, 0.167, 0.5, 0.833, 0.95, 0.975)),2)

weighted_quant119 <- apply(weighted_distn119, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
weighted_quant126 <- apply(weighted_distn126, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
weighted_quant245 <- apply(weighted_distn245, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
weighted_quant370 <- apply(weighted_distn370, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))
weighted_quant585 <- apply(weighted_distn585, 2, function(x) quantile(x,c(0.025, 0.05, 0.167, 0.25, 0.5, 0.75, 0.833, 0.95, 0.975)))

colnames(weighted_quant119) <- paste("y", years, sep = "")
colnames(weighted_quant126) <- paste("y", years, sep = "")
colnames(weighted_quant245) <- paste("y", years, sep = "")
colnames(weighted_quant370) <- paste("y", years, sep = "")
colnames(weighted_quant585) <- paste("y", years, sep = "")

if (save_pred){
  write.csv(weighted_quant119, "Distributions/weighted_quant119.csv", row.names = TRUE)
  write.csv(weighted_quant126, "Distributions/weighted_quant126.csv", row.names = TRUE)
  write.csv(weighted_quant245, "Distributions/weighted_quant245.csv", row.names = TRUE)
  write.csv(weighted_quant370, "Distributions/weighted_quant370.csv", row.names = TRUE)
  write.csv(weighted_quant585, "Distributions/weighted_quant585.csv", row.names = TRUE)
}

colnames(weighted_distn119) <- paste("y", years, sep = "")
colnames(weighted_distn126) <- paste("y", years, sep = "")
colnames(weighted_distn245) <- paste("y", years, sep = "")
colnames(weighted_distn370) <- paste("y", years, sep = "")
colnames(weighted_distn585) <- paste("y", years, sep = "")

if (save_pred){
  write.csv(weighted_distn119, "Distributions/weighted_distn119.csv", row.names = FALSE)
  write.csv(weighted_distn126, "Distributions/weighted_distn126.csv", row.names = FALSE)
  write.csv(weighted_distn245, "Distributions/weighted_distn245.csv", row.names = FALSE)
  write.csv(weighted_distn370, "Distributions/weighted_distn370.csv", row.names = FALSE)
  write.csv(weighted_distn585, "Distributions/weighted_distn585.csv", row.names = FALSE)
}



plot(density(distn585[,70]), col=rgb(132, 11, 34, maxColorValue = 255, alpha = 100), xlab = "SLE at 2300 relative to 2000 (m)", main = " ", ylim=c(0,0.5), lty = 'dashed', lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
lines(density(distn370[,70]), col=rgb(242, 17, 17, maxColorValue = 255, alpha = 100), lty = 'dashed', lwd=2)
lines(density(distn245[,70]), col=rgb(234, 221, 61, maxColorValue = 255, alpha = 100), lty = 'dashed', lwd = 2)
lines(density(distn126[,70]), col=rgb(29, 51, 84, maxColorValue = 255, alpha = 100), lty = 'dashed', lwd = 2)
lines(density(distn119[,70]), col=rgb(30, 150, 132, maxColorValue = 255, alpha = 100), lty = 'dashed', lwd = 2)

lines(density(weighted_distn585[,70]), col=rgb(132, 11, 34, maxColorValue = 255), lwd=2)
lines(density(weighted_distn370[,70]), col=rgb(242, 17, 17, maxColorValue = 255), lwd=2)
lines(density(weighted_distn245[,70]), col=rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
lines(density(weighted_distn126[,70]), col=rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
lines(density(weighted_distn119[,70]), col=rgb(30, 150, 132, maxColorValue = 255), lwd = 2)

#abline(v = quantile(weighted_distn585[,70], probs = 0.95), col=rgb(132, 11, 34, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn585[,70], probs = 0.95)+0.28, 0.5, round(quantile(weighted_distn585[,70], probs = 0.95),2), col=rgb(132, 11, 34, maxColorValue = 255))
#abline(v = quantile(weighted_distn585[,70], probs = 0.05), col=rgb(132, 11, 34, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn585[,70], probs = 0.05)+0.32, 0.5, round(quantile(weighted_distn585[,70], probs = 0.05),2), col=rgb(132, 11, 34, maxColorValue = 255))

#abline(v = quantile(weighted_distn370[,70], probs = 0.95), col=rgb(242, 17, 17, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn370[,70], probs = 0.95)+0.2, 0.47, round(quantile(weighted_distn370[,70], probs = 0.95),2), col=rgb(242, 17, 17, maxColorValue = 255))
#abline(v = quantile(weighted_distn370[,70], probs = 0.05), col=rgb(242, 17, 17, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn370[,70], probs = 0.05)+0.28, 0.47, round(quantile(weighted_distn370[,70], probs = 0.05),2), col=rgb(242, 17, 17, maxColorValue = 255))

#abline(v = quantile(weighted_distn245[,70], probs = 0.95), col=rgb(234, 221, 61, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn245[,70], probs = 0.95)+0.28, 0.5, round(quantile(weighted_distn245[,70], probs = 0.95),2), col=rgb(234, 221, 61, maxColorValue = 255))
#abline(v = quantile(weighted_distn245[,70], probs = 0.05), col=rgb(234, 221, 61, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn245[,70], probs = 0.05)+0.28, 0.5, round(quantile(weighted_distn245[,70], probs = 0.05),2), col=rgb(234, 221, 61, maxColorValue = 255))

#abline(v = quantile(weighted_distn126[,70], probs = 0.95), col=rgb(29, 51, 84, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn126[,70], probs = 0.95)+0.28, 0.47, round(quantile(weighted_distn126[,70], probs = 0.95),2), col=rgb(29, 51, 84, maxColorValue = 255))
#abline(v = quantile(weighted_distn126[,70], probs = 0.05), col=rgb(29, 51, 84, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn126[,70], probs = 0.05)+0.29, 0.47, round(quantile(weighted_distn126[,70], probs = 0.05),2), col=rgb(29, 51, 84, maxColorValue = 255))

#abline(v = quantile(weighted_distn119[,70], probs = 0.95), col=rgb(30, 150, 132, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn119[,70], probs = 0.95)-0.3, 0.5, round(quantile(weighted_distn119[,70], probs = 0.95),2), col=rgb(30, 150, 132, maxColorValue = 255))
#abline(v = quantile(weighted_distn119[,70], probs = 0.05), col=rgb(30, 150, 132, maxColorValue = 255), lty = 2)
#text(quantile(weighted_distn119[,70], probs = 0.05)-0.3, 0.5, round(quantile(weighted_distn119[,70], probs = 0.05),2), col=rgb(30, 150, 132, maxColorValue = 255))


legend("topright", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1, bty = "n")
if (save_plot){
  dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/weighted_pdfs_SLE_2300.pdf") 
}




layout_mat <- matrix(c(1, 2), nrow = 1, ncol = 2,
                     byrow = TRUE)

my_lay <- layout(mat = layout_mat, 
                 heights = c(1, 1),
                 widths = c(3, 0.5), respect =TRUE)


par(mar = c(5.1, 4.1, 0, 0))
plot(0,0,xlim = c(1955,2300),ylim = c(-1,5),type = "n", xlab = "Year", ylab = paste("SLE (m)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 2000, lwd = 0.5)
abline(h = 0, lwd = 0.5)


lines(years, apply(distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn585, 2, function(x) quantile(x,0.05)), rev(apply(distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn585, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn370, 2, function(x) quantile(x,0.05)), rev(apply(distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn370, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn245, 2, function(x) quantile(x,0.05)), rev(apply(distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn245, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn126, 2, function(x) quantile(x,0.05)), rev(apply(distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn126, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn119, 2, function(x) quantile(x,0.05)), rev(apply(distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn119, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 51), border = NA)

legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1, bty = "n")

par(mar = c(5.1, 0, 0, 0))

boxplot(cbind(weighted_quant119[c(2,3,5,7,8),70],weighted_quant126[c(2,3,5,7,8),70],weighted_quant245[c(2,3,5,7,8),70],weighted_quant370[c(2,3,5,7,8),70],weighted_quant585[c(2,3,5,7,8),70]), frame = FALSE, axes = FALSE,  col=c(rgb(30, 150, 132, maxColorValue = 255, alpha = 153),rgb(29, 51, 84, maxColorValue = 255),rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)),ylim = c(-1,5))

dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/weighted_predictions_bplots.pdf")  

par(mfrow = c(1,1))



layout_mat <- matrix(c(1, 2), nrow = 1, ncol = 2,
                     byrow = TRUE)

my_lay <- layout(mat = layout_mat, 
                 heights = c(1, 1),
                 widths = c(3, 0.5), respect =TRUE)


par(mar = c(5.1, 4.1, 0, 0))
plot(0,0,xlim = c(1955,2100),ylim = c(-0.5,0.5),type = "n", xlab = "Year", ylab = paste("SLE (m)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 2000, lwd = 0.5)
abline(h = 0, lwd = 0.5)


lines(years, apply(distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn585, 2, function(x) quantile(x,0.05)), rev(apply(distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn585, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn370, 2, function(x) quantile(x,0.05)), rev(apply(distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn370, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn245, 2, function(x) quantile(x,0.05)), rev(apply(distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn245, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn126, 2, function(x) quantile(x,0.05)), rev(apply(distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn126, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn119, 2, function(x) quantile(x,0.05)), rev(apply(distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 20), border = NA)

lines(years, apply(weighted_distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(weighted_distn119, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 51), border = NA)

legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=0.7, bty = "n")

par(mar = c(5.1, 0, 0, 0))

boxplot(cbind(weighted_quant119[c(2,3,5,7,8),30],weighted_quant126[c(2,3,5,7,8),30],weighted_quant245[c(2,3,5,7,8),30],weighted_quant370[c(2,3,5,7,8),30],weighted_quant585[c(2,3,5,7,8),30]), frame = FALSE, axes = FALSE,  col=c(rgb(30, 150, 132, maxColorValue = 255, alpha = 153),rgb(29, 51, 84, maxColorValue = 255),rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)),ylim = c(-0.5,0.5))

dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/weighted_predictions_bplots_2100.pdf")  


#plot(0,0,xlim = c(1955,2300),ylim = c(-1,5),type = "n",xlab = "Year", ylab = paste("Sea level contribution relative to 2000 (m SLE)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
#abline(v = 2000, lwd = 0.5)
#abline(h = 0, lwd = 0.5)


#lines(years, apply(distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn585, 2, function(x) quantile(x,0.05)), rev(apply(distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 20), border = NA)

#lines(years, apply(weighted_distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255), lwd = 2)
#polygon(c(years, rev(years)), c(apply(weighted_distn585, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 51), border = NA)


#lines(years, apply(distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn370, 2, function(x) quantile(x,0.05)), rev(apply(distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 20), border = NA)

#lines(years, apply(weighted_distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255), lwd = 2)
#polygon(c(years, rev(years)), c(apply(weighted_distn370, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 51), border = NA)


#lines(years, apply(distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn245, 2, function(x) quantile(x,0.05)), rev(apply(distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 20), border = NA)

#lines(years, apply(weighted_distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
#polygon(c(years, rev(years)), c(apply(weighted_distn245, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 51), border = NA)


#lines(years, apply(distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn126, 2, function(x) quantile(x,0.05)), rev(apply(distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 20), border = NA)

#lines(years, apply(weighted_distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
#polygon(c(years, rev(years)), c(apply(weighted_distn126, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 51), border = NA)


#lines(years, apply(distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 180), lty = 'dashed', lwd = 2)
#polygon(c(years, rev(years)), c(apply(distn119, 2, function(x) quantile(x,0.05)), rev(apply(distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 20), border = NA)

#lines(years, apply(weighted_distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255), lwd = 2)
#polygon(c(years, rev(years)), c(apply(weighted_distn119, 2, function(x) quantile(x,0.05)), rev(apply(weighted_distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 51), border = NA)

#legend("topleft", legend=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
#       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1, bty = "n")

#dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/weighted_predictions.pdf")  