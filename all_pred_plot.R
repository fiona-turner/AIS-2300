dev.off()

layout_mat <- matrix(c(1:9), nrow = 3, ncol = 3,
                     byrow = TRUE)

my_lay <- layout(mat = layout_mat, 
                 heights = c(1, 1, 1),
                 widths = c(3, 0.5, 0.5), respect =TRUE)

par(mar = c(5.1, 4.1, 0.1, 0.1))
plot(0,0,xlim = c(1955,2300),ylim = c(-2,6.5),type = "n", xlab = "Year", ylab = paste("SLE (m)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
abline(v = 2000, lwd = 0.5)
abline(h = 0, lwd = 0.5)


lines(years, apply(distn585, 2, function(x) quantile(x,0.5)), col = rgb(132, 11, 34, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(distn585, 2, function(x) quantile(x,0.05)), rev(apply(distn585, 2, function(x) quantile(x,0.95)))), col = rgb(132, 11, 34, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn370, 2, function(x) quantile(x,0.5)), col = rgb(242, 17, 17, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(distn370, 2, function(x) quantile(x,0.05)), rev(apply(distn370, 2, function(x) quantile(x,0.95)))), col = rgb(242, 17, 17, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn245, 2, function(x) quantile(x,0.5)), col = rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(distn245, 2, function(x) quantile(x,0.05)), rev(apply(distn245, 2, function(x) quantile(x,0.95)))), col = rgb(234, 221, 61, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn126, 2, function(x) quantile(x,0.5)), col = rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(distn126, 2, function(x) quantile(x,0.05)), rev(apply(distn126, 2, function(x) quantile(x,0.95)))), col = rgb(29, 51, 84, maxColorValue = 255, alpha = 51), border = NA)


lines(years, apply(distn119, 2, function(x) quantile(x,0.5)), col = rgb(30, 150, 132, maxColorValue = 255), lwd = 2)
polygon(c(years, rev(years)), c(apply(distn119, 2, function(x) quantile(x,0.05)), rev(apply(distn119, 2, function(x) quantile(x,0.95)))), col = rgb(30, 150, 132, maxColorValue = 255, alpha = 51), border = NA)

legend(1925, 6.8, legend=as.expression(bquote(bold("a"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


legend("topleft", legend=c(as.expression(bquote(bold("SSP1-1.9"))), as.expression(bquote(bold("SSP1-2.6"))), as.expression(bquote(bold("SSP2-4.5"))), as.expression(bquote(bold("SSP3-7.0"))), as.expression(bquote(bold("SSP5-8.5")))),
       text.col=c(rgb(30, 150, 132, maxColorValue = 255), rgb(29, 51, 84, maxColorValue = 255), rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)), cex=1.1, bty = "n")

par(mar = c(5.1, 0.1, 0.1, 0.1))

boxplot(cbind(quant119[c(2,3,5,7,8),70],quant126[c(2,3,5,7,8),70],quant245[c(2,3,5,7,8),70],quant370[c(2,3,5,7,8),70],quant585[c(2,3,5,7,8),70]), frame = FALSE, axes = FALSE,  col=c(rgb(30, 150, 132, maxColorValue = 255, alpha = 153),rgb(29, 51, 84, maxColorValue = 255),rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)),ylim = c(-2,6.5))

legend(-1, 6.8, legend=as.expression(bquote(bold("b"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


par(mar = c(5.1, 0.1, 0.1, 0.1))
plot(density(distn585[,70]), col=rgb(132, 11, 34, maxColorValue = 255), xlab = "SLE (m)", main = " ", ylim=c(0,0.35), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, frame = FALSE, yaxt='n')
lines(density(distn370[,70]), col=rgb(242, 17, 17, maxColorValue = 255), lwd=2)
lines(density(distn245[,70]), col=rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
lines(density(distn126[,70]), col=rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
lines(density(distn119[,70]), col=rgb(30, 150, 132, maxColorValue = 255), lwd = 2)

legend(-10, 0.39, legend=as.expression(bquote(bold("c"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


par(mar = c(5.1, 4.1, 0.1, 0.1))
plot(0,0,xlim = c(1955,2300),ylim = c(-2,6.5),type = "n", xlab = "Year", ylab = paste("SLE (m)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
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

legend(1925, 6.8, legend=as.expression(bquote(bold("d"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


par(mar = c(5.1, 0.1, 0.1, 0.1))

boxplot(cbind(mh_quant119[c(2,3,5,7,8),70],mh_quant126[c(2,3,5,7,8),70],mh_quant245[c(2,3,5,7,8),70],mh_quant370[c(2,3,5,7,8),70],mh_quant585[c(2,3,5,7,8),70]), frame = FALSE, axes = FALSE,  col=c(rgb(30, 150, 132, maxColorValue = 255, alpha = 153),rgb(29, 51, 84, maxColorValue = 255),rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)),ylim = c(-2,6.5))

legend(-1, 6.8, legend=as.expression(bquote(bold("e"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


par(mar = c(5.1, 0.1, 0.1, 0.1))
plot(density(mh_distn585[,70]), col=rgb(132, 11, 34, maxColorValue = 255), xlab = "SLE (m)", main = " ", ylim=c(0,0.35), lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, frame = FALSE, yaxt='n')
lines(density(mh_distn370[,70]), col=rgb(242, 17, 17, maxColorValue = 255), lwd=2)
lines(density(mh_distn245[,70]), col=rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
lines(density(mh_distn126[,70]), col=rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
lines(density(mh_distn119[,70]), col=rgb(30, 150, 132, maxColorValue = 255), lwd = 2)

legend(-10, 0.35, legend=as.expression(bquote(bold("f"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")



par(mar = c(5.1, 4.1, 0.1, 0.1))
plot(0,0,xlim = c(1955,2100),ylim = c(-0.5,1),type = "n", xlab = "Year", ylab = paste("SLE (m)"), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
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

legend(1945, 1.2, legend=as.expression(bquote(bold("g"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


par(mar = c(5.1, 0.1, 0.1, 0.1))

boxplot(cbind(mh_quant119[c(2,3,5,7,8),30],mh_quant126[c(2,3,5,7,8),30],mh_quant245[c(2,3,5,7,8),30],mh_quant370[c(2,3,5,7,8),30],mh_quant585[c(2,3,5,7,8),30]), frame = FALSE, axes = FALSE,  col=c(rgb(30, 150, 132, maxColorValue = 255, alpha = 153),rgb(29, 51, 84, maxColorValue = 255),rgb(234, 221, 61, maxColorValue = 255), rgb(242, 17, 17, maxColorValue = 255), rgb(132, 11, 34, maxColorValue = 255)),ylim = c(-0.5,1))

legend(-1, 1.2, legend=as.expression(bquote(bold("h"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


par(mar = c(5.1, 0.1, 0.1, 0.1))
plot(density(mh_distn585[,30]), col=rgb(132, 11, 34, maxColorValue = 255), xlab = "SLE (m)", main = " ", lwd = 2, cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, frame = FALSE, yaxt='n')
lines(density(mh_distn370[,30]), col=rgb(242, 17, 17, maxColorValue = 255), lwd=2)
lines(density(mh_distn245[,30]), col=rgb(234, 221, 61, maxColorValue = 255), lwd = 2)
lines(density(mh_distn126[,30]), col=rgb(29, 51, 84, maxColorValue = 255), lwd = 2)
lines(density(mh_distn119[,30]), col=rgb(30, 150, 132, maxColorValue = 255), lwd = 2)

legend(-2, 1.3, legend=as.expression(bquote(bold("i"))),
       text.col=rgb(0, 0, 0, maxColorValue = 255), cex=1.5, bty = "n")


dev.print(pdf, width = 11.69, height = 8.27, "Multi_year_plots/all_predictions_mh.pdf")  
