# Perform the leave one out cross validation and evalute main effects

############################# LOOCV ############################################

# Make a function that returns a data frame with leave one out emulator predictions (of the components of U)
# outputs : robj - data frame containing LOOCV info
#           Includes three columns: 
#             actual - actual values from simulations
#             mean   - predictions from the emulator trained on all simulations except that one
#             sd     - standard deviation in the emulator prediction
#
# inputs:   emu - qemu type emulator
#           subset - subset of the simulations one which to perform the LOOCV (default is for LOOCV to be performed on all simulations)
#
LOO_moments <- function(emu, subset = "all") {

  
  stopifnot(inherits(emu, "qemu")) #stop the function if emu doesn't inherit from qemu
  X <- emu$Runs$X #input values
  y <- emu$Runs$y #output values
  n <- nrow(X)    #number of input values
  
  if (subset[1L] == "all") { #if subset is "all", make it 1:n 
    subset <- 1L:n 
  } else { #otherwise, check that every value in subset is in the range 1:n and no duplicate
    stopifnot(subset %in% 1:n, !duplicated(subset))
  }
  
  args <- emu$args #arguments of the emulator

    
    ## evaluate each emulator in series.
    
    robj <- lapply(subset, function(i) {  #for each i in subset, apply the function "make_qemu"
      foo <- do.call("make_qemu", c(alist(
        X = X[-i, , drop=FALSE],  #input data is every value except i
        y = y[-i],                #output data every value except i
        fmla = args$fmla,         #make sure all the arguments match
        inlogs = args$inlogs, 
        offset = args$offset,
        nthreads = args$nthreads, 
        mtry = args$mtry,
        nodesize = args$nodesize
        ), emu$moreargs))
      predict(foo, X[i, , drop=FALSE], type = "moments") # Predict, from the emulator, the prediction on i. 1-row DF with mean and sd
    })
    robj <- do.call("rbind", robj) # put all of the data into one data frame
    
  
  robj <- data.frame(
            actual = y[subset],
            mean = robj$mean,
            sd = robj$sd) #data frame of predictions and values from loocv, with sds
  
  attr(robj, "X") <- X[subset, , drop=FALSE] #This sets an attribute named "X" on the object robj with rows according to those left out
  class(robj) <- c("LOO", class(robj)) #set the class of robj to "LOO".
  
  robj
}

message("running loocv")

# take subsample of 100 entries
oo <- order(X$GSAT_2300)[seq(from = 1, to = dim(y)[1], length.out = n_loocv)] #takes n_out evenly spaced indices from 1 to 1400 (number of simulations)

# apply the LOO_moments function to 
loo <- lapply(1L:r, function(j) { LOO_moments(emu[[j]], subset = oo)}) #for each component j, apply the function "function(j) { LOO_moments(emu[[j]], subset = oo)}" to it
cv_mean <- sapply(loo, "[[", "mean") #extract predictions from loo (take the columns labelled mean)
cv_sd <- sapply(loo, "[[", "sd") #extract sd values from loo

## save the actual time series values (recall that ave stores the 1400 simulation at the 70 year timeslices)
sim <- ave[oo,] 

##transform loo output to time series by reversing the svd
cv_meanx <- sweep(cv_mean %*% Vt, 2L, cc, "+")
cv_sdx <- t(sapply(1L:nrow(cv_sd), function(i) {
  sqrt(colSums((cv_sd[i, ] * Vt)^2)) # n vector
}))

rmse <- lapply(1L:n, function(j) { sqrt(mean((cv_meanx[,j] - sim[,j])^2))})

## want loocv plots at 2100, 2150, 2200, 2300
## time slice index is years[:,c(30, 40, 50, 70)]
tidx <- c(30, 40, 50, 70)

if (plot_loocv){
for(t in tidx){
  wrong <- sim[order(sim[,t]),t] < (cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t]) | sim[order(sim[,t]),t] > (cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t]) #returns array with true if the emulator prediction is more than 2 emulator standard deviations away from the actual
  
  #assign colours: deepskyblue is emulator prediction within 2 emulator SDs of actual, otherwise orange
  col_dots <- rep("deepskyblue4", length(sim[order(sim[,t]),t])) 
  col_wrong <- rgb(243, 122, 107, maxColorValue = 255)
  col_dots[wrong] <- col_wrong
  
  plot(sim[order(sim[,t]),t],cv_meanx[order(sim[,t]),t], pch = 19, xlab =paste("Simulated values at ", years[t]," (SLE (m))",sep=""),
      ylab = paste("Emulated values at ", years[t]," (SLE (m))",sep=""), main = " ", col = col_dots, xlim=c(min(cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t]),max(cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t])), ylim=c(min(cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t]),max(cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t])), cex = 1.1, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  arrows(sim[order(sim[,t]),t], cv_meanx[order(sim[,t]),t] - 2*cv_sdx[order(sim[,t]),t], sim[order(sim[,t]),t], cv_meanx[order(sim[,t]),t] + 2*cv_sdx[order(sim[,t]),t], length=0.05, angle=90, code=3, col = col_dots)
  abline(a = 0, b = 1, lwd = 0.5)
  legend('topleft', legend=c(paste("Coverage: ",100-sum(wrong), "%", sep=""),paste("RMSE:  ", round(as.numeric(rmse[t]),2), "m", sep="")), cex=1.1, bty = "n")
  if (save_valid){
    dev.print(pdf, paste("./Multi_year_plots/svd_qemu_LOO_",years[t],".pdf",sep=""), width=6, height=6)
  }
}
}

# output the loocv data (timeseries for each of the n_loocv loocv simulations alongside actual values, as separate csv files)
colnames(sim) <- years
colnames(cv_meanx) <- years
colnames(cv_sdx) <- years
if (output_loocv_data){
  write.csv(sim, file = "./outputs/emulator_output_data/loocv_simulation_data.csv", row.names = FALSE)
  write.csv(cv_meanx, file = "./outputs/emulator_output_data/loocv_emulator_mean.csv", row.names = FALSE)
  write.csv(cv_sdx, file = "./outputs/emulator_output_data/loocv_emulator_sd.csv", row.names = FALSE)
}
  
  

