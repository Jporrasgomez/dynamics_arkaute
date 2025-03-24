# M.J. Lajeunesse, University of South Florida, 11/26/14, lajeunesse@usf.edu
#
# Log response ratio Monte Carlo simulation and diagnostics.
# RR is the orginal response ratio, and delta-RR and sigma-RR are bias corrected versions.
#
# This script uses two R libraries: moments (L. Komsta: http://cran.r-project.org/web/packages/moments/index.html) 
#                                   parallel (D. Eddelbuettel: http://cran.r-project.org/web/views/HighPerformanceComputing.html)

rm(list = ls())
set.seed(13)

RRsim_helper <- function(a, K, step, atotal, btotal, var_a, var_b, N_t, N_c) {
  
  # moments: v. 0.13
  library(moments); 
  
  X_t <- rep(NA,K); var_t <- rep(NA,K);
  X_c <- rep(NA,K); var_c <- rep(NA,K);
  
  count <- 0
  
  for(b in seq(0.01, btotal + 0.01, by = step)) { 
    
    aList <- data.frame(X = rlnorm(N_t*K, meanlog = log((a^2)/(sqrt(var_a + a^2))), sdlog = sqrt(log(var_a/(a^2) + 1.0))), group=rep(1:K, each=N_t))  
    bList <- data.frame(Y = rlnorm(N_c*K, meanlog = log((b^2)/(sqrt(var_b + b^2))), sdlog = sqrt(log(var_b/(b^2) + 1.0))), group=rep(1:K, each=N_c))	
    
    X_t <- tapply(aList$X, list(aList$group), mean); sd_t <- tapply(aList$X, list(aList$group), sd);
    X_c <- tapply(bList$Y, list(bList$group), mean); sd_c <- tapply(bList$Y, list(bList$group), sd);
    
 
    # RR
    RR <- log(X_t / X_c) ## double checked with metawin 8/7/13
    var_RR <- (sd_t^2)/(N_t * (X_t^2)) + (sd_c^2)/(N_c * (X_c^2))  ## double checked with metawin 8/7/13
    
    effects <- data.frame(X_t=X_t, sd_t=sd_t, N_t=N_t, X_c=X_c, sd_c=sd_c, N_c=N_c, RR=RR, var_RR=var_RR)
    effects <- effects[complete.cases(effects),]
  
    RR_mean <- mean(effects$RR)
    RR_var <- var(effects$RR)
    RR_skew <- skewness(effects$RR)
    var_RR_mean <- mean(effects$var_RR)
    var_RR_var <- var(effects$var_RR)	


    # delta-RR
    delta_RR <- effects$RR + 0.5 * ( (effects$sd_t^2)/(effects$N_t * (effects$X_t^2)) - (effects$sd_c^2)/(effects$N_c * (effects$X_c^2)) )
    var_delta_RR <- effects$var_RR + 0.5 * ( (effects$sd_t^4)/((effects$N_t^2)*(effects$X_t^4)) + (effects$sd_c^4)/((effects$N_c^2)*(effects$X_c^4)) )

    delta_RR_mean <- mean(delta_RR)
    delta_RR_var <- var(delta_RR)
    delta_RR_skew <- skewness(delta_RR)
    var_delta_RR_mean <- mean( var_delta_RR )
    var_delta_RR_var <- var( var_delta_RR )		


    # sigma-RR    
    sigma_RR <- 0.5 * log((effects$X_t^2 + (effects$sd_t^2)/effects$N_t)/(effects$X_c^2 + (effects$sd_c^2)/effects$N_c))
    var_sigma_RR <- 2.0 * effects$var_RR - log(1.0 + effects$var_RR + ((effects$sd_t^2)*(effects$sd_c^2))/(effects$N_c*effects$N_t*(effects$X_t^2)*(effects$X_c^2)))
  
    sigma_RR_mean <- mean(sigma_RR)
    sigma_RR_var <- var(sigma_RR)
    sigma_RR_skew <- skewness(sigma_RR)
    var_sigma_RR_mean <- mean(var_sigma_RR)
    var_sigma_RR_var <- var(var_sigma_RR)

    # Geary's diagnostic
    std_mean_a <- effects$X_t * sqrt(effects$N_t) / effects$sd_t 
    std_mean_b <- effects$X_c * sqrt(effects$N_c) / effects$sd_c 
    Geary_diagnostic <- sum((std_mean_a >= 3.0) & (std_mean_b >= 3.0)) / ((length(std_mean_b) + length(std_mean_a)) / 2.0)

    # updated Geary's diagnostic
    std_mean_a <- (4.0 * effects$X_t * effects$N_t ^ (3.0 / 2.0)) / (effects$sd_t * ( 1.0 + 4.0 * effects$N_t))
    std_mean_b <- (4.0 * effects$X_c * effects$N_c ^ (3.0 / 2.0)) / (effects$sd_c * ( 1.0 + 4.0 * effects$N_c)) 
    Geary_new_diagnostic <- sum((std_mean_a >= 3.0) & (std_mean_b >= 3.0)) / ((length(std_mean_b) + length(std_mean_a)) / 2.0)

    # collect results
    if(count == 0) {
      results <- matrix(c(	a, 
				b, 
				N_t, 
				N_c,
				var_a,
				var_b,
				log(a/b),
				RR_mean,
				RR_var,
				RR_skew,
				var_RR_mean,
				var_RR_var,	
				delta_RR_mean,
				delta_RR_var,
				delta_RR_skew,
				var_delta_RR_mean,
				var_delta_RR_var,		
				sigma_RR_mean,
				sigma_RR_var,
				sigma_RR_skew,
				var_sigma_RR_mean,
				var_sigma_RR_var,
				Geary_diagnostic,
				Geary_new_diagnostic), nrow=1, ncol = 24, byrow = TRUE);
      colnames(results) <- c(	"a", 
				"b", 
				"N_t", 
				"N_c",
				"var_a",
				"var_b",
				"log_ab",
				"RR_mean",
				"RR_var",
				"RR_skew",
				"var_RR_mean",
				"var_RR_var",	
				"delta_RR_mean",
				"delta_RR_var",
				"delta_RR_skew",
				"var_delta_RR_mean",
				"var_delta_RR_var",		
				"sigma_RR_mean",
				"sigma_RR_var",
				"sigma_RR_skew",
				"var_sigma_RR_mean",
				"var_sigma_RR_var",
				"Geary_diagnostic",
				"Geary_new_diagnostic" );
    } else {
      results <- rbind(results, matrix(c(	a, 
						b, 
						N_t, 
						N_c,
						var_a,
						var_b,
						log(a/b),
						RR_mean,
						RR_var,
						RR_skew,
						var_RR_mean,
						var_RR_var,	
						delta_RR_mean,
						delta_RR_var,
						delta_RR_skew,
						var_delta_RR_mean,
						var_delta_RR_var,		
						sigma_RR_mean,
						sigma_RR_var,
						sigma_RR_skew,
						var_sigma_RR_mean,
						var_sigma_RR_var,
						Geary_diagnostic,
						Geary_new_diagnostic), nrow=1, ncol = 24, byrow = TRUE)); 
      }
      count <- count + 1
    }
  
    # alert when simulation is complete
    system(paste('"c:/program files (x86)/videolan/vlc/vlc"', 'C:/Users/lajeunesse/Desktop/CatbirdGrayMew.mp3 vlc://quit'), wait = FALSE)

    return(results)
}


RRsim <- function(K, step, atotal, btotal, var_a, var_b, N_t, N_c, FileResults_Name) {

    # parallel: v. 3.0.2
    library(parallel) 
    numCores <- detectCores(logical = TRUE)
    cl <- makeCluster(numCores)
    	the_a_list <- seq(0.01, atotal + 0.01, by = step)
    	allResults <- parLapply(cl, the_a_list, RRsim_helper, K, step, atotal, btotal, var_a, var_b, N_t, N_c)
    stopCluster(cl)
       
    allResultsMerged <- do.call(rbind, allResults)
    write.table(data.frame(allResultsMerged), file = FileResults_Name, append = FALSE, quote = FALSE, sep = "\t", na = "NA", row.names = FALSE, col.names = TRUE)

    return(data.frame(allResultsMerged))
}

# a = t (treatment), and b = c (control) | 4, 8, 16, and 32 sample size simulations
system.time(invisible(RRsim(K = 100000, step = 0.25, atotal = 8, btotal = 8, var_a = 1.0, var_b = 1.0, N_t = 2, N_c = 2, FileResults_Name = "N_is_2.txt")))
system.time(invisible(RRsim(K = 100000, step = 0.25, atotal = 8, btotal = 8, var_a = 1.0, var_b = 1.0, N_t = 4, N_c = 4, FileResults_Name = "N_is_4.txt")))
system.time(invisible(RRsim(K = 100000, step = 0.25, atotal = 8, btotal = 8, var_a = 1.0, var_b = 1.0, N_t = 8, N_c = 8, FileResults_Name = "N_is_8.txt")))
system.time(invisible(RRsim(K = 100000, step = 0.25, atotal = 8, btotal = 8, var_a = 1.0, var_b = 1.0, N_t = 16, N_c = 16, FileResults_Name = "N_is_16.txt")))
