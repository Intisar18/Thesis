# -----------------------------------------------------------------------------
# TEST data
# -----------------------------------------------------------------------------
# fitObj = ourfit_test 
# y = train_intensity
# X = train_dtm
# nfolds = 5
# errtype = "mean-squared-error"

# # ... 
# rho = 0.01, 
# eps1 = 1e-5, 
# eps2 = 1e-5, 
# maxite = 1e4

# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------
rarefit.cv_test <- function(
  fitObj, 
  y, 
  X, 
  errtype = "mean-squared-error", 
  nfolds = 5, ...) {
  
  n <- length(y)
  nlam <- length(fitObj$lambda)
  nalpha <- length(fitObj$alpha)
  errs <- array(NA, dim=c(nlam, nalpha, nfolds))

  # define error function
  errfun <- function(est, truth) colMeans((est - truth)^2)
  if (errtype == "mean-absolute-error") {
    errfun <- function(est, truth) colMeans(abs(est - truth))
  } else if (errtype != "mean-squared-error") {
    stop("The error function needs to be either mean squared error or mean absolute error.")
  }

  # make folds
  nn <- round(n / nfolds)
  sizes <- rep(nn, nfolds)
  sizes[nfolds] <- sizes[nfolds] + n - nn * nfolds
  b <- c(0, cumsum(sizes))
  set.seed(100) # set.seed for random number generator
  ii <- sample(n)
  folds <- list()
  for (i in seq(nfolds))
    folds[[i]] <- ii[seq(b[i] + 1, b[i + 1])]
  folds

  # Fit based on folds and compute error metric
  for (i in seq(nfolds)) {
    
    # DEBUG 
    message("fold: ", i)

    # fit model on all but the ith fold
    fit_cv <- rarefit(
      y = y[-folds[[i]]], 
      X = X[-folds[[i]], ], 
      A = fitObj$A, 
      Q = fitObj$Q,
      intercept = fitObj$intercept, 
      lambda = fitObj$lambda, 
      alpha = fitObj$alpha, 
      ...        # Non-test
      # rho = 0.01,    # Test data
      # eps1 = 1e-5, 
      # eps2 = 1e-5, 
      # maxite = 1e4
      )
    

    message("t/f intercept? ", fitObj$intercept)

    pred_te <- lapply(seq(nalpha), 
      function(k) {
          if (fitObj$intercept) {
            as.matrix(X[folds[[i]], ]) %*% fit_cv$beta[[k]] + 
              rep(fit_cv$beta0[[k]], 
                  each = length(folds[[i]]))
          } else {
            X[folds[[i]], ] %*% fit_cv$beta[[k]]
          }
        })

    for (k in seq(nalpha)) errs[, k, i] <- errfun(pred_te[[k]], y[folds[[i]]])
    cat("##########################\n")
    cat(sprintf("Finished model fits for fold[%s].\n", i))
    cat("##########################\n")
  }

  m <- apply(errs, c(1, 2), mean)
  se <- apply(errs, c(1, 2), stats::sd) / sqrt(nfolds)
  ibest <- which(m == min(m), arr.ind = TRUE)[1, , drop = FALSE]

  list (folds = folds, errs = errs, m = m, se = se, ibest = ibest,
        lambda.best = fitObj$lambda[ibest[1]], alpha.best = fitObj$alpha[ibest[2]])
}





# -----------------------------------------------------------------------------
# PREDICT
# -----------------------------------------------------------------------------




#' Make predictions from a rarefit object and a rarefit.cv object
#'
#' The function makes predictions using a \code{rarefit} object at optimal
#' (\code{lambda}, \code{alpha}) chosen by \code{rarefit.cv}.
#'
#' @param fitObj Output of \code{rarefit}.
#' @param cvObj Output of \code{rarefit.cv}.
#' @param newx Matrix of new values for x at which predictions are made.
#'
#' @return Returns a sequence of predictions.
#'
#' @examples
#' \dontrun{
#' # See vignette for more details.
#' set.seed(100)
#' ts <- sample(1:length(data.rating), 400) # Train set indices
#' # Fit the model on train set
#' ourfit <- rarefit(y = data.rating[ts], X = data.dtm[ts, ], hc = data.hc, lam.min.ratio = 1e-6,
#'                   nlam = 20, nalpha = 10, rho = 0.01, eps1 = 1e-5, eps2 = 1e-5, maxite = 1e4)
#' # Cross validation
#' ourfit.cv <- rarefit.cv(ourfit, y = data.rating[ts], X = data.dtm[ts, ],
#'                         rho = 0.01, eps1 = 1e-5, eps2 = 1e-5, maxite = 1e4)
#' # Prediction on test set
#' pred <- rarefit.predict(ourfit, ourfit.cv, data.dtm[-ts, ])
#' pred.error <- mean((pred - data.rating[-ts])^2)
#' }
#'
#' @seealso \code{\link{rarefit}}, \code{\link{rarefit.cv}}
#'
#' @export




# # Example: # Prediction on test set
# pred <- rarefit_predict_test(ourfit_test, ourfit_cv_test, dev_dtm)

# -----------------------------------------------------------------------------
# TEST DATA
# -----------------------------------------------------------------------------
# fitObj = ourfit_test 
# cvObj = ourfit_cv_test 
# newx = dev_dtm 


# -----------------------------------------------------------------------------
# Modified function
# -----------------------------------------------------------------------------
rarefit.predict_test <- function(fitObj, cvObj, newx) {


  ibest.lambda <- cvObj$ibest[1]
  ibest.alpha <- cvObj$ibest[2]
  if (fitObj$intercept) {
    as.vector(
      as.matrix(newx) %*% 
      fitObj$beta[[ibest.alpha]][, ibest.lambda] + 
      fitObj$beta0[[ibest.alpha]][ibest.lambda])
  } else {
    as.vector(newx %*% fitObj$beta[[ibest.alpha]][, ibest.lambda])
  }


}
