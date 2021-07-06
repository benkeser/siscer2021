## ----opts, eval = TRUE, echo = FALSE, message = FALSE--------------------
options(width = 60)


## ----simulating_data-----------------------------------------------------
# set a seed for reproducibility
set.seed(212)
# sample size
n <- 100
# W1 has Normal distribution, W2 has Uniform distribution
W1 <- rnorm(n = n); W2 <- runif(n = n)
# pr(A = 1 | W) is logistic linear in W
g1W <- plogis(-1 + W1 - W2 + W1*W2)
# generate binary treatment 
A <- rbinom(n = n, size = 1, prob = g1W)
# E[Y | A, W] is linear in A, W
QAW <- W1 - W2 + A
# generate outcome by adding random error with N(0,1) distribution
Y <- QAW + rnorm(n = n)


## ----naive_reg-----------------------------------------------------------
# fit Q1 on full data
Q1_fit <- glm(Y ~ W1 + W2 + A)
# get predicted value for each observation
Q1n <- as.numeric(predict(Q1_fit))
# compute mean squared-error
risk_Q1 <- mean((Y - Q1n)^2)
# fit Q2 on full data
Q2_fit <- glm(Y ~ I(W1^2) + I(W2^2) + W1*W2 + W1*A + W2*A)
# get predicted value for each observation
Q2n <- as.numeric(predict(Q2_fit))
# compute mean squared-error
risk_Q2 <- mean((Y - Q2n)^2)


## ----two_fold_cv---------------------------------------------------------
# first 100 observations go in split 2, second 100 in split 2
split <- c(rep(1, n/2), rep(2, n/2))
# make a data.frame of all the data
full_data <- data.frame(W1 = W1, W2 = W2, A = A, Y = Y)
# data.frame of only split 1 observations
split1 <- subset(full_data, split == 1)
# data.frame of only split 2 observations
split2 <- subset(full_data, split == 2)
# fit Q1 in split 1
Q1_fit_split1 <- glm(Y ~ W1 + W2 + A, data = split1)
# predict from split 1 fit in split 2
Q1n_split2 <- predict(Q1_fit_split1, newdata = split2)
# estimate of MSE based on split 2
risk_Q1_split2 <- mean((Y[split == 2] - Q1n_split2)^2)
# fit Q1 in split 2
Q1_fit_split2 <- glm(Y ~ W1 + W2 + A, data = split2)
# predict from split 2 fit in split 1
Q1n_split1 <- predict(Q1_fit_split1, newdata = split1)
# estimate of MSE based on split 1
risk_Q1_split1 <- mean((Y[split == 1] - Q1n_split1)^2)
# average the two estimates
cv_risk_Q1 <- (risk_Q1_split1 + risk_Q1_split2) / 2


## ----two_fold_cv2--------------------------------------------------------
# fit Q2 in split 1

# predict from split 1 fit in split 2

# estimate of MSE based on split 2

# fit Q2 in split 2

# predict from split 2 fit in split 1

# estimate of MSE based on split 1

# average the two estimates



## ----two_fold_cv2_answer, eval = TRUE, echo = FALSE----------------------
# fit Q2 in split 1
Q2_fit_split1 <- glm(Y ~ I(W1^2) + I(W2^2) + W1*W2 + W1*A + W2*A, data = split1)
# predict from split 1 fit in split 2
Q2n_split2 <- predict(Q2_fit_split1, newdata = split2)
# estimate of MSE based on split 2
risk_Q2_split2 <- mean((Y[split == 2] - Q2n_split2)^2)
# fit Q2 in split 2
Q2_fit_split2 <- glm(Y ~ I(W1^2) + I(W2^2) + W1*W2 + W1*A + W2*A, data = split2)
# predict from split 2 fit in split 1
Q2n_split1 <- predict(Q2_fit_split2, newdata = split1)
# estimate of MSE based on split 1
risk_Q2_split1 <- mean((Y[split == 1] - Q2n_split1)^2)
# average the two estimates
cv_risk_Q2 <- (risk_Q2_split1 + risk_Q2_split2) / 2


## ------------------------------------------------------------------------
# a function to generate a prediction from an ensemble of Q1 and Q2
# Q1_fit and Q2_fit are fitted glm's
# newdata is a data.frame that you want to get predictions on
# Q1_weight is the weight given to the prediction from Q1_fit
# Q2_weight is 1 - Q1_weight
ensemble_predict <- function(Q1_fit, Q2_fit, newdata, 
                   			 Q1_weight, Q2_weight = 1 - Q1_weight){
  # make sure weights approximately sum to 1
  stopifnot(abs(1 - Q1_weight - Q2_weight) < 1e-5)
  # prediction from Q1_fit on newdata
  Q1n <- predict(Q1_fit, newdata = newdata)
  # prediction from Q2_fit on newdata
  Q2n <- predict(Q2_fit, newdata = newdata)
  # weighted combination
  ensemble_prediction <- Q1_weight * Q1n + Q2_weight * Q2n
  return(as.numeric(ensemble_prediction))
}


## ----call_ens_1----------------------------------------------------------
# just get data for the first observation
obs1 <- full_data[1,]
obs1 
# get ensemble prediction for first observation using equal weights
ensemble_predict(Q1_fit = Q1_fit, Q2_fit = Q2_fit, 
                 newdata = obs1, Q1_weight = 0.5)
# should be the same as
Q1n[1] * 0.5 + Q2n[1] * 0.5


## ----call_ens_2, eval = FALSE--------------------------------------------
## # get ensemble prediction for a sequence of weights between 0,1
## weight_sequence <- seq(from = 0, to = 1, length = 500)
## obs1_predict <- sapply(weight_sequence, ensemble_predict, Q1_fit = Q1_fit,
##                        Q2_fit = Q2_fit, newdata = obs1)
## # set graphical parameters
## par(mar = c(4.1, 4.1, 0.5, 0.5), mgp = c(1.3, 0.45, 0))
## plot(obs1_predict ~ weight_sequence, type = "l", lwd = 2, bty = "n",
##      xlab = expression(bar(Q)[list("n",1)]*" weight"),
##      ylab = "Prediction", cex.lab = 0.65, cex.axis = 0.65,
##      ylim = c(-0.6, -0.35))
## points(y = obs1_predict[1], x = 0, pch = 19, col = 'red', cex = 1.2)
## points(y = obs1_predict[500], x = 1, pch = 19, col = 'blue', cex = 1.2)


## ----call_ens_2, echo = FALSE, eval = TRUE, fig.height = 2.5, fig.width = 3.5, fig.align = "center"----
# get ensemble prediction for a sequence of weights between 0,1
weight_sequence <- seq(from = 0, to = 1, length = 500)
obs1_predict <- sapply(weight_sequence, ensemble_predict, Q1_fit = Q1_fit, 
                       Q2_fit = Q2_fit, newdata = obs1)
# set graphical parameters
par(mar = c(4.1, 4.1, 0.5, 0.5), mgp = c(1.3, 0.45, 0))
plot(obs1_predict ~ weight_sequence, type = "l", lwd = 2, bty = "n",
     xlab = expression(bar(Q)[list("n",1)]*" weight"),
     ylab = "Prediction", cex.lab = 0.65, cex.axis = 0.65, 
     ylim = c(-0.6, -0.35))
points(y = obs1_predict[1], x = 0, pch = 19, col = 'red', cex = 1.2)
points(y = obs1_predict[500], x = 1, pch = 19, col = 'blue', cex = 1.2)


## ----ensemble_cv_risk----------------------------------------------------
# get ensemble predictions on split 2 from fits in split 1
enspred_split2 <- ensemble_predict(Q1_fit = Q1_fit_split1,
                                   Q2_fit = Q2_fit_split1,
                                   Q1_weight = 0.5,
                                   newdata = split2)
# estimate of MSE based on split 2
risk_ens_split2 <- mean((Y[split == 2] - enspred_split2)^2)
# get ensemble predictions on split 2 from fits in split 1
enspred_split1 <- ensemble_predict(Q1_fit = Q1_fit_split2,
                                   Q2_fit = Q2_fit_split2,
                                   Q1_weight = 0.5,
                                   newdata = split1)
# estimate of MSE based on split 1
risk_ens_split1 <- mean((Y[split == 1] - enspred_split1)^2)
# average the two estimates
cv_risk_ens <- (risk_ens_split1 + risk_ens_split2) / 2


## ----cv_risk_fn----------------------------------------------------------
# define an empty vector of cv risk values
cv_risks <- rep(NA, length(weight_sequence))
# for each value of weights compute the cv risk
for(i in seq_along(weight_sequence)){
	# get ensemble predictions on split 2 from fits in split 1
	enspred_split2 <- ensemble_predict(Q1_fit = Q1_fit_split1,
	                                   Q2_fit = Q2_fit_split1,
	                                   Q1_weight = weight_sequence[i],
	                                   newdata = split2)
	# estimate of MSE based on split 2
	risk_ens_split2 <- mean((Y[split == 2] - enspred_split2)^2)
	# get ensemble predictions on split 2 from fits in split 1
	enspred_split1 <- ensemble_predict(Q1_fit = Q1_fit_split2,
	                                   Q2_fit = Q2_fit_split2,
	                                   Q1_weight = weight_sequence[i],
	                                   newdata = split1)
	# estimate of MSE based on split 1
	risk_ens_split1 <- mean((Y[split == 1] - enspred_split1)^2)
	# save cv risk in cv_risks vector
	cv_risks[i] <- (risk_ens_split1 + risk_ens_split2) / 2
}	


## ----plot_cv_risk_by_weight, echo = TRUE, eval = FALSE-------------------
## # set some graphical parameters
## par(mar = c(4.1, 4.1, 0.5, 0.5), mgp = c(1.3, 0.45, 0))
## # plot all risks
## plot(cv_risks ~ weight_sequence, bty = 'n',
##   	 xlab = expression(bar(Q)[list("n",1)]*" weight"),
##      ylab = "CV-risk of ensemble", cex.lab = 0.65, cex.axis = 0.65)
## # add solid point where risk is lowest
## min_idx <- which.min(cv_risks)
## points(y = cv_risks[min_idx], x = weight_sequence[min_idx], col = 'blue',
##        pch = 19, cex = 1.5)


## ---- echo = FALSE, eval = TRUE------------------------------------------
min_idx <- which.min(cv_risks) 


## ----plot_cv_risk_by_weight, echo = FALSE, eval = TRUE,fig.height = 2.5, fig.width = 3.5, fig.align = "center"----
# set some graphical parameters
par(mar = c(4.1, 4.1, 0.5, 0.5), mgp = c(1.3, 0.45, 0))
# plot all risks
plot(cv_risks ~ weight_sequence, bty = 'n', 
  	 xlab = expression(bar(Q)[list("n",1)]*" weight"),
     ylab = "CV-risk of ensemble", cex.lab = 0.65, cex.axis = 0.65)
# add solid point where risk is lowest
min_idx <- which.min(cv_risks)
points(y = cv_risks[min_idx], x = weight_sequence[min_idx], col = 'blue', 
       pch = 19, cex = 1.5)


## ----load_sl, eval = TRUE, echo = FALSE, message = FALSE-----------------
library(SuperLearner); library(quadprog)


## ----simple_call_to_sl, echo = TRUE, eval = FALSE------------------------
## # set a seed for reproducibility
## set.seed(123)
## # basic call to SuperLearner to estimate E[Y | A, W]
## sl_fit1 <- SuperLearner(Y = Y,
##                         X = data.frame(W1 = W1, W2 = W2, A = A),
##                         SL.library = c("SL.glm", "SL.mean"),
##                         family = gaussian(),
##                         method = "method.CC_LS",
##                         cvControl = list(V = 2),
##                         verbose = FALSE)
## # see what the printed output contains
## sl_fit1


## ----simple_call_to_sl, echo = FALSE, eval = TRUE------------------------
# set a seed for reproducibility
set.seed(123)
# basic call to SuperLearner to estimate E[Y | A, W]
sl_fit1 <- SuperLearner(Y = Y, 
                        X = data.frame(W1 = W1, W2 = W2, A = A),
                        SL.library = c("SL.glm", "SL.mean"),
                        family = gaussian(), 
                        method = "method.CC_LS",
                        cvControl = list(V = 2),
                        verbose = FALSE)
# see what the printed output contains
sl_fit1


## ----names_sl------------------------------------------------------------
# what objects are in the output
names(sl_fit1)


## ----predict_sl_new_data-------------------------------------------------
# a new observation
new_obs <- data.frame(A = 1, W1 = 0, W2 = 0)
# call predict using sl_fit1
new_predictions <- predict(sl_fit1, newdata = new_obs)
# super learner prediction is in $pred
new_predictions$pred
# the prediction made by each estimator is in $library.predict
new_predictions$library.predict


## ----look_at_slglm-------------------------------------------------------
SL.glm


## ----show_slmean---------------------------------------------------------
SL.mean


## ----our_own_wrapper-----------------------------------------------------
SL.quadglm <- function(Y, X, newX, family, ...){
	# this line assumes X will have a named columns W1, W2, and A
	# we are also ignoring the family input, assuming that we will be
	# using this function for linear regression only
	quadglm_fit <- glm(Y ~ W1 + I(W1^2) + W2 + I(W2^2) + A, data = X)
    # get predictions on newX
	pred <- predict(quadglm_fit, newdata = newX)
  	# format the output as named list
  	fit <- list(fitted_model = quadglm_fit, message = "Hello world!")
  	out <- list(fit = fit, pred = pred)
  	# give the object a class
  	class(out$fit) <- "SL.quadglm"
	# return the output
	return(out)
}


## ----fit_sl2-------------------------------------------------------------
# set a seed for reproducibility
set.seed(123)
# call to SuperLearner that includes our own wrapper
sl_fit2 <- SuperLearner(Y = Y, 
                        X = data.frame(W1 = W1, W2 = W2, A = A),
                        SL.library = c("SL.glm", "SL.mean", "SL.quadglm"),
                        family = gaussian(), 
                        method = "method.CC_LS",
                        cvControl = list(V = 2),
                        verbose = FALSE)


## ----find_message--------------------------------------------------------
sl_fit2$fitLibrary$SL.quadglm$message


## ----break_predictions, eval = FALSE, error = TRUE-----------------------
## predict(sl_fit2, newdata = new_obs)


## ----define_predict_method-----------------------------------------------
predict.SL.quadglm <- function(object, newdata, ...){
	# object will be the $fit entry of the output from SL.quadglm
	# since class(object$fitted_model) = "glm", this will in turn call
	# predict.glm to obtain predictions 
	pred <- predict(object$fitted_model, newdata = newdata)
	# return predictions
	return(pred)
}


## ----fix_predictions-----------------------------------------------------
# this calls predict.SuperLearner(object = sl_fit2, ...)
sl2_pred <- predict(sl_fit2, newdata = new_obs)
# super learner prediction
sl2_pred$pred
# library predictions
sl2_pred$library.predict


## ----true_propens_ex, eval = FALSE, error = TRUE-------------------------
## # define wrapper function
## SL.truepropensity <- function(Y, X, newX, family, ...){
## 	# fit logistic regression model with interaction between W1 & W2
## 	# get predictions on newX
## 	# format output
## 	# return output
## }
## 
## # define predict method
## predict.SL.truepropensity <- function(object, newdata, ...){
## 	# predict from object
## 	# return predictions
## }


## ----true_propens_ex2, eval = FALSE, error = TRUE------------------------
## sl_fit3 <- SuperLearner(Y = ,
##                         X = ,
##                         family = ,
##                         SL.library = ,
##                         # use negative log-likelihood loss
##                         method = "method.CC_nloglik",
##                         cvControl = list(V = 2),
##                         verbose = FALSE)


## ----load_nloptr, echo = FALSE, message = FALSE--------------------------
library(nloptr)


## ----true_propens_ex_sol, echo = FALSE-----------------------------------
# define wrapper function
SL.truepropensity <- function(Y, X, newX, family, ...){
	# fit logistic regression model with interaction between W1 & W2
	fitted_model <- glm(Y ~ W1*W2, data = X, family = binomial())
	# get predictions on newX
	# type = 'response' is key to getting predictions on the correct scale!
	pred <- predict(fitted_model, newdata = newX, type = "response")
	# format output
	out <- list(fit = list(fitted_model = fitted_model), pred = pred)
	# return output
	return(out)
}

# define predict method
predict.SL.truepropensity <- function(object, newdata, ...){
	# predict from object
	pred <- predict(object$fitted_model, newdata = newdata, type = "response")
	# return predictions
	return(pred)
}


## ----true_propens_ex2_sol, echo = FALSE, eval = TRUE---------------------
# set seed
set.seed(1234)
# fit super learner
sl_fit3 <- SuperLearner(Y = A,
                        X = data.frame(W1 = W1, W2 = W2),
                        family = binomial(),
                        SL.library = c("SL.glm","SL.mean","SL.truepropensity"),
                        # use negative log-likelihood loss
                        method = "method.CC_nloglik",
                        cvControl = list(V = 2), 
                        verbose = FALSE)


## ----print_output_sl_fit3, echo = FALSE----------------------------------
sl_fit3


## ----look_at_screen------------------------------------------------------
screen.corP


## ----sl_with_screens-----------------------------------------------------
sl_fit4 <- SuperLearner(Y = Y, 
                        X = data.frame(W1 = W1, W2 = W2, A = A),
                        SL.library = list(c("SL.glm", "All"),
                                          c("SL.glm", "screen.corP"),
                                          c("SL.mean", "All")),
                        family = gaussian(), 
                        method = "method.CC_LS",
                        cvControl = list(V = 2),
                        verbose = FALSE)



## ----look_at_output------------------------------------------------------
sl_fit4


## ----my_screening--------------------------------------------------------
screen_confounders <- function(Y, X, family, trt_name = "A", 
                               change = 0.1, ...){
	# fit treatment only model & get coefficient for treatment
	fit_init <- glm(as.formula(paste0("Y ~ ", trt_name)), 
	                data = X, family = family)
	trt_coef <- fit_init$coef[2]
	# identify which column of X is the trt variable
	trt_col <- which(colnames(X) == trt_name)
	# set all variables except trt to not be included initially 
	include <- rep(FALSE, ncol(X));	include[trt_col] <- TRUE
	# loop over variables in X other than trt
	for(j in seq_len(ncol(X))[-trt_col]){
		# find variable name
		var_name <- colnames(X)[j]
		# fit trt + variable model, get trt coefficient
		fit <- glm(as.formula(paste0("Y ~ ", trt_name, "+", var_name)), 
		           data = X, family = family)
		new_trt_coef <- fit$coef[2]
		# check if changed more than specified amount
		include[j] <- abs((new_trt_coef - trt_coef)/trt_coef) > change
	}
	return(include)
}


## ----sl_with_screens2----------------------------------------------------
sl_fit5 <- SuperLearner(Y = Y, 
                        X = data.frame(W1 = W1, W2 = W2, A = A),
                        SL.library = list(c("SL.glm", "screen_confounders"),
                                          c("SL.mean", "All")),
                        family = gaussian(), 
                        method = "method.CC_LS",
                        cvControl = list(V = 2),
                        verbose = FALSE)
# print results
sl_fit5


## ----load_ggplot, echo=FALSE, message=FALSE------------------------------
library(ggplot2)


## ----cvSuperLearner, message=FALSE, cache=TRUE, warning=FALSE, eval=FALSE, echo=TRUE----
## # set seed for reproducibility
## set.seed(1234)
## 
## # two-fold cross-validated evaluation of two-fold super learner
## cv_sl <- CV.SuperLearner(
##   Y = Y, X = data.frame(W1 = W1, W2 = W2, A = A),
##   family = gaussian(),
##   method="method.CC_LS",
##   SL.library = c("SL.glm","SL.mean","SL.quadglm"),
##   cvControl = list(V = 2),
##   innerCvControl = list(list(V = 2),
##                         list(V = 2))
## )
## # plot the results
## plot(cv_sl)


## ----cvSuperLearner, eval=TRUE, echo=FALSE, fig.height=5-----------------
# set seed for reproducibility 
set.seed(1234)

# two-fold cross-validated evaluation of two-fold super learner
cv_sl <- CV.SuperLearner(
  Y = Y, X = data.frame(W1 = W1, W2 = W2, A = A),
  family = gaussian(),
  method="method.CC_LS",
  SL.library = c("SL.glm","SL.mean","SL.quadglm"),
  cvControl = list(V = 2),
  innerCvControl = list(list(V = 2),
                        list(V = 2))
)
# plot the results
plot(cv_sl)

