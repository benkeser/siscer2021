## ----opts, eval = TRUE, echo = FALSE, message = FALSE--------------------
options(width = 60)


## ----simulating_data-----------------------------------------------------
# set a seed for reproducibility
set.seed(212)
# sample size
n <- 300
# W1 has Normal distribution, W2 has Uniform distribution
W1 <- rnorm(n); W2 <- runif(n)
# make a data.frame of covariates
W <- data.frame(W1 = W1, W2 = W2)
# pr(A = 1 | W) is logistic linear in W
g1W <- plogis(-1 + W1 - W2 + W1*W2)
# generate binary treatment 
A <- rbinom(n, 1, g1W)
# E[Y | A, W] is logistic linear in A, W
QAW <- plogis(W1 - W2 + A)
# generate outcome by adding random error
Y <- rbinom(n, 1, QAW)


## ----load_drtmle, eval = TRUE, echo = FALSE, message = FALSE-------------
library(drtmle); library(SuperLearner)


## ----simple_call_to_drtmle, echo = TRUE, eval = FALSE, warning = FALSE----
## set.seed(123)
## fit1 <- drtmle(W = W, A = A, Y = Y, a_0 = c(0,1), family = binomial(),
##                SL_g = c("SL.earth", "SL.glm"),
##                SL_Q = c("SL.earth", "SL.glm"),
##                SL_gr = c("SL.earth", "SL.glm"),
##                SL_Qr = c("SL.earth", "SL.glm"),
##                stratify = FALSE)
## fit1


## ----simple_call_to_drtmle, echo = FALSE, eval = TRUE, message = FALSE----
set.seed(123)
fit1 <- drtmle(W = W, A = A, Y = Y, a_0 = c(0,1), family = binomial(),
               SL_g = c("SL.earth", "SL.glm"),
               SL_Q = c("SL.earth", "SL.glm"),
               SL_gr = c("SL.earth", "SL.glm"),
               SL_Qr = c("SL.earth", "SL.glm"),
               stratify = FALSE)
fit1


## ----ci1-----------------------------------------------------------------
ci(fit1)


## ----ci2-----------------------------------------------------------------
ci(fit1, contrast = c(-1, 1))


## ------------------------------------------------------------------------
riskRatio <- list(f = function(eff){ log(eff) },
                  f_inv = function(eff){ exp(eff) },
                  h = function(est){ est[1]/est[2] },
                  fh_grad =  function(est){ c(1/est[1],-1/est[2]) })
ci(fit1, contrast = riskRatio)


## ------------------------------------------------------------------------
logitMean <- list(f = function(eff){ qlogis(eff) },
                  f_inv = function(eff){ plogis(eff) },
                  h = function(est){ est[1] },
                  fh_grad = function(est){ c(1/(est[1] - est[1]^2), 0) })
ci(fit1, contrast = logitMean)


## ------------------------------------------------------------------------
wald_test(fit1, null = c(0.5, 0.6))


## ------------------------------------------------------------------------
# test ATE = 0
wald_test(fit1, contrast = c(1, -1))
# test ATE = 0.1
wald_test(fit1, contrast = c(1, -1), null = 0.1)


## ------------------------------------------------------------------------
wald_test(fit1, contrast = riskRatio, null = 1)


## ----silly, echo = FALSE, eval = TRUE------------------------------------
grbg_fit <- drtmle(W = W, A = A, Y = Y, a_0 = c(0,1), family = binomial(),
               SL_Q = "SL.glm",
               SL_g = c("SL.glm","SL.glm","SL.mean"),
               SL_Qr = "SL.glm",
               SL_gr = "SL.mean",
               stratify = FALSE)


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


## ----my_own_regression---------------------------------------------------
SL.screened_regression <- function(Y, X, newX, family, ...){
	# screen columns of X using screen_confounders
	include_cols <- screen_confounders(Y = Y, X = X, family = family)
	# fit main terms glm with only those columns
	fitted_glm <- glm(Y ~ ., data = X[ , include_cols], family = family)
	# get predictions
	pred <- predict(fitted_glm, newdata = newX[ , include_cols], 
	                type = "response")
	# format output
	out <- list(fit = list(fitted_model = fitted_glm, 
	                       include_cols = include_cols),
				pred = pred)
	# assign class
	class(out$fit) <- "SL.screened_regression"
	return(out)
}


## ----predict_method------------------------------------------------------
predict.SL.screened_regression <- function(object, newdata, ...){
	pred <- predict(object$fitted_model, 
	                newdata = newdata[ , object$include_cols], 
	                type = "response")
	return(pred)
}


## ----custom_drtmle_fit, message=FALSE, warning=FALSE---------------------
set.seed(123)
fit2 <- drtmle(W = W, A = A, Y = Y, a_0 = c(0,1), family = gaussian(),
               # specify main terms logistic regression via glm_g
               glm_g = "W1 + W2",
               # specify custom outcome regression via SL_Qs
               SL_Q = "SL.screened_regression",
               # the residual regression stay the same
               SL_gr = c("SL.earth", "SL.glm", "SL.mean"),
               SL_Qr = c("SL.earth", "SL.glm", "SL.mean"),
               stratify = FALSE)
fit2


## ----add_miss------------------------------------------------------------
set.seed(123)
DeltaA <- rbinom(n, 1, plogis(2 + W$W1))
DeltaY <- rbinom(n, 1, plogis(2 + W$W2 + A))
A[DeltaA == 0] <- NA
Y[DeltaY == 0] <- NA


## ---- cache = TRUE, eval = TRUE, echo = TRUE, message = FALSE------------
set.seed(123)
fit3 <- drtmle(W = W, A = A, Y = Y, a_0 = c(0,1), family = binomial(),
       		   SL_g = list(DeltaA = c("SL.earth", "SL.glm", "SL.mean"),
       		               A = c("SL.earth", "SL.glm", "SL.mean"),
       		               DeltaY = c("SL.glm", "SL.mean")),
               SL_Q = c("SL.earth", "SL.glm", "SL.mean"),
               SL_gr = c("SL.earth", "SL.glm", "SL.mean"),
               SL_Qr = c("SL.earth", "SL.glm", "SL.mean"),
               stratify = FALSE)


## ----show_miss_fit-------------------------------------------------------
fit3
# calls to ci and wald_test are same as before
ci(fit3)


## ---- cache = TRUE, message = FALSE, warning = FALSE---------------------
set.seed(1234)
n <- 300
W <- data.frame(W1 = runif(n), W2 = rbinom(n, 1, 0.5))
A <- rbinom(n, 2, plogis(W$W1 + W$W2))
Y <- rbinom(n, 1, plogis(W$W1 + W$W2*A))


## ---- message = FALSE, warning = FALSE-----------------------------------
fit4 <- drtmle(W = W, A = A, Y = Y, stratify = FALSE,
               SL_Q = c("SL.earth", "SL.glm"),
               SL_g = c("SL.earth", "SL.glm"),
               SL_Qr = c("SL.earth", "SL.glm"),
               SL_gr = c("SL.earth", "SL.glm"),
               family = binomial(), a_0 = c(0,1,2))


## ----show_the_fit--------------------------------------------------------
fit4


## ---- ci_and_wald--------------------------------------------------------
ci(fit4)
wald_test(fit4, null = c(0.4, 0.5, 0.6))


## ---- cache = TRUE-------------------------------------------------------
ci(fit4, contrast = c(-1, 1, 0))
ci(fit4, contrast = c(-1, 0, 1))


## ---- rr_10--------------------------------------------------------------
riskRatio_1v0 <- list(f = function(eff){ log(eff) },
                      f_inv = function(eff){ exp(eff) },
                      h = function(est){ est[2]/est[1] },
                      fh_grad =  function(est){ c(1/est[2], -1/est[1], 0) })
ci(fit4, contrast = riskRatio_1v0)


## ---- rr_20--------------------------------------------------------------
riskRatio_2v0 <- list(f = function(eff){ log(eff) },
                      f_inv = function(eff){ exp(eff) },
                      # note now comparing 3rd to 1st estimate
                      h = function(est){ est[3]/est[1] },
                      fh_grad =  function(est){ c(0, -1/est[1], 1/est[3]) })
ci(fit4, contrast = riskRatio_2v0)

