## ----opts, eval = TRUE, echo = FALSE, message = FALSE--------------------
options(width = 60)


## ----simulating_data-----------------------------------------------------
# set a seed for reproducibility
set.seed(212)
n <- 5000
A0 <- rbinom(n, size = 1, p = 0.5)
L1 <- rnorm(n, mean = A0 + 1, sd = 1)
A1 <- rbinom(n, size = 1, p = plogis(-1 + L1 + A0))
Y <- rnorm(n, mean = 1 + A1 + 2 * L1, 1)


## ----fit_wrong_Reg-------------------------------------------------------
# fit a regression of Y ~ A1 + L1 + A0
fit <- glm(Y ~ A1 + L1 + A0)
# show results
fit


## ----get_coef, echo = FALSE, eval = TRUE---------------------------------
betas <- round(as.numeric(fit$coef),2)


## ----estimate_reg--------------------------------------------------------
# full data.frame
full_data <- data.frame(A0 = A0, L1 = L1, A1 = A1, Y = Y)
# subset data to observations with A0 = 1 & A1 = 1
data_11 <- subset(full_data, A0 == 1 & A1 == 1)
# fit regression of Y ~ L1
fit_11 <- glm(Y ~ L1, data = data_11)
fit_11


## ----get_coef11, echo = FALSE, eval = TRUE-------------------------------
betas11 <- round(as.numeric(fit_11$coef),2)


## ----estimate_reg2-------------------------------------------------------
# get predicted value for everyone
full_data$Q2n_11 <- predict(fit_11, newdata = full_data)
# subset data to observations with A0 = 1
data_1 <- subset(full_data, A0 == 1)
# fit regression
fit_1 <- glm(Q2n_11 ~ 1, data = data_1)
# intercept is estimate of E[Y(1,1)]
fit_1


## ----gcomp_ex------------------------------------------------------------
# subset data to observations with A0 = a0 & A1 = a1

# fit regression of Y ~ L1 in A0/A1 subset data

# get predicted value for everyone

# subset data to observations with A0 = a0

# fit intercept-only regression in A0 subset data

# intercept is estimate of E[Y(a0,a1)]


## ----gcomp_sol-----------------------------------------------------------
cfmean_gcomp <- function(a0, a1, full_data){
	# subset data to observations with A0 = a0 & A1 = a1
	data_a0a1 <- subset(full_data, A0 == a0 & A1 == a1)
	# fit regression of Y ~ L1 in A0/A1 subset data
	fit_a0a1 <- glm(Y ~ L1, data = data_a0a1)
	# get predicted value for everyone
	full_data$Q2n_a0a1 <- predict(fit_a0a1, newdata = full_data)
	# subset data to observations with A0 = a0
	data_a0 <- subset(full_data, A0 == a0)
	# fit intercept-only regression in A0 subset data
	fit_a0 <- glm(Q2n_a0a1 ~ 1, data = data_a0)
	# intercept is estimate of E[Y(a0,a1)]
	return(as.numeric(fit_a0$coefficients))
}
# evaluate the function 
EY11_gcomp <- cfmean_gcomp(a0 = 1, a1 = 1, full_data)
EY10_gcomp <- cfmean_gcomp(a0 = 1, a1 = 0, full_data)
EY01_gcomp <- cfmean_gcomp(a0 = 0, a1 = 1, full_data)
EY00_gcomp <- cfmean_gcomp(a0 = 0, a1 = 0, full_data)


## ----gcomp_sol2----------------------------------------------------------
# should be ~ 6, 5, 4, 3
round(c(EY11_gcomp, EY10_gcomp, EY01_gcomp, EY00_gcomp), 2)


## ----iptw_comp_fn--------------------------------------------------------
cfmean_iptw <- function(a0, a1, full_data){
	# subset data to observations with A0 = a0
	data_a0 <- subset(full_data, A0 == a0)
	# fit logistic regression of I(A1 = a1) ~ L1 in a0 subset
	ps_a1 <- glm(I(A1 == a1) ~ L1, data = data_a0, family = binomial())
	# get predicted value for everybody 
	full_data$phat_a1 <- predict(ps_a1, newdata = full_data, 
	                             type = 'response')
	# fit regression of I(A0 = a0) ~ 1 in full_data
	ps_a0 <- glm(I(A0 == a0) ~ 1, data = full_data, family = binomial())
	# get predicted value for everybody 
	full_data$phat_a0 <- predict(ps_a0, newdata = full_data, 
	                             type = 'response')
	# compute iptw estimator
	EYa0a1 <- with(full_data, mean( 
      as.numeric(A0 == a0) * as.numeric(A1 == a1) / (phat_a0 * phat_a1) * Y 
    ))
	# intercept is estimate of E[Y(a0,a1)]
	return(EYa0a1)
}


## ----iptw_comp-----------------------------------------------------------
# evaluate the function 
EY11_iptw <- cfmean_iptw(a0 = 1, a1 = 1, full_data)
EY10_iptw <- cfmean_iptw(a0 = 1, a1 = 0, full_data)
EY01_iptw <- cfmean_iptw(a0 = 0, a1 = 1, full_data)
EY00_iptw <- cfmean_iptw(a0 = 0, a1 = 0, full_data)
# should be ~ 6,5,4,3
round(c(EY11_iptw, EY10_iptw, EY01_iptw, EY00_iptw),2)


## ----ltmle_data----------------------------------------------------------
# set seed for reproducibility & set sample size of 500
set.seed(212); n <- 500
# baseline variables
L0 <- data.frame(L01 = rnorm(n), L02 = rbinom(n, 1, 0.5))
# first treatment
gA0 <- plogis(0.2 * L0$L01 - 0.2 * L0$L02)
A0 <- rbinom(n = n, size = 1, prob = gA0)
# intermediate variable at time 1
L1 <- rnorm(n = n, mean = -A0 + L0$L01 - L0$L02, sd = 1)
# second treatment decision
gA1 <- plogis(0.2 * A0 - L1 + L0$L01)
A1 <- rbinom(n = n, size = 1, prob = gA1)
# intermediate variable at time 2
L2 <- rnorm(n = n, mean = -A0*A1 + 2*A1 - L0$L01 + L1, sd = 2)
# third treatment decision
gA2 <- plogis(A0 - A1 + 2*A0*A1 - L0$L01 + 0.2 * L1*L0$L02)
A2 <- rbinom(n = n, size = 1, prob = gA2)
# outcome
Y <- rnorm(n = n, mean = L0$L01 * L0$L02 * L2 - A0 - A1 - A2*A0*L2, sd = 2)
# put into a data frame
full_data <- data.frame(L0, A0 = A0, L1 = L1, 
                        A1 = A1, L2 = L2, A2 = A2, Y = Y)


## ----head_data-----------------------------------------------------------
head(full_data)


## ----echo = FALSE, eval = TRUE-------------------------------------------
compute_truth <- function(n = 1e5, a0 = 1, a1 = 1, a2 = 1){
	set.seed(212)
	L0 <- data.frame(L01 = rnorm(n), L02 = rbinom(n, 1, 0.5))
	A0 <- rep(a0, n)
	L1 <- rnorm(n = n, mean = -A0 + L0$L01 - L0$L02, sd = 1)
	A1 <- rep(a1, n)
	L2 <- rnorm(n = n, mean = -A0*A1 + 2*A1 - L0$L01 + L1, sd = 2)
	A2 <- rep(a2, n)
	# outcome
	Y <- rnorm(n = n, mean = L0$L01 * L0$L02 * L2 - A0 - A1 - A2*A0*L2, sd = 2)
	# put into a data frame
	return(mean(Y))
}


## ----load_drtmle, eval = TRUE, echo = FALSE, message = FALSE-------------
library(ltmle); library(SuperLearner)


## ----simple_call_to_ltmle, echo = TRUE, eval = TRUE, message = FALSE, warning = FALSE----
set.seed(123)
ltmle_fit1 <- ltmle(
    data = full_data, 
    Anodes = c("A0", "A1", "A2"),
    Lnodes = c("L01","L02","L1","L2"),
    Ynodes = "Y",
    SL.library = list(Q = c("SL.earth", "SL.glm", "SL.mean"),
                      g = c("SL.earth", "SL.glm", "SL.mean")),
    stratify = FALSE, abar = list(treatment = c(1,1,1),
                                  control = c(0,0,0))
    )


## ----ltmle_sum-----------------------------------------------------------
summary(ltmle_fit1)	


## ----look_at_sl_weights--------------------------------------------------
# weights for outcome regressions, because we set stratify = FALSE, the output in 
# ltmle_fit1$fit$Q[[1]] is the same as in ltmle_fit1$fit$Q[[2]]
ltmle_fit1$fit$Q[[1]]


## ----look_at_sl_weights2-------------------------------------------------
# weights for propensity scores, because we set stratify = FALSE, the output in 
# ltmle_fit1$fit$g[[1]] is the same as in ltmle_fit1$fit$g[[2]]
ltmle_fit1$fit$g[[1]]


## ----echo = FALSE--------------------------------------------------------
tmp <- summary(ltmle_fit1)
EY1 <- tmp$effect.measures$treatment$estimate
EY1_ci <- tmp$effect.measures$treatment$CI
EY0 <- tmp$effect.measures$control$estimate
EY0_ci <- tmp$effect.measures$control$CI


## ----echo = FALSE--------------------------------------------------------
w1 <- formatC(ltmle_fit1$fit$Q[[1]][[1]][,2], digits = 2, format = "f")
w2 <- formatC(ltmle_fit1$fit$Q[[1]][[2]][,2], digits = 2, format = "f")
w3 <- formatC(ltmle_fit1$fit$Q[[1]][[3]][,2], digits = 2, format = "f")


## ----ltmle_cens_data-----------------------------------------------------
set.seed(12)
# censoring prior to time 1 (1 = censored)
gC1 <- plogis(-2 + 0.05 * L0$L01)
C1 <- rbinom(n = n, size = 1, prob = gC1)
# censoring prior to time 2 (1 = censored)
gC2 <- plogis(-3 + 0.05 * A0 + 0.025 * L1 - 0.025 * L0$L02)
C2 <- rbinom(n = n, size = 1, prob = gC2)
# censoring prior to time 3 (1 = censored)
gC3 <- plogis(-3.5 + 0.05*A0*A1 - 0.025*L2 + 0.025 * L1)
C3 <- rbinom(n = n, size = 1, prob = gC3)
# make a cumulative indicator of censoring
anyC1 <- C1 == 1; anyC2 <- C1 == 1 | C2 == 1 
anyC3 <- C1 == 1 | C2 == 1 | C3 == 1
# censored data set
cens_data <- data.frame(L0, A0 = A0, 
               C1 = BinaryToCensoring(is.censored = C1),
               L1 = ifelse(anyC1, NA, L1), A1 = ifelse(anyC1, NA, A1), 
               C2 = BinaryToCensoring(is.censored = ifelse(anyC1, NA, C2)),
               L2 = ifelse(anyC2, NA, L2), A2 = ifelse(anyC2, NA, A2), 
               C3 = BinaryToCensoring(is.censored = ifelse(anyC2, NA, C3)),
               Y = ifelse(anyC3, NA, Y))


## ----look_ltmle_cens_data------------------------------------------------
head(cens_data, 9)


## ----simple_call_to_ltmle2, echo=TRUE, eval=TRUE, results='hide', message=FALSE, warning=FALSE----
set.seed(123)
ltmle_fit2 <- ltmle(
    data = cens_data, 
    Anodes = c("A0", "A1", "A2"),
    Lnodes = c("L01","L02","L1","L2"),
    Cnodes = c("C1","C2","C3"),
    Ynodes = "Y",
    SL.library = list(Q = c("SL.earth", "SL.glm", "SL.mean"),
                      g = c("SL.earth", "SL.glm", "SL.mean")),
    stratify = FALSE, abar = list(treatment = c(1,1,1),
                                  control = c(0,0,0))
    )


## ----ltmle_sum2----------------------------------------------------------
summary(ltmle_fit2)


## ----define_rule---------------------------------------------------------
rule1 <- function(pt_data){
	# all patients start on control
	A0 <- 0
	# patients get treatment at time 1 if L1 > -1
	# set patients with missing L1 to NA
	if(!is.na(pt_data$L1)){
		A1 <- ifelse(pt_data$L1 > -1, 1, 0)
	}else{
		A1 <- NA
	}
	# patients get treatment at time 2 if L2 > -1
	# set patients with missing L2 to NA
	if(!is.na(pt_data$L1)){
		A2 <- ifelse(pt_data$L2 > -1, 1, 0)
	}else{
		A2 <- NA
	}
	return(c(A0,A1,A2))
}


## ----define_rule2--------------------------------------------------------
rule2 <- function(pt_data){
	# all patients start on control
	A0 <- 0
	# and stay on control unless censored
	A1 <- ifelse(is.na(pt_data$L1), NA, 0)
	A2 <- ifelse(is.na(pt_data$L2), NA, 0)
	return(c(A0,A1,A2))
}


## ----simple_call_to_ltmle3, echo=TRUE, eval=TRUE, results='hide', message=FALSE, warning=FALSE----
set.seed(123)
ltmle_fit3 <- ltmle(
    data = cens_data, 
    Anodes = c("A0", "A1", "A2"),
    Lnodes = c("L01","L02","L1","L2"),
    Cnodes = c("C1","C2","C3"),
    Ynodes = "Y", stratify = FALSE, 
    SL.library = list(Q = c("SL.earth", "SL.glm", "SL.mean"),
                      g = c("SL.earth", "SL.glm", "SL.mean")),
    rule = list(treatment = rule1, control = rule2)
    )


## ----summary_dr_ltmle----------------------------------------------------
summary(ltmle_fit3)

