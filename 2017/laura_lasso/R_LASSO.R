RAWmisc::InitialiseProject(
  HOME = "/analyses/code_major/laura_lasso/",
  RAW = "/analyses/data_raw/laura_lasso/",
  CLEAN = "/analyses/data_clean/laura_lasso",
  BAKED = "/analyses/results_baked/laura_lasso/",
  FINAL = "/analyses/results_final/laura_lasso/",
  SHARED = "/dropbox/results_shared/laura_lasso/")


#Big model with 11 dummy variables

data <- haven::read_dta(file.path(RPROJ$RAW,"wide_ESBL.dta"))
d <- na.omit(as.matrix(data[,c("case", "sex", "betalaktam_pen", "alder_u16", "alder_65", "alder_16_64", "travel_Thai_no_hosp", "travel_Thai_hosp", "travel_Asia_no_Thai", "anitibi", "innlagt_Norge", "hud_udstyr", "reist_asiab", "q9_rejThailandb" )]))
y <- d[,1]
x <- d[,-1]

# LASSO REGRESSION
gfit = glmnet::cv.glmnet(x,y,standardize=T, nfold=200, family="binomial")
plot(gfit)

# LOG ODDS RATIO (PENALISED)
coef(gfit)

# LOG ODDS RATIO (NORMAL LOGISTIC REGRESSION)
glm(case~reist_asiab,data=data, family="binomial")



lambda = gfit$lambda.min/10

beta = coef(gfit,s=lambda,exact=TRUE)
beta
selectiveInference::fixedLassoInf(x,y,beta,lambda*nrow(x),type="partial",family="binomial", alpha=0.05)




set.seed(43)
n = 50
p = 10
sigma = 1

x = matrix(rnorm(n*p),n,p)
x=scale(x,TRUE,TRUE)

beta = c(3,2,rep(0,p-2))
y = x%*%beta + sigma*rnorm(n)

# first run glmnet
gfit = glmnet::glmnet(x,y,standardize=FALSE)

# extract coef for a given lambda; note the 1/n factor!
# (and we don't save the intercept term)
lambda = .8
beta = coef(gfit, s=lambda/n, exact=TRUE)[-1]

# compute fixed lambda p-values and selection intervals
out = selectiveInference::fixedLassoInf(x,y,beta,lambda,sigma=sigma)
out