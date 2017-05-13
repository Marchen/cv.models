
test.data <- read.csv(
	"C:/Users/mic/Dropbox/おしごと/かいせき/2013.05.08 いまひたん/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)

# lm
cvlm <- cv.models(
	lm, args.model = list(
		formula = n.mizunara ~ ba+ba.mizunara+ba.konara+ba.buna+ba.sugi
	),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 1
)
cvlm
r <- get.best.models(cvlm)
r
summary(r)


# mgcv::gam
cvgam <- cv.models(
	gam, args.model = list(formula = n.mizunara ~ ba+ ba.mizunara,family=poisson),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 4
)
cvgam
r <- get.best.models(cvgam)
r
summary(r)
cvgam <- cv.models(
	gam, args.model = list(formula = dead ~ ba+ ba.mizunara,family=binomial),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 1
)
cvgam
r <- get.best.models(cvgam)
r
summary(r)

# mgcv::gamm
cvgamm <- cv.models(
	gamm, args.model = list(
		formula = n.mizunara ~ ba+ ba.mizunara,
		family=poisson, random=list(rindou=~1)
	),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 1
)
cvgamm
r <- get.best.models(cvgamm)
r
summary(r)
cvgamm <- cv.models(
	gamm, args.model = list(formula = dead ~ ba+ ba.mizunara,family=binomial),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 4
)
cvgamm
r <- get.best.models(cvgamm)
r
summary(r)

# gam::gam
cvgam <- cv.models(
	gam, args.model = list(formula = n.mizunara ~ ba+ ba.mizunara,family=poisson),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 4, package.name = "gam"
)
cvgam
r <- get.best.models(cvgam)
r
summary(r)
cvgam <- cv.models(
	gam, args.model = list(formula = dead ~ ba+ ba.mizunara,family=binomial),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 1, package.name = "gam"
)
cvgam
r <- get.best.models(cvgam)
r
summary(r)



# lme
cvlme <- cv.models(
	lme, args.model = list(
		fixed = n.mizunara ~ ba+ba.mizunara+ba.konara+ba.buna+ba.sugi,
		random=~1|rindou
	),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 1
)
cvlme
r <- get.best.models(cvlme)
r
summary(r)

# lmer
cvlmer <- cv.models(
	lmer, args.model = list(
		formula = n.mizunara ~ ba.mizunara+ba.konara+ba.buna+ba.sugi+(1|ba)
	),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 1
)
cvlmer
r <- get.best.models(cvlmer)
r
summary(r)

# glmer
cvglmer <- cv.models(
	glmer, args.model = list(
		formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi+(1|ba),
		family = binomial
	),
	data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 1
)
cvglmer
r <- get.best.models(cvglmer)
r
summary(r)



r = randomForest(dead ~ ba * n.mizunara * n.konara,data=test.data)
r = randomForest(dead ~ ba * n.mizunara * n.konara,family=binomial,data=test.data,na.action=na.fail)
d=dredge(r)


r1=tree(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data)
r2=tree(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data2)
plot(predict(r1),predict(r2)[,2])

r1=rpart(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data)
r2=rpart(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data2)
plot(predict(r1),predict(r2)[,2])

set.seed(1)
r1=randomForest(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data)
set.seed(1)
r2=randomForest(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data2)
plot(predict(r1), predict(r2, type="prob")[,2])

set.seed(1)
r1=cforest(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data)
set.seed(1)
r2=cforest(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data2)
plot(predict(r1),do.call(rbind, predict(r2, type="prob"))[,2])

set.seed(1)
r1=ctree(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data)
set.seed(1)
r2=ctree(formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data2)
plot(predict(r1),do.call(rbind, predict(r2, type="prob"))[,2])
r2=ctree(formula = rindou ~ ba.mizunara+ba.konara+ba.buna+ba.sugi,data = test.data2)
plot(predict(r1),do.call(rbind, predict(r2, type="prob"))[,2])

cvcforest <- cv.models(
	cforest,
	args.model = list(
		formula = rindou ~ ba + ba.mizunara + ba.konara + ba.buna + ba.sugi
	),
	data = test.data2, cv.metrics = c("auc", "mse", "rmse", "informedness"),
	n.cores = 4
)
r <- get.best.models(cvcforest)


cvrforest <- cv.models(
	randomForest,
	args.model = list(formula = windthrow~ELV+SLP+TPI_200+TPI_2000+stem_L+Hd+stem_PL+age+W_MAX+W_HIS+Region),
	data = d, cv.metrics = c("threshold","auc", "mcc","informedness"),
	n.cores = 4
)



r=svm(formula = dead ~ ba + n.mizunara + n.konara, data = test.data2, probability=T)
		
#
#past.fit <- gbm.models(formula = exist~.,
						 #past_dataset, 
						 #interaction.depth=c(1,2,5,10),
						 #n.minobsinnode=c(1,2,5,10),
						 #bag.fraction=c(0.5,0.75),
						 #class.stratify.cv=FALSE,
						 #cv.folds=nrow(past_dataset),
						 #n.cores = 4, # the number of cores of CPU to use for the computation
						 #random.seed = 12345
#)
#past.fit2 <- gbm.models(formula = exist~.,
						 #past_dataset, 
						 #interaction.depth=c(1,2,5,10),
						 #n.minobsinnode=c(1,2,5,10),
						 #bag.fraction=c(0.5,0.75),
						 #class.stratify.cv=FALSE,
						 #cv.folds=nrow(past_dataset),
						 #n.cores = 4, # the number of cores of CPU to use for the computation
						 #random.seed = 123456
#)
#



# 作業ディレクトリ設定
setwd("C:/Users/mic/Dropbox/おしごと/かいせき")
setwd("/Volumes/Mac Data/Dropbox/おしごと/かいせき/")
setwd("D:/mic/SyncPecol/かいせき/")

# 自分の関数類を読み込み
setwd("0000.00.00 Shared")
source("include.r", encoding = "CP932")
setwd("../2013.05.08 いまひたん")
#source("gbm_functions.r")
source("gbm_functions_archive.r")

library(gbm)
library(caret)

# データ読み込み
dat.all <- df.manager$new("data/imahi.analyze.csv")
r <- glm(dead ~ ba.mizunara, data = dat.all$df, family=binomial)
library(ROCR)
library(pROC)
coords.ret <- c(
	"threshold", "specificity", "sensitivity", "accuracy",
	"tn", "tp", "fn", "fp", "npv", "ppv",
	"1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"
)
Rprof()
proc.roc <- roc(dat.all$df$dead, predict(r, type="response"))
proc.coords <- coords(proc.roc, x = "best", best.method = "youden", ret=coords.ret)
rocr.prediction <- prediction(predict(r, type="response"), dat.all$df$dead)
rocr.preformance <- prediction(rocr.prediction, "tpr")
Rprof(NULL)





test <- setRefClass("test",methods = list(test=function(x)x*2))




glmmML2 <- function (formula, family = binomial, data, cluster, ...){
	browser()
	args <- list(formula = formula, family = family, data = data, cluster = as.name(cluster), ...)
#	args$cluster <- as.expression(args$cluster)
	do.call(glmmML, args)
	
	
}

glmmML2(Species ~ Sepal.Length, cluster = "Random", family = binomial, data = iris)



a<-args.model(Species ~ Sepal.Length, cluster = Random, family = binomial, data = iris)


test <- function(arg1, arg){
	browser()
}
args.model <- substitute(args.model)
args.model$cluster <- as.name("Random")
args.model<-eval(args.model)

test(arg = glm(x~y, data=2))




model.perf <- function(model, test.data){
	a <- deparse(model)
	return(a)
}


rc1 <- setRefClass(
	"rc1",
	fields = list(
		a = "character"
	)
)

rc2 <- setRefClass(
	"rc2",
	fields = list(
		b = "character",
		rc1 = "rc1"
	),
	methods = list(
		set.a = function(){rc1$a <<- "baka"},
		initialize = function(...){
			# 関数を割り当て
			browser()
#			.self$usingMethods(.refClassDef$methods())
			.self$usingMethods("set.a")
		}
	)
)

r1 <- rc1$new(a = "aho")
r2 <- rc2$new(rc1 = r1)

library(parallel)
cl <- makeCluster(2)
x <- list(r2, r2, r2, r2)
clusterExport(cl, c("rc1", "rc2"))
clusterApply(cl, x, function(x){
#	return(x$set.a)
	x$set.a()
	return(x$rc1$a)
})




model.perf <- function(model, test.data){
	a <- deparse(model)
	return(a)
}


rc1 <- setRefClass(
	"rc1",
	fields = list(
		a = "character"
	)
)

rc2 <- setRefClass(
	"rc2",
	fields = list(
		b = "character",
		rc1 = "rc1"
	),
	methods = list(
		set.a = function(){rc1$a <<- "baka"},
		initialize = function(...){
			.self$usingMethods('set.a')
		}
	)
)

r1 <- rc1$new(a = "aho")
r2 <- rc2$new(rc1 = r1)
r1.1 <- rc1$new(a = "aho")


library(parallel)
if (!is.null(cl)) cl <- makeCluster(4)
temp <- list(r2, r2, r2, r2)
fn <- function(x){
	x$set.a()
	x$rc1$a
}
clusterApply(cl, temp, fn)


data(iris)
r <- lm(Sepal.Length~Sepal.Width,data=iris)

x <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), by = 0.01)

pred <- predict(r, newdata=data.frame(Sepal.Width=x), se.fit = TRUE, interval = "confidence")

coef(r)

cf <- summary(r)$coefficients
cf["(Intercept)", "Estimate"] + qnorm(0.975) * cf["(Intercept)", "Std. Error"] + 
(cf["Sepal.Width", "Estimate"] +  qnorm(0.975) * cf["Sepal.Width", "Std. Error"]) * x

plot(x, pred$fit[, "lwr"])