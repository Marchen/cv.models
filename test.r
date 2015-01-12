
test.data <- read.csv(
	"C:/Users/mic/Dropbox/‚¨‚µ‚²‚Æ/‚©‚¢‚¹‚«/2013.05.08 ‚¢‚Ü‚Ğ‚½‚ñ/Data/imahi.analyze.csv"
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