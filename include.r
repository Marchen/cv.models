#===============================================================================
#	���낢��ȃ��f���ɑ΂��ăN���X�o���f�[�V�����Ő��\�w�W���v�Z����B
#
#	���̂Ƃ���̃N���X�o���f�[�V�����Ή��֐�
#		lm, glm, lme, glmmML, lmer, glmer, ctree, cforest, randomForest,
#		gbm, svm, tree, rpart, mgcv::gam, mgcv::gamm, gam::gam, ranger
#
#	���̂Ƃ���̃p�����[�^�[�`���[�j���O�Ή��֐�
#		gbm (shrinkage, interaction.depth, n.minobsinnode, bag.fraction, n.trees)
#		randomForest (mtry)
#
#	�Ή��s�\�֐�
#		MCMCglmm�ipredict��newdata�ɑΉ����ĂȂ��j
#
#	�e�֐��̒��ӓ_
#		�S��:
#			predict�֐���type�͉����ϐ������q�^����"prob"�ɁA����ȊO����
#			"response"�Ɏ����I�ɏ����������܂��B
#			�����ϐ����񍀕��z�̎��A�����ϐ���cbind�Ŏw�肷��̂ɂ͑Ή����Ă��܂���B
#		lme:
#			predict�����܂�NA��Ԃ��̂ŁAmse�Ƃ�rmse���v�Z�ł��Ȃ������B
#			�v�Z��na.omit����ꂿ�Ⴄ�H
#		glmmML:
#			predict�̓����_�����ʂ𖳎����āA�S�̂̊֌W������
#			�\���l���v�Z���Ă��܂��Bpredict�͎��O�Ŏ������Ă��܂��B
#		randomForest:
#			formula���g���ă��f�����w�肷��ꍇ�����Ή��B
#		svm:
#			formula���g���ă��f�����w�肷��ꍇ�����ɑΉ��B
#			���f���쐬���Epredict���ɂ͎����I��probability = TRUE���ݒ肳��A
#			�e�N���X�̊m�����v�Z����܂��B
#		lmer, glmer
#			�����_�����ʂ̃O���[�v��CV�̃e�X�g�f�[�^�Ɗw�K�f�[�^�ň�����Ⴄ�ƁA
#			predict�Ɏ��s����B�������������_�����ʂ͎g���Ȃ��B
#		tree, mgcv:
#			predict��type�͏��������Ă��܂���B
#		gam:
#			�f�t�H���g�ł�mgcv�p�b�P�[�W��gam�֐����Ăт܂��B
#			gam�p�b�P�[�W��gam�֐����Ăт����Ƃ��ɂ́Apackage.name = "gam"��
#			�w�肵�Ă��������B
#		gamm:
#			summary��predict�ɂ�gamm$gam�I�u�W�F�N�g���g���Ă��܂��B
#			predict���邽�߂ɁApredict.gamm�֐����`���Ă���A�\�����ɂ�
#			�����_�����ʂ͖������đS�̂̊֌W������\���l���v�Z���Ă��܂��B
#
#	�V�����֐��ւ̑Ή�
#		�Eget.package.name()�֐���get.class.name()�֐���V�����֐��ɑΉ�������B
#		�E�K�v�Ȃ�A�ȉ��̊֐���V�������f���֐��ɑΉ�������B
#			detect.model.type
#			expand.dot
#			format.prediction
#			get.formula
#			get.response.class
#			get.response.name
#			get.tunable.args
#			modify.args.model
#			modify.args.predict
#			modify.response.var
#		�E�p�����[�^�[�`���[�j���O�ɑΉ�����ɂ́Aget.tunable.args()�֐��������B
#		�E�K�v�Ȃ�print��summary���\�b�h�ɑΉ��������B
#
#	TODO:
#		������
#			�Epositive class�̔��ʂ����܂������Ă��Ȃ����ۂ��B
#			�Edredge & stepAIC
#			�Eget.positive.class�֐��̌x�����b�Z�[�W��check.args()�֐��ֈړ��B
#			�E�����ϐ��i�Ƃ��j��log�����܂����肵���Ƃ��A���܂������Ȃ����B
#				������I(x^2)�Ƃ�������ˁB
#		����
#			�Eglm�Ƃ��̉����ϐ���cbind�̂Ƃ�
#			�Edetect.model.type�͉����ϐ���cbind�̌^�̂Ƃ����܂��s���Ȃ��͂��B
#			�Emodify.args.predict.tree��rpart��type������������B
#			�Emodify.args.predict.gbm��n.trees���w�肳��Ă��Ȃ������Ƃ��̑Ή��B
#		��
#			�Egbm��randomForest��formula���g��Ȃ������p�t�H�[�}���X���悢�炵���B
#				svm��formula�g��Ȃ���Ȃ��炵���B
#			�Epredicted probability��colname
#			�Epredict.cv.best.models()
#			�Ecforest�̃`���[�j���O�Ή��H�i�v�p�����[�^�[�I���j
#			�Eplot.cv.models�֐� 2d, 3d, 4d? parsp
#			�Ecv.more�֐���merge�֐��Amore.metrics()�֐�
#
#-------------------------------------------------------------------------------
#	function		class			package
#	-----------------------------------------------
#	lm				lm				stats
#	glm				glm, lm			stats
#	lme				lme				nlme
#	glmmML			glmmML			glmmML
#	lmer			lmerMod			lme4
#	glmer			glmerMod		lme4
#	ctree			BinaryTree		party
#	cforest			RandomForest	party
#	randomForest	randomForest	randomForest
#	gbm				gbm				gbm
#	svm				svm.formula		e1071
#	tree			tree			tree
#	rpart			rpart			rpart
#	gam				gam				mgcv, gam
#	gamm			gamm			mgcv
#	ranger			ranger			ranger
#-------------------------------------------------------------------------------
#	�g�����̗�
#
#	# �܂��I�u�W�F�N�g�����
#	cvgbm <- cv.models(
#		gbm,									# �Ăяo���֐����w��
#		args.model = list(						# ���f���\�z�Ɏg���������w��
#			formula = y ~ x1 + x2 + x3,
#			distribution = "bernoulli",
#			shrinkage=c(0.1, 0.01),				# �`���[�j���O����p�����[�^�[��
#			interaction.depth = c(1, 3, 5)		# �����w��\�i�������A�Ή�������΁j
#		),
#		args.predict = list(					# predict�ɓn�������B
#			n.trees=c(1, 10, 100)				# �`���[�j���O����p�����[�^�[��
#		),										# �����w��\�i����j
#		data = test.data,						# �f�[�^�́��̈����ɓ���Ȃ�
#		cv.metrics = c(							# �v�Z���鐫�\�]���w�W������
#			"auc", "mse",
#			"rmse", "informedness"
#		),
#		cv.folds = 10,							# CV�������B�f�t�H���g��10�B
#		seed = 1,								# ���ʂ��Œ肵�����Ƃ��ɂ�seed���w��
#		n.cores = 4								# �v�Z�Ɏg���R�A�����w��
#	)
#
#	cvglm <- cv.models(
#		glm,
#		args.model = list(formula = y ~ x1 + x2 + x3, family=binomial),
#		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
#		n.cores = 1
#	)
#
#	# CV�̌��ʂ�����B
#	cvgbm
#	cvglm
#
#	# �x�X�g���f���̍쐬�B���̊֐��Ńx�X�g���f�����쐬�����B
#	best1 <- get.best.models(
#		cvgbm,										# CV���ʂ��w��
#		metrics = c("informedness","auc", "mse")	# ���f���I���w�W���w��
#													# �K��metrics = �Ŗ��O�w��B
#	)
#	summary(best1)
#
#	# ���\�w�W�Ń^�C����������\��������̂ŁA���ʂ̓I�u�W�F�N�g�����������X�g�B
#	best1[[1]]$model			# �x�X�g���f���̃��f���I�u�W�F�N�g
#	best1[[1]]$cv.metrics		# �x�X�g���f����CV�w�W
#	best1[[1]]$cv.prediction	# CV�v�Z�Ɏg�����\���l
#	best1[[1]]$cv.response		# CV�v�Z�Ɏg�������בւ��ς݉����ϐ�
#	best1[[1]]$confusion.matrix	# �w�W�v�Z�Ɏg��ꂽconfusion.matrix
#	best1[[1]]$function.name	# �ꉞ�ۑ�����Ă�֐���
#
#	# ������OK
#	best2 <- get.best.models(
#		cvgbm, cvglm, metrics = c("informedness","auc", "mse")
#	)
#	summary(best2)
#
#
#-------------------------------------------------------------------------------
#	�Ή��֐�����
#
#	gbm:
#		distribution = "bernoulli"�̂Ƃ��A�����ϐ���0 or 1��TRUE or FALSE����
#		�󂯕t���Ȃ��B���q�^�͎󂯕t���Ȃ��B
#
#	tree, rpart, randomForest:
#		0 or 1/TRUE or FALSE��A���l�ŗ\������̂ƈ��q�^�ŗ\������̂Ō��ʂ��Ⴄ
#	cforest, ctree
#		0 or 1/TRUE or FALSE��A���l�ŗ\������̂ƈ��q�^�ŗ\������̂Ō��ʂ͓���
#
#===============================================================================


#-------------------------------------------------------------------------------
#	�X�N���v�g������f�B���N�g������Ԃ��֐��B
#	http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
#-------------------------------------------------------------------------------
get.this.file.dir <- function(){
	cmdArgs <- commandArgs(trailingOnly = FALSE)
	needle <- "--file="
	match <- grep(needle, cmdArgs)
	if (length(match) > 0) {
		# Rscript
		return(dirname(sub(needle, "", cmdArgs[match])))
	} else {
		# 'source'd via R console
		return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
	}
}

library(model.adapter)
library(R6)
library(ranger)
library(gbm)
library(e1071)

#-------------------------------------------------------------------------------
#	�\�[�X�ǂݍ���
#-------------------------------------------------------------------------------

# predict��gamm��glmmML�ɑΉ�������
source(file.path(get.this.file.dir(), "R", "cluster.manager.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "cv.best.models.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "cv.group.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "classification.metrics.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "regression.metrics.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "cv.metrics.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "cv.models.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "utils.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "interface.r"), encoding = "UTF-8")
source(file.path(get.this.file.dir(), "R", "which.min.max.r"), encoding = "UTF-8")




#r <- cv.models(lm(Petal.Length ~ ., data = iris), seed = 1)
#r <- cv.models(gbm(Petal.Length ~ ., data = iris), seed = 1, n.trees=10, positive.class = "versicolor")
#f <- Species ~ .
#r <- cv.models(
	#gbm(f, data = iris, weights = iris$Petal.Length), seed = 1, n.trees = 10,
	#positive.class = "versicolor", n.cores = 1
#)


#iris2 <- droplevels(subset(iris, Species != "setosa"))
#iris2$Species <- as.numeric(iris2$Species) - 1
#r <- cv.models(glm(Species ~ ., data = iris2, family = "binomial"))

#r <- cv.models(svm(Species ~ ., data = iris2, probability = TRUE), seed = 2)
#r <- cv.models(ranger(Species ~ ., data = iris2, write.forest = TRUE), seed = 2)


#m <- gbm(Petal.Length ~ ., data = iris)
#r <- predict(m, n.trees = 1:100)
#result <- list()
#for (i in 1:100) {
	#result[[i]] <- predict(m, n.trees = i)
#}
#r <- lapply(X = 1:100, FUN = predict, object = m)

#library(cv.models)
#library(gbm)
#grid = list(interaction.depth = c(1, 2), n.minobsinnode = c(5, 10))
#grid.predict = list(n.trees = 1:10)
#r <- cv.models(
	#gbm(Petal.Length ~ ., data = iris), seed = 1, grid = grid,
	#grid.predict = grid.predict
#)

#r <- cv.models(
	#gbm(Species ~ ., data = iris), seed = 1, grid = grid,
	#grid.predict = grid.predict
#)


