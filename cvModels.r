#===============================================================================
#	���낢��ȃ��f���ɑ΂��ăN���X�o���f�[�V�����Ő��\�w�W���v�Z����B
#
#	���̂Ƃ���̃N���X�o���f�[�V�����Ή��֐�
#		lm, glm, lme, lmer, glmer, ctree, cforest, randomForest, gbm, svm, tree,
#		rpart, mgcv::gam, mgcv::gamm, gam::gam
#
#	���̂Ƃ���̃p�����[�^�[�`���[�j���O�Ή��֐�
#		gbm (shrinkage, interaction.depth, n.minobsinnode, bag.fraction, n.trees)
#		randomForest (mtry)
#
#	�Ή��s�\�֐�
#		MCMCglmm�ipredict��newdata�ɑΉ����ĂȂ��j
#		glmmML�ipredict���Ȃ��j
#
#	�e�֐��̒��ӓ_
#		�S��:
#			predict�֐���type�͉����ϐ������q�^����"prob"�ɁA����ȊO����
#			"response"�Ɏ����I�ɏ����������܂��B
#		lme:
#			predict�����܂�NA��Ԃ��̂ŁAmse�Ƃ�rmse���v�Z�ł��Ȃ������B
#			�v�Z��na.omit����ꂿ�Ⴄ�H
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
#			predict���邽�߂ɁApredict.gamm�֐����`���Ă��܂��B
#
#	�V�����֐��ւ̑Ή�
#		�Eget.package.name()�֐���get.class.name()�֐���V�����֐��ɑΉ�������B
#		�E�K�v�Ȃ�get.response.name(), get.data(), format.prediction(),
#		  modify.args.predict()�֐��������B
#		�E�p�����[�^�[�`���[�j���O�ɑΉ�����ɂ́Aget.tunable.args()�֐��������B
#		�E�K�v�Ȃ�print��summary���\�b�h�ɑΉ��������B
#
#	TODO:
#		������
#			�Eglm�Ƃ��̉����ϐ���cbind�̂Ƃ�
#			�E���ʖ��̎������Aconfusion�}�g���N�X���v�Z����B
#				��get.problem(function.name, args.model)�֐�
#		����
#			�Eget.positive.class�֐��̌x�����b�Z�[�W��check.args()�֐��ֈړ��B
#			�Edredge & stepAIC
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

#-------------------------------------------------------------------------------
#	�\�[�X�ǂݍ���
#-------------------------------------------------------------------------------

# gamm��predict�ɑΉ�������
source(file.path(get.this.file.dir(), "R", "predict.gamm.r"), encoding = "CP932")

# �֐��̈Ⴂ���z������֐��Q
source(file.path(get.this.file.dir(), "R", "get.response.name.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.response.class.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.response.var.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.formula.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.tunable.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "expand.tunable.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "merge.tunable.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "format.prediction.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "detect.model.type.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "make.dummy.r"), encoding = "CP932")

# �p�����[�^�[�̐������𒲐�����֐��Q
source(file.path(get.this.file.dir(), "R", "modify.args.predict.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "modify.args.model.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "modify.response.var.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "modify.args.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "expand.dot.r"), encoding = "CP932")


# �N���X���E�p�b�P�[�W�����擾����֐��Q
source(file.path(get.this.file.dir(), "R", "get.class.name.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.package.name.r"), encoding = "CP932")

# �N���X�^�[�֘A�֐��Q
source(file.path(get.this.file.dir(), "R", "cluster.r"), encoding = "CP932")

# ���f�����\�]���w�W�̌v�Z�֐��Q
source(file.path(get.this.file.dir(), "R", "cv.performance.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "confusion.matrix.r"), encoding = "CP932")

# �N���X�o���f�[�V�����֘A�֐��Q
source(file.path(get.this.file.dir(), "R", "make.cv.group.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.positive.prob.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cv.one.fold.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cross.validation.r"), encoding = "CP932")

# �p�����[�^�[�I���֘A�֐��Q
source(file.path(get.this.file.dir(), "R", "run.dredge.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cv.models.r"), encoding = "CP932")

# �x�X�g���f���I���֐��Q
source(file.path(get.this.file.dir(), "R", "which.min.max.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.best.metrics.index.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "construct.model.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "get.best.models.r"), encoding = "CP932")
source(file.path(get.this.file.dir(), "R", "cv.best.models.r"), encoding = "CP932")

