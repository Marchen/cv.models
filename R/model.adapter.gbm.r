#-------------------------------------------------------------------------------
#'	model.adapter class for gbm
#'
#'	This reference class contains methods for \code{\link[gbm]{gbm}} in 
#'	\emph{gbm} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gbm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.gbm <- setRefClass(
	"model.adapter.gbm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.gbm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		# ���z���w�肳��Ă���Ƃ�
		if (!is.null(settings$args.model$distribution)){
			# �ȉ������ʁA����ȊO�͉�A�Ƃ��Ĉ����B
			classification.families <- c(
				"bernoulli", "huberized", "multinomial", "adaboost"
			)
			if (settings$args.model$distribution %in% classification.families){
				return("classification")
			} else {
				return("regression")
			}
		}
		# ���z���w�肳��Ă��Ȃ�������Agbm�Ɠ����悤�ɐ���B
		response <- get.response.var()
		if (nlevels(as.factor(response)) == 2){
			# 2�N���X�������环�ʁB
			return("classification")
		}
		if (is(response, "factor")){
			# �����ϐ������q�������环�ʁB
			return("classification")
		}
		return("regression")
	}
)

#-------------------------------------------------------------------------------
#	���f���\�z�p��n.trees��predict�p��n.trees���������Ȃ�������A
#	�����I��n.trees�𑝂₷�B
#-------------------------------------------------------------------------------
.model.adapter.gbm$methods(
	modify.args.model = function(){
		"
		If the maximum value of n.trees specified in \\emph{args.predict}
		is larger than the value of n.trees specified in \\emph{args.model},
		this function change the value of n.trees in \\emph{args.model} to the
		maximum value.
		"
		args.model <- settings$args.model
		args.predict <- settings$args.predict
		if (!is.null(args.predict$n.trees)){
			n.trees.predict <- max(args.predict$n.trees)
			n.trees.model <- ifelse(
				is.null(args.modesl$n.trees), 100, args.model$n.trees
			)
			if (n.trees.model < n.trees.predict){
				args.model$n.trees <- n.trees.predict
			}
		}
		settings$args.model.src <<- settings.args.model
		settings$args.model <<- args.model
	}
)

