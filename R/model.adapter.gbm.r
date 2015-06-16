#-------------------------------------------------------------------------------
#	gbm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.gbm <- setRefClass(
	"model.adapter.gbm", contains = "model.adapter"
}

.model.adapter.gbm$methods(
	get.model.type = function(cv.dummy, args.model, data){
		# ���z���w�肳��Ă���Ƃ�
		if (!is.null(args.model$distribution)){
			# �ȉ������ʁA����ȊO�͉�A�Ƃ��Ĉ����B
			classification.families <- c(
				"bernoulli", "huberized", "multinomial", "adaboost"
			)
			if (args.model$distribution %in% classification.families){
				return(MODEL_TYPE_CLASSIFICATION)
			} else {
				return(MODEL_TYPE_REGRESSION)
			}
		}
		# ���z���w�肳��Ă��Ȃ�������Agbm�Ɠ����悤�ɐ���B
		response <- get.response.var(cv.dummy, data, args.model)
		if (nlevels(as.factor(response)) == 2){
			# 2�N���X�������环�ʁB
			return(MODEL_TYPE_CLASSIFICATION)
		}
		if (is(response, "factor")){
			# �����ϐ������q�������环�ʁB
			return(MODEL_TYPE_CLASSIFICATION)
		}
		return(MODEL_TYPE_REGRESSION)
	}
)

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.model
#'	Method for \code{\link[gbm]{gbm}} object in \emph{gbm} package.
#'	@method modify.args.model gbm
#-------------------------------------------------------------------------------
#	���f���\�z�p��n.trees��predict�p��n.trees���������Ȃ�������A
#	�����I��n.trees�𑝂₷�B
#-------------------------------------------------------------------------------
.model.adapter.gbm$methods(
	modify.args.model = function(){
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

