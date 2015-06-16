MODEL_TYPE_REGRESSION = "regression"
MODEL_TYPE_CLASSIFICATION = "classification"


#-------------------------------------------------------------------------------
#	���f���̈Ⴂ���z������A�_�v�^�[�N���X�̃I�u�W�F�N�g�����֐��B
#	R�̃|�����[�t�B�Y�����g�킸�A���O�ŃN���X�̏��������s���B
#
#	Args:
#		settings: model.settings�̃C���X�^���X
#
#	Value:
#		model.adapter�N���X�̃I�u�W�F�N�g
#
#	model.adapter
#		Field:
#			settings: model.settings�N���X�̃I�u�W�F�N�g
#		Methods:
#			initialize(model.settings)
#			get.model.type()
#				���f���̎�ނƃN���X�����擾
#-------------------------------------------------------------------------------
model.adapter <- function(settings){
	if (settings$is.default){
		code <- ".model.adapter.default$new()"
	} else {
		code <- sprintf(".model.adapter.%s$new()", settings$function.name)
	}
	object <- eval(parse(text = code))
	return(object)	
}

.model.adapter.default <- setRefClass(
	"model.adapter",
	fields = list(
		settings = "model.settings"
	)
)

.model.adapter.default$methods(
	get.model.type = function(cv.dummy, args.model, data){
		return(get.model.type.from.response.var(cv.dummy, args.model, data))
	}
}

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model from family.
#'
#'	This internal function detects type of model from glm and mgcv::gam family.
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type.from.family = function(args.model){
		# family���Ȃ�������f�t�H���g�͐��K���z�Ȃ̂ŁA��A�B
		if (is.null(args.model$family)){
			return(MODEL_TYPE_REGRESSION)
		}
		# family�𕶎���ɕϊ��B
		family <- format.family(args.model$family, type = "character")
		# glm��gam�̈ȉ���family�����ʁB����ȊO�͉�A
		classification.families <- c(
			"binomial", "quasibinomial", "negbin", "ocat", "nb",
			"betar", "cox.ph"
		)
		if (family %in% classification.families){
			return(MODEL_TYPE_CLASSIFICATION)
		}
		return(MODEL_TYPE_REGRESSION)
	}
)

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model from class of response variable.
#'
#'	This internal function detects type of model from class of response variable.
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type.from.response.var = function(cv.dummy, args.model, data){
		if (is(get.response.var(cv.dummy, data, args.model), "factor")){
			return(MODEL_TYPE_CLASSIFICATION)
		} else {
			return(MODEL_TYPE_REGRESSION)
		}
	}
)

#-------------------------------------------------------------------------------
#'	Modify settings of modeling.
#'
#'	Internal function that hodifis parameters used for modeling.
#'
#'	@details
#'	\describe{
#'		\item{\code{\link[e1071]{svm}}}{
#'			This function set \emph{probability} in \emph{args.model} to TRUE.
#'		}
#'		\item{\code{\link[gbm]{gbm}}}{
#'			If the maximum value of n.trees specified in \emph{args.predict}
#'			is larger than the value of n.trees specified in \emph{args.model},
#'			this function change the value of n.trees in \emph{args.model} to the
#'			maximum value.
#'		}
#'		\item{
#'			merMod object created by \code{\link[lme4]{lmer}} and 
#'			\code{\link[lme4]{glmer}} functions, \code{\link[gam]{gam}} 
#'			in \emph{gam} and \emph{mgcv} packages, \code{\link[mgcv]{gamm}}
#'		}{
#'			This function expands '.' in specified formula and makes formula
#'			without '.'.
#'		}
#'	}
#'
#'	@return A list containing modified parameters for model construction.
#-------------------------------------------------------------------------------
#	���f�������\�w�W�𐳂����v�Z����悤�ɁA���f���\�z�̈������C�����鑍�̊֐��B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	modify.args.model = function(){}
)


#-------------------------------------------------------------------------------
#'	Get formula from parameters.
#'
#'	This internal function retrieves formula from arguments used for modeling.
#'
#'	@inheritParams modify.args.model
#-------------------------------------------------------------------------------
#	���f���\�z�Ɏg����������烂�f����������킷formula���擾����B
#
#	Args:
#		object: ���f���I�u�W�F�N�g�B�v�Z�ɂ͎g���Ȃ��B
#		args.model: ���f���\�z�Ɏg�����������ꂽ���X�g�B
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn get.formula Default S3 method.
#'	This function is used for handling a result of
#'		\code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'		\code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, 
#'		\code{\link[party]{ctree}}, \code{\link[party]{cforest}}, 
#'		\code{\link[randomForest]{randomForest}}, \code{\link[gbm]{gbm}}, 
#'		\code{\link[e1071]{svm}}, \code{\link[tree]{tree}}, 
#'		\code{\link[rpart]{rpart}}, \code{\link[gam]{gam}} in \emph{gam} package,
#'		\code{\link[mgcv]{gam}} in \emph{mgcv} package and
#'		\code{\link[mgcv]{gamm}}.
#'	@method get.formula default
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.formula = function(){
		args.model <- settings$args.model
		if (!is.null(args.model$formula)){
			return(args.model$formula)
		} else {
			return(args.model[sapply(args.model, is.formula)][[1]])
		}
	}
)



