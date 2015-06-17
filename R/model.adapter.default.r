

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

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model.
#'
#'	This internal function detects type of model (classification/regression).
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
#	���f�������ʂȂ̂���A�Ȃ̂��𔻒肷��B
#
#	Args:
#		cv.dummy: cv.dummy�I�u�W�F�N�g
#		args.model: ���f���\�z�Ɏg����p�����[�^�[�B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type = function(){
		return(get.model.type.from.response.var())
	}
)

#-------------------------------------------------------------------------------
#'	(Internal) Detect type of model from family.
#'
#'	This internal function detects type of model from glm and mgcv::gam family.
#'
#'	@inheritParams modify.args.model
#'	@return a character "classification" or "regression".
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type.from.family = function(){
		# family���Ȃ�������f�t�H���g�͐��K���z�Ȃ̂ŁA��A�B
		if (is.null(settings$args.model$family)){
			return("regression")
		}
		# family�𕶎���ɕϊ��B
		family <- format.family(settings$args.model$family, type = "character")
		# glm��gam�̈ȉ���family�����ʁB����ȊO�͉�A
		classification.families <- c(
			"binomial", "quasibinomial", "negbin", "ocat", "nb",
			"betar", "cox.ph"
		)
		if (family %in% classification.families){
			return("classification")
		}
		return("regression")
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
	get.model.type.from.response.var = function(){
		if (is(get.response.var(), "factor")){
			return("classification")
		} else {
			return("regression")
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
#	lme�����A�ʏ����B
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

#-------------------------------------------------------------------------------
#'	(Internal) Expand dot in formula.
#'
#'	This internal function expand dot ('.') in model formula used for modeling.
#'
#'	@inheritParams modify.args.model
#'	@param specials A vector of character which passed to 
#'	\code{\link[stats]{terms.formula}} function.
#-------------------------------------------------------------------------------
#	args.model�̒���formula��.�����ۂ̕ϐ��ɒu�������鑍�̊֐��B
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Default S3 method. Intended to be used for \emph{lmerMod} object created by
#'	\code{\link[lme4]{lmer}} and \emph{glmerMod} object created by 
#'	\code{\link[lme4]{glmer}} function in \emph{lme4} package.
#'	@method expand.dot default
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	expand.dot = function(specials = NULL){
		# ���̏���
		f <- get.formula()
		f <- terms(f, data = settings$data, specials = specials)
		attributes(f) <- NULL
		f <- as.formula(f)
		args.model <- settings$args.model
		args.model[[which(sapply(args.model, is.formula))]] <- f
		settings$args.model <<- args.model
	}
)

#-------------------------------------------------------------------------------
#'	Get name of response variable from parameters.
#'
#'	This internal function retrieves the name of the response variable in 
#'	specified parameters used for modeling.
#'
#'	@inheritParams modify.args.model
#-------------------------------------------------------------------------------
#	�����ϐ��̖��O��Ԃ����̊֐�
#	���܂̂Ƃ���A�֐��̑Ή��K�v�Ȃ��B
#
#	Args:
#		object: ���f���I�u�W�F�N�g�B
#		args.model: ���f���\�z�Ɏg�����������ꂽ���X�g�B
#
#	Value:
#		�����ϐ��̖��O��\��������B
#
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn get.response.name
#'		Default S3 method. This function handles result of \code{\link[stats]{lm}}, 
#'		\code{\link[stats]{glm}}, \code{\link[nlme]{lme}} and 
#'		\code{\link[randomForest]{randomForest}} functions. 
#'	@method get.response.name default
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.response.name = function(){
		return(as.character(get.formula())[2])
	}
)




#-------------------------------------------------------------------------------
#'	Get response variable from parameters.
#'
#'	This internal function retrieves  the response variable in specified 
#'	parameters used for modeling.
#'
#'	@inheritParams modify.args.model
#'	@param data data used for modeling.
#-------------------------------------------------------------------------------
#	�����ϐ���Ԃ��B���̂Ƃ���A�֐��ւ̑Ή��K�v�Ȃ��B
#
#	Args:
#		object: ���f���I�u�W�F�N�g
#		data: ���f���\�z�Ɏg����f�[�^�B
#		args.model: ���f���\�z�Ɏg����p�����[�^�[�B
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#'	@describeIn get.response.var
#'	@method get.response.var default Default S3 method.
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.response.var = function(){
		return(data[[get.response.name()]])
	}
)

