#-------------------------------------------------------------------------------
#'	Make a model.adapter object.
#'
#'	This function makes an object of a derived class of \emph{model.adapter} 
#'	class that abstract differences in specifications of supported modeling 
#'	functions.
#'	Inheritance of the derived class is determined by this function using the 
#'	value of \emph{function.name} field of \emph{settings} field rather than the
#'	standard generic function mechanism of R. So, if a programer wants to add 
#'	a support for a new function, he or she needs to implement a generator 
#'	object of a Reference Class named .model.adapter.FUNCTION_NAME.
#'
#'	@param settings a \code{\link{model.settings}} object.
#'	@return
#'		an object of derived class of \code{\link{model.adapter-class}}
#'		depending on the function specified in \emph{settings}.
#-------------------------------------------------------------------------------
#	���f���̈Ⴂ���z������A�_�v�^�[�N���X�̃I�u�W�F�N�g�����֐��B
#	R�̃|�����[�t�B�Y�����g�킸�A���O�ŃN���X�̏��������s���̂ŁA�V�K�֐��ւ�
#	�Ή���ǉ����邽�߂ɂ́A.model.adapter.�֐�����Reference Class��
#	�W�F�l���[�^�[����������K�v������B
#
#	Args:
#		settings: model.settings�̃C���X�^���X
#
#	Value:
#		model.adapter�N���X�̃I�u�W�F�N�g
#
#-------------------------------------------------------------------------------
model.adapter <- function(settings){
	if (settings$is.default){
		code <- ".model.adapter.default$new(settings = settings)"
	} else {
		code <- sprintf(
			".model.adapter.%s$new(settings = settings)", settings$function.name
		)
	}
	object <- eval(parse(text = code))
	return(object)	
}

#-------------------------------------------------------------------------------
#'	Abstraction layer for model functions/objects.
#'
#'	This class encapsulates differences in specifications of statistical/machine
#'	learning functions and model objects to provide standard way to access 
#'	data and properties of models. To add support for a new modeling function,
#'	function, new generator object of a reference class inheriting this class
#'	must be implimented and methods that cannot work well with the model should
#'	be overriden. Because 
#'
#'	@field settings
#'		an object of \code{\link{model.settings}} to keep settings of the 
#'		object.
#'	@export
#-------------------------------------------------------------------------------
#	���f�����O�֐��̈Ⴂ���z������Reference Class�Amodel.adapter�N���X��
#	�W�F�l���[�^�[�I�u�W�F�N�g�B
#	���̊��N���X���p�����āA�l�X�ȃ��f���ɑΉ�������B
#	�i���܂̂Ƃ���j�p�����K�v�ȃ��\�b�h�͈ȉ��̒ʂ�B
#
#			���\�b�h��							�p���N���X�ł̊֐��ւ̑Ή�
#			get.model.type()					�K�v
#			get.model.type.from.family()		�s�v
#			get.model.type.from.response.var()	�s�v
#			modify.args.model()					�K�v
#			get.formula()						�K�v
#			expand.dot()						�K�v
#			get.response.name()					�s�v
#			get.response.var()					�s�v
#
#		Field:
#			settings: model.settings�N���X�̃I�u�W�F�N�g
#
#		Methods:
#			�ȉ����Q�ƁB
#-------------------------------------------------------------------------------
.model.adapter.default <- setRefClass(
	"model.adapter",
	fields = list(
		settings = "model.settings"
	)
)

#-------------------------------------------------------------------------------
#	���f�������ʂȂ̂���A�Ȃ̂��𔻒肷��B
#	Values:
#		��A�Ȃ�"regression"�A���ʂȂ�"classification"�B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type = function(){
		"
		Return a character vector specifying model type 
		(regression or classification).
		If the model is regression model, it returns 'regression'.
		If the model is classification model, it returns 'classification'
		"
		return(get.model.type.from.response.var())
	}
)

#-------------------------------------------------------------------------------
#	settings$args.model$family�Ɋ�Â��āA���f�������ʂȂ̂���A�Ȃ̂��𔻒肷��B
#
#	Values:
#		��A�Ȃ�"regression"�A���ʂȂ�"classification"�B	
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type.from.family = function(){
		"
		Return a character vector specifying model type 
		(regression or classification).
		Detection is based on the type of family in \\emph{family} in 
		\\emph{args.model} field of \\emph{settings} field (i.e. 
		settings$args.model$family).
		If the model is regression model, it returns 'regression'.
		If the model is classification model, it returns 'classification'
		"
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
#	�����ϐ��̌^�Ɋ�Â��āA���f�������ʂȂ̂���A�Ȃ̂��𔻒肷��B
#
#	Values:
#		��A�Ȃ�"regression"�A���ʂȂ�"classification"�B	
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.model.type.from.response.var = function(){
		"
		Return a character vector specifying model type
		(regression or classification).
		Detection is based on the class of response variable.
		If the model is regression model, it returns 'regression'.
		If the model is classification model, it returns 'classification'
		"
		if (is(get.response.var(), "factor")){
			return("classification")
		} else {
			return("regression")
		}
	}
)

#-------------------------------------------------------------------------------
#	���f�������\�w�W�𐳂����v�Z����悤�ɁA���f���\�z�̈������C�����鑍�̊֐��B
#	���N���X�̊֐��͂Ȃɂ����Ȃ��B
#	�p�������N���X�̊֐���args.model��ύX����ꍇ�A�I���W�i����args.model��
#	args.model.src�ɕۑ�����K�v������B
#	�C�����Ȃ��ꍇ�Aargs.model.src�͒���0�̃��X�g�ł���K�v������B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	modify.args.model = function(){
		"
		Modify arguments used for model construction to obtain correct 
		calculation of performance measures.
		Default function in this base class do nothing.
		If an overiding function in an inherited class modifies args.model, it 
		should store original arg.model in args.model.src. Otherwise, 
		args.model.src should be a list of length zero (list()).
		"
		NULL
	}
)

#-------------------------------------------------------------------------------
#	���f���\�z�Ɏg����������烂�f����������킷formula���擾����B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.formula = function(){
		"
		Retrieve model formula (of fixed effect) from the arguments for the
		modeling function.
		"
		args.model <- settings$args.model
		if (!is.null(args.model$formula)){
			return(args.model$formula)
		} else {
			return(args.model[sapply(args.model, is.formula)][[1]])
		}
	}
)

#-------------------------------------------------------------------------------
#	args.model�̒���formula��.�����ۂ̕ϐ��ɒu�������鑍�̊֐��B
#	lmer, glmer�̎���.��W�J���邽�߂Ɏg����B
#
#	Args:
#		specials: terms.formula�֐��ɓn�����B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	expand.dot = function(specials = NULL){
		"
		Expand dot ('.') in model formula used for modeling and store it in
		\\emph{args.model} field of \\emph{settings} field.
		Argument \\emph{specials} is passed to 
		\\code{\\link[stats]{terms.formula}} function.
		This function is intended to be used when users use 
		\\code{\\link[lme4]{lmer}} and \\code{\\link[lme4]{glmer}} functions in
		\\emph{lme4} package.
		"
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
#	�����ϐ��̖��O�����o���֐��B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.response.name = function(){
		"
		Retrieve the name of the response variable from arguments used for 
		modeling.
		"
		return(as.character(get.formula())[2])
	}
)

#-------------------------------------------------------------------------------
#	�����ϐ������f���̈���������o���B
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.response.var = function(){
		"
		Retrieves the response variable from arguments used for modeling.
		"
		return(settings$data[[get.response.name()]])
	}
)



