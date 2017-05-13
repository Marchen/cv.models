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
#	モデルの違いを吸収するアダプタークラスのオブジェクトを作る関数。
#	Rのポリモーフィズムを使わず、自前でクラスの初期化を行うので、新規関数への
#	対応を追加するためには、.model.adapter.関数名のReference Classの
#	ジェネレーターを実装する必要がある。
#
#	Args:
#		settings: model.settingsのインスタンス
#
#	Value:
#		model.adapterクラスのオブジェクト
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
#	モデリング関数の違いを吸収するReference Class、model.adapterクラスの
#	ジェネレーターオブジェクト。
#	この基底クラスを継承して、様々なモデルに対応させる。
#	（いまのところ）継承が必要なメソッドは以下の通り。
#
#			メソッド名							継承クラスでの関数への対応
#			get.model.type()					必要
#			get.model.type.from.family()		不要
#			get.model.type.from.response.var()	不要
#			modify.args.model()					必要
#			get.formula()						必要
#			expand.dot()						必要
#			get.response.name()					不要
#			get.response.var()					不要
#
#		Field:
#			settings: model.settingsクラスのオブジェクト
#
#		Methods:
#			以下を参照。
#-------------------------------------------------------------------------------
.model.adapter.default <- setRefClass(
	"model.adapter",
	fields = list(
		settings = "model.settings"
	)
)

#-------------------------------------------------------------------------------
#	モデルが識別なのか回帰なのかを判定する。
#	Values:
#		回帰なら"regression"、識別なら"classification"。
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
#	settings$args.model$familyに基づいて、モデルが識別なのか回帰なのかを判定する。
#
#	Values:
#		回帰なら"regression"、識別なら"classification"。	
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
		# familyがなかったらデフォルトは正規分布なので、回帰。
		if (is.null(settings$args.model$family)){
			return("regression")
		}
		# familyを文字列に変換。
		family <- format.family(settings$args.model$family, type = "character")
		# glmとgamの以下のfamilyが識別。それ以外は回帰
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
#	応答変数の型に基づいて、モデルが識別なのか回帰なのかを判定する。
#
#	Values:
#		回帰なら"regression"、識別なら"classification"。	
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
#	モデルが性能指標を正しく計算するように、モデル構築の引数を修正する総称関数。
#	基底クラスの関数はなにもしない。
#	継承したクラスの関数がargs.modelを変更する場合、オリジナルのargs.modelを
#	args.model.srcに保存する必要がある。
#	修正がない場合、args.model.srcは長さ0のリストである必要がある。
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
#	モデル構築に使われる引数からモデル式をあらわすformulaを取得する。
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
#	args.modelの中のformulaの.を実際の変数に置き換える総称関数。
#	lmer, glmerの式の.を展開するために使われる。
#
#	Args:
#		specials: terms.formula関数に渡される。
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
		# 式の準備
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
#	応答変数の名前を取り出す関数。
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
#	応答変数をモデルの引数から取り出す。
#-------------------------------------------------------------------------------
.model.adapter.default$methods(
	get.response.var = function(){
		"
		Retrieves the response variable from arguments used for modeling.
		"
		return(settings$data[[get.response.name()]])
	}
)



