

SUPPORTED_FUNCTIONS <- c(
	"lm", "glm", "lme", "glmmML", "lmer", "glmer", "ctree", "cforest",
	"randomForest", "gbm", "svm", "tree", "rpart", "gam", "gamm"
)

model.settings <- setRefClass(
	Class = "model.settings",
	field = list(
		model				= "language",
		function.name		= "character",
		package.name		= "character",
		model.type			= "character",
		n.class.response	= "integer",
		is.default			= "logical",
		args.model			= "list",
		args.predict		= "list",
		args.model.src		= "list",
		args.predict.src	= "list",
		data				= "data.frame"
	)
)

model.settings$methods(
	initialize = function(...){
		if (length(list(...)) > 0){	#model.adapter‚Ì’è‹`‚©‚çŒÄ‚Î‚ê‚éŽž‚Ì‘Îˆ
			object <- callSuper(...)
			if (!object$function.name %in% SUPPORTED_FUNCTIONS){
				object$is.default <- TRUE
			} else {
				object$is.default <- FALSE
			}
			return(object)
		}
	}
)

model.settings$methods(
	set.model.type = function(type = c("regression", "classification")){
		type <- match.arg(type)
		model.type <<- type
	}
)




