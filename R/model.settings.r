.model.settings <- setRefClass(
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
	),
	methods = list(
		set.model.type = function(type = c("regression", "classification")){
			type <- match.arg(type)
			model.type <<- type
		}
	)
)

model.settings <- function(){
	.model.settings$new()
}


