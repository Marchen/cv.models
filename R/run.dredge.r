run.dredge <- function(
	args.dredge, model.function, args.model, data, n.cores, seed
){
	args.model$data <- data
	grobal.model <- do.call(model.fuction, args.models)
	cl <- init.cluster(n.cores, seed)
	cl
}
