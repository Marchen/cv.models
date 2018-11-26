
#-------------------------------------------------------------------------------
#'	(Internal) Multiple arguments version of which.min and which.max.
#'
#'	Evaluations of maximum/minimum are sequentially conducted in an order of
#'	variables provided in the arguments if the data were provided as numeric
#'	vectors.
#'	If first argument is a matrix or data.frame, evaluation are sequentially
#'	conducted left column to right column.
#'	If there is a tie, this function returns ALL index of them. This behavior is
#'	different from original which.min/which.max.
#'
#'	@param data
#'		a matrix, data.frame or list containing same length of vectors.
#'	@param min.max.function
#'		`min` or `max`.
#-------------------------------------------------------------------------------
which.min.or.max.multi <- function(data, min.max.function){
	# Check length of argument
	if (length(data) != 1 & !"data.frame" %in% class(data)){
		for (i in 2:length(data)){
			if (length(data[[1]]) != length(data[[i]])){
				stop("Length of the variables is not same!")
			}
		}
	}
	# search max/min value
	index <- 1:length(data[[1]])
	for (i in 1:length(data)){
		current.data <- data[[i]][index]
		index <- index[current.data == min.max.function(current.data)]
		if (length(index) == 1){
			return(index)
		}
	}
	return(index)
}


#-------------------------------------------------------------------------------
#'	(Internal) Multiple arguments version of which.min.
#'
#'	@param data
#'		a matrix, data.frame or list containing same length of vectors.
#-------------------------------------------------------------------------------
which.min.multi <- function(data){
	UseMethod("which.min.multi")
}


#-------------------------------------------------------------------------------
#'	(Internal) Multiple arguments version of which.min.
#'
#'	@param data
#'		a matrix, data.frame or list containing same length of vectors.
#-------------------------------------------------------------------------------
which.max.multi <- function(data){
	UseMethod("which.max.multi")
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi list
#-------------------------------------------------------------------------------
which.min.multi.list <- function(data){
	which.min.or.max.multi(data, min)
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi list
#-------------------------------------------------------------------------------
which.max.multi.list <- function(data){
	which.min.or.max.multi(data, max)
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi data.frame
#-------------------------------------------------------------------------------
which.min.multi.data.frame <- function(data){
	which.min.or.max.multi(data, min)
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi data.frame
#-------------------------------------------------------------------------------
which.max.multi.data.frame <- function(data){
	which.min.or.max.multi(data, max)
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi matrix
#-------------------------------------------------------------------------------
which.min.multi.matrix <- function(data){
	which.min.multi.list(data.frame(data))
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi matrix
#-------------------------------------------------------------------------------
which.max.multi.matrix <- function(data){
	which.max.multi.list(data.frame(data))
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi numeric
#-------------------------------------------------------------------------------
which.min.multi.numeric <- function(data, ...){
	which.min.multi.list(list(data, ...))
}


#-------------------------------------------------------------------------------
#'	@method which.min.multi numeric
#-------------------------------------------------------------------------------
which.max.multi.numeric <- function(data, ...){
	which.max.multi.list(list(data, ...))
}
