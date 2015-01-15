#-------------------------------------------------------------------------------
#'	Expand dot in formula.
#'
#'	This internal function expand the dot ('.') in model formula in args used 
#'	for modeling.
#'
#'	@inheritParams modify.args.model
#-------------------------------------------------------------------------------
#	args.model�̒���formula��.�����ۂ̕ϐ��ɒu�������鑍�̊֐��B
#-------------------------------------------------------------------------------
expand.dot <- function(cv.dummy, args.model, data){
	UseMethod("expand.dot")
}

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Default S3 method. Intended to be used for \emph{merMod} object created by
#'	\code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}} functions in 
#'	\emph{lme4} package.
#'	@method expand.dot default
#-------------------------------------------------------------------------------
expand.dot.default <- function(cv.dummy, args.model, data, specials = ""){
	# ���̏���
	f <- get.formula(cv.dummy, args.model)
	f <- terms(f, data = data, specials = specials)
	attributes(f) <- NULL
	f <- as.formula(f)
	args.model[[which(sapply(args.model, is.formula))]] <- f
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Method for \code{\link[gam]{gam}} functions in \emph{gam} and \emph{mgcv}
#'	packages.
#'	@method expand.dot gam
#-------------------------------------------------------------------------------
expand.dot.gam <- function(cv.dummy, args.model, data, specials = ""){
	# mgcv::gam��gam:gam�œ��ꕶ���̎�ނ�ς���B
	if (cv.dummy$package == "mgcv"){
		args.model <- expand.dot.default(
			cv.dummy, args.model, data, c("s", "te", "ti", "t2")
		)
	} else {
		require(gam)
		args.model <- expand.dot.default(
			cv.dummy, args.model, data, gam::gam.slist
		)
	}
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Method for \code{\link[mgcv]{gamm}} function in \emph{mgcv} package.
#'	@method expand.dot gamm
#-------------------------------------------------------------------------------
expand.dot.gamm <- function(cv.dummy, args.model, data, specials = ""){
	args.model <- expand.dot.default(
		cv.dummy, args.model, data, c("s", "te", "ti", "t2")
	)
	return(args.model)
}

