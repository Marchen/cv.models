#-------------------------------------------------------------------------------
#'	(Internal) Expand dot in formula.
#'
#'	This internal function expand dot ('.') in model formula used for modeling.
#'
#'	@inheritParams modify.args.model
#'	@param specials A vector of character which passed to 
#'	\code{\link[stats]{terms.formula}} function.
#-------------------------------------------------------------------------------
#	args.model‚Ì’†‚Ìformula‚Ì.‚ğÀÛ‚Ì•Ï”‚É’u‚«Š·‚¦‚é‘ÌŠÖ”B
#-------------------------------------------------------------------------------
expand.dot <- function(cv.dummy, args.model, data, specials = NULL){
	UseMethod("expand.dot")
}

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Default S3 method. Intended to be used for \emph{lmerMod} object created by
#'	\code{\link[lme4]{lmer}} and \emph{glmerMod} object created by 
#'	\code{\link[lme4]{glmer}} function in \emph{lme4} package.
#'	@method expand.dot default
#-------------------------------------------------------------------------------
expand.dot.default <- function(cv.dummy, args.model, data, specials = NULL){
	# ®‚Ì€”õ
	f <- get.formula(cv.dummy, args.model)
	f <- terms(f, data = data, specials = specials)
	attributes(f) <- NULL
	f <- as.formula(f)
	args.model[[which(sapply(args.model, is.formula))]] <- f
	return(args.model)
}

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Method for \code{\link[gam]{gam}} function in \emph{gam} package and 
#'	\code{\link[mgcv]{gam}} function \emph{mgcv} package.
#'	@method expand.dot gam
#-------------------------------------------------------------------------------
expand.dot.gam <- function(cv.dummy, args.model, data, specials = NULL){
	# mgcv::gam‚Ægam:gam‚Å“Áê•¶š‚Ìí—Ş‚ğ•Ï‚¦‚éB
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
expand.dot.gamm <- function(cv.dummy, args.model, data, specials = NULL){
	args.model <- expand.dot.default(
		cv.dummy, args.model, data, c("s", "te", "ti", "t2")
	)
	return(args.model)
}

