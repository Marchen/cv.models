#-------------------------------------------------------------------------------
#'	(Internal) Wrapper function of clusterApplyLB.
#'
#'	This function wraps difference in arguments between 
#'	\code{\link[base]{lapply}} and \code{\link[parallel]{clusterApplyLB}}.
#'
#'	@param X same as \emph{lapply}.
#'	@param FUN same as \emph{lapply}.
#'	@param ... same as \emph{lapply}.
#-------------------------------------------------------------------------------
#	lapply�̃��b�p�[
#-------------------------------------------------------------------------------
cl.lapply  <- function(X, FUN, ...){
	return(clusterApplyLB(cl = cl, x = X, fun = FUN, ...))
}

#-------------------------------------------------------------------------------
#'	(Internal) Wrapper function of clusterMap.
#'
#'	This function wraps difference in arguments between 
#'	\code{\link[base]{mapply}} and \code{\link[parallel]{clusterMap}}.
#'
#'	@param FUN same as \emph{mapply}.
#'	@param ... same as \emph{mapply}.
#'	@param MoreArgs same as \emph{mapply}.
#'	@param SIMPLIFY same as \emph{mapply}.
#'	@param USE.NAMES same as \emph{mapply}.
#-------------------------------------------------------------------------------
#	mapply�̃��b�p�[
#-------------------------------------------------------------------------------
cl.mapply <- function(
	FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE
){
	return(
		clusterMAP(
			cl = cl, fun = FUN, ..., MoreArgs = MoreArgs,
			RECYCLE = TRUE, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES
		)
	)
}

#-------------------------------------------------------------------------------
#	�N���X�^�[�Ńp�b�P�[�W��ǂݍ��ފ֐��B
#
#	Args:
#		package.name: �p�b�P�[�W����\��������
#-------------------------------------------------------------------------------
cl.library <- function(package.name){
	ncl.library(package.name)
	expr <- parse(text = sprintf("require(%s)", package.name))
	clusterExport(cl, "expr", environment())
	clusterEvalQ(cl, eval(expr))
	clusterEvalQ(cl, rm(expr))
}

#-------------------------------------------------------------------------------
#	������Ŏw�肳�ꂽ�p�b�P�[�W��ǂݍ��ފ֐��B
#
#	Args:
#		package.name: �p�b�P�[�W����\��������
#-------------------------------------------------------------------------------
ncl.library <- function(package.name){
	expr <- parse(text = sprintf("require(%s)", package.name))
	eval(expr)
}

#-------------------------------------------------------------------------------
#	�N���X�o���f�[�V�����ƃp�����[�^�[�I���Ɏg���R�A�̐������蓖�Ă�B
#	�p�����[�^�[�I���̌�␔���g�p����R�A����������������p�����[�^�[�I�����A
#	���Ȃ�������N���X�o���f�[�V���������v�Z����B
#
#	expanded.args.model: �p�����[�^�[�I���̌�����ꂽ���X�g�B
#	n.cores: �v�Z�Ɏg���R�A�̐��B
#-------------------------------------------------------------------------------
assign.cores <- function(expanded.args.model, n.cores){
	if (is.null(n.cores)){
		require(parallel)
		n.cores <- detectCores()
	}
	n.cores.cv <- ifelse(length(expanded.args.model) < n.cores, n.cores, 1)
	n.cores.param.tune <- ifelse(length(expanded.args.model) < n.cores, 1, n.cores)
	return(list(cv = n.cores.cv, param.tune = n.cores.param.tune))
}


#-------------------------------------------------------------------------------
#	�N���X�^�[�ŕK�v�Ȋ֐��𑗐M����֐��B
#
#	Args:
#		cl: parallel�p�b�P�[�W�̃N���X�^�[�̃n���h���B
#-------------------------------------------------------------------------------
export.functions <- function(cl){
	export.objects <- c(
		"cv.one.fold", "init.cluster", "ncl.library", "make.cv.group",
		"get.package.name", "get.class.name", "get.args", "make.dummy",
		"merge.tunable.args", "predict.gamm", "get.positive.prob",
		"get.positive.class", "detect.type.from.response.var",
		"detect.type.from.family",
		"is.formula", "confusion.matrix", "cv.performance"
	)
	export.pattern <- paste(
		"^get\\.response\\.name.*", "^get\\.response\\.var", "^calc\\..*",
		"^get\\.tunable\\.args.*", "^modify\\.args\\.predict.*",
		"^modify\\.args\\.model.*", "^modify\\.response\\.var.*",
		"^get\\.formula.*", "^get\\.response\\.class.*",
		"^format\\.prediction.*", "^detect\\.model\\.type.*", sep = "|"
	)
	clusterExport(
		cl,
		c(export.objects, ls(pattern = export.pattern, envir = .GlobalEnv))
	)
}

#-------------------------------------------------------------------------------
#	parallel�p�b�P�[�W�̃N���X�^�[������������֐��B
#	Args:
#		n.cores: �v�Z�Ɏg���R�A�̐��B
#
#	Value:
#		�ȉ��̃I�u�W�F�N�g���܂܂ꂽ���X�g�B
#		cl: �N���X�^�[�̃n���h���B�R�A�����P�̂Ƃ��ɂ�NULL�������Ă�B
#		lapply:
#			lapply�Ƃ��Ďg����֐��B�R�A�����P����lapply()�A�R�A�����Q�ȏゾ��
#			clusterApplyLB()���g�����b�p�[�֐����i�[�����B
#		mapply:
#			mapply�Ƃ��Ďg����֐��B�R�A�����P����mapply()�A�R�A�����Q�ȏゾ��
#			clusterMap()���g�����b�p�[�֐����i�[�����B
#		close:
#			�N���X�^�[�����֐��B�R�A�����P���Ɖ������Ȃ��֐��A�R�A�����Q�ȏ�
#			����stopCluster()���Ăяo���ăN���X�^�[����郉�b�p�[���i�[�����B
#-------------------------------------------------------------------------------
init.cluster <- function(n.cores, seed = NULL){
	require(parallel)
	if (is.null(n.cores)) n.cores <- detectCores()
	if (n.cores != 1){
		result <- list(cl = makeCluster(n.cores))
		export.functions(result$cl)
		# �֐������蓖�ĂāA�N���X�^�̃n���h���𑗂荞�ށB
		result$lapply <- cl.lapply
		result$mapply <- cl.mapply
		result$library <- cl.library
		assign("cl", result$cl, envir = environment(cl.lapply))
		assign("cl", result$cl, envir = environment(cl.mapply))
		assign("cl", result$cl, envir = environment(cl.library))
		result$close <- function() stopCluster(result$cl)
	} else {
		result <- list(
			cl = NULL, lapply = lapply, mapply = mapply,
			close = function(){}, library = ncl.library
		)
	}
	return(result)
}



