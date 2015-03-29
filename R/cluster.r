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
#		package.names: �p�b�P�[�W����\��������
#-------------------------------------------------------------------------------
cl.library <- function(package.names){
	ncl.library(package.names)
	for (name in package.names){
		expr <- parse(text = sprintf("require(%s)", name))
		clusterExport(cl, "expr", environment())
		clusterEvalQ(cl, eval(expr))
		clusterEvalQ(cl, rm(expr))
	}
}

#-------------------------------------------------------------------------------
#	������Ŏw�肳�ꂽ�p�b�P�[�W��ǂݍ��ފ֐��B
#
#	Args:
#		package.names: �p�b�P�[�W����\��������B�����w��\�B
#-------------------------------------------------------------------------------
ncl.library <- function(package.names){
	for (name in package.names){
		expr <- parse(text = sprintf("require(%s)", name))
		eval(expr)
	}
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
		"merge.tunable.args", "predict.gamm", "predict.glmmML",
		"get.positive.prob", "get.positive.class",
		"get.model.type.from.response.var", "get.model.type.from.family",
		"merge.cv.performances", "is.formula", "confusion.matrix",
		"cv.performance", "format.family"
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


#-------------------------------------------------------------------------------
#'	Model prediction with parallel computing.
#'
#'	This function call predict method with parallel computation. When predict 
#'	is called, newdata is splitted into (nearly) equall number of rows
#'
#'	@param object a model object to be passed to predict method.
#'	@param newdata passed to predict method.
#'	@param ... other arguments passed to predict method.
#'	@param n.cores
#'		number of cores used for prediction. If not specified, the number of 
#'		logical cores of the machine is used.
#'	@param package.names 
#'		a character vector specifying package names to be loaded in the cluster.
#'
#-------------------------------------------------------------------------------
#	����v�Z��predict�����s����B
#
#	Args:
#		object, newdata, ...: predict�ɓn�����B
#		n.cores: ����v�Z�Ŏg���R�A�̐�
#		package.names:
#			�N���X�^�[�Ń��[�h����p�b�P�[�W����\���x�N�g���B
#			�w�肵�Ȃ���object�����Ɏ������肷��B
#-------------------------------------------------------------------------------
parallel.predict <- function(
	object, newdata = NULL, ..., n.cores = NULL,
	package.names = get.package.name(object)
){
	cl <- init.cluster(n.cores)
	# �R�A���P������������A�ӂ���predict���ʂ�Ԃ��B
	if (is.null(cl$cl)){
		return(predict(object, newdata = newdata, ...))
	}
	on.exit(cl$close())
	cl$library(package.names)
	newdata = split(newdata, cut(1:nrow(newdata), breaks = length(cl$cl)))
	result <- cl$lapply(newdata, FUN = predict, object = object, ...)
	result <- do.call(c, result)
	return(result)
}


