#-------------------------------------------------------------------------------
#'	predict method for gamm object.
#'
#'	This is a predict method for \emph{gamm} object.
#'	This function is an internal function and not intended to be used directly 
#'	by users. This function internally pass the arguments to the
#'	\emph{predict.gam} using \emph{gam} object in \emph{gamm} object.
#'
#-------------------------------------------------------------------------------
#	gamm�I�u�W�F�N�g�p��predict�֐��B�����I��predict.gam���Ăяo���B
#
#	Args:
#		predict.gam�����Ă��������B
#-------------------------------------------------------------------------------
predict.gamm <- function(
	object, newdata, type = "link", se.fit = FALSE, terms = NULL, 
	block.size = NULL, newdata.guaranteed = FALSE, na.action = na.pass, 
	unconditional = FALSE, ...
){
#	warning("Prediction will use the unconditional (population-level) values and random effects are ignored.")
	result <- predict(
		object$gam, newdata, type, se.fit, terms, block.size,
		newdata.guaranteed, na.action, unconditional, ...
	)
	return(result)
}


#-------------------------------------------------------------------------------
#'	predict method for glmmML object.
#'
#'	This is a predict method for \emph{glmmML} object.
#'
#'	@param object a glmmML object.
#'	@param newdata a data.frame containing data used for prediction.
#'	@param type
#'		type of prediction. Default is scale of response variable.
#'		If "link" is specified, prediction is of link scale is calculated.
#'	@param conditional
#		if FALSE, marginal (using fixed terms only and unconditioned to random 
#		effect) predicted values are calculated. If TRUE, predicted values 
#		conditioned to the random effect is calculated.
#'	@param ... further arguments passed to other methods.
#-------------------------------------------------------------------------------
#	glmmML�p��predict���\�b�h�B
#
#	Args:
#		object:
#			glmmML�I�u�W�F�N�g�B
#		newdata:
#			�\���Ɏg���f�[�^���������f�[�^�t���[���B
#		type:
#			�\�����v�Z����X�P�[���B
#			�f�t�H���g�ł͉����ϐ��̃X�P�[���ŗ\�����v�Z
#			����B"link"���w�肷��ƁA�����N�֐��̃X�P�[���ŗ\�����v�Z����B
#		conditional:
#			TRUE����conditional�i�����_�����ʂɂ��ؕЂ̈Ⴂ���l�������j��
#			�\���l���v�Z����B
#			FALSE����marginal�i�����_�����ʂ𖳎������Œ���ʂ����j�ȗ\���l��
#			�v�Z����B
#		...: ���̃��\�b�h�ɓn���������B
#-------------------------------------------------------------------------------
predict.glmmML <- function(
	object, newdata, type = c("response", "link"), conditional = FALSE, ...
){
	type = match.arg(type)
	design.matrix <- model.matrix(object, data = newdata)
	result <- design.matrix %*% coef(object)
	# �����_�����ʂ��g�����ɂ̓����_�����ʕ��̐ؕЂ̂�������Z����B
	if (conditional){
		cluster <- eval(object$call$cluster, eval(object$call$data))
		result <- result + object$posterior.mode[as.numeric(cluster)]
	}
	if (type == "link"){
		return(result)
	} else {
		family <- format.family(object$call$family, type = "family")
		return(family$linkinv(result))
	}
}

