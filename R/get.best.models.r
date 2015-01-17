#-------------------------------------------------------------------------------
#'	Get best models from cv.models object.
#'
#'	This function construct best model(s) from \emph{cv.models} object(s) by
#'	maximizing specified performance metrics.
#'
#'	@param ... \emph{cv.models} objects.
#'	@param metrics 
#'		a character vector containing names of performance metrics calculated by 
#'		cross validation by which performances of the models are evaluated. The 
#'		metrics used for this function must be specified in the argument of
#'		\code{\link{cv.models}} funciton. For the detail, see the document of 
#'		\code{\link{cv.models}} funciton.
#'
#'	@return
#'		a \emph{cv.best.models} object, which is a list containing 
#'		\emph{cv.best.model} objects with following fields.
#'	\describe{
#'		\item{model}{
#'			model object created by a model function.
#'		}
#'		\item{cv.metrics}{
#'			performance metrics calculated by cross validation.
#'		}
#'		\item{cv.prediction}{
#'			predicted values used for the calculation of performance metrics.
#'		}
#'		\item{cv.response}{
#'			values of response variables corresponding \emph{cv.prediction}.
#'		}
#'		\item{confusion.matrix}{
#'			a table object representing confusion.matrix. If the model is 
#'			regression model, this field is NULL.
#'		}
#'		\item{function.name}{
#'			a character string of function name used for constructing of
#'			\emph{model}.
#'		}
#'		\item{package.name}{
#'			a character string of package name containing the function used for
#'			model construction.
#'		}
#'	}
#'	@export
#-------------------------------------------------------------------------------
#	cv.models�I�u�W�F�N�g����metrics�Ŏw�肵���w�W�ōł����т̂悢���f����
#	���o���Bcv.models�I�u�W�F�N�g�Emetrics�͕����w��\�B
#
#	Args:
#		...: cv.models�I�u�W�F�N�g�B
#		metrics:
#			���f���ԂŔ�r���鐫�\�w�W�̖��O��������������x�N�g���B
#			�����w�肷��ƒl���^�C�������Ƃ��ɏ��Ԃɕ]�������B
#
#	Value:
#		�ȉ��̍\����cv.best.models�I�u�W�F�N�g�B
#		�^�C������ƃ��f���������ɂȂ�\��������̂ŁAconstruct.model()�֐���
#		���ʂ����X�g�ɓ���ĕԂ��B
#			list(
#				list(model, cv.metrics, function.name),
#				list(model, cv.metrics, function.name), ...
#			)
#			model: �\�z�������f���̃I�u�W�F�N�g�B
#			cv.metrics: ���̃��f���̃N���X�o���f�[�V�������\�w�W�B
#			cv.prediction: �N���X�o���f�[�V�����Ɏg�����\���l�B
#			cv.response: �\���l�ƑΉ�������בւ���ꂽ�����ϐ��̒l�B
#			function.name: �Ăяo�����֐��̖��O�B
#			package.name: �֐����܂�ł���p�b�P�[�W���B
#-------------------------------------------------------------------------------
get.best.models <- function(..., metrics = "auc"){
	# �����w�肳�ꂽcv.models�I�u�W�F�N�g�̒�����A���ꂼ��ōœK�Ȏw�W�����o���B
	# �w�W�̑��ɉ��Ԗڂ̎w�W����\��metrics.index���ǉ�����B
	get.best.metrics <- function(x, metrics){
		if (!is(x, "cv.models")) stop("Please specify 'cv.models' object!")
		met <- cbind(x$cv.metrics, metrics.index = 1:nrow(x$cv.metrics))
		met <- met[
			get.best.metrics.index(met[metrics]), c(metrics, "metrics.index")
		]
		return(met)
	}
	objects <- list(...)
	best.metrics <- lapply(objects, get.best.metrics, metrics)
	# ���Ԗڂ̃��f���I�u�W�F�N�g����\��object.index��ǉ����Č����B
	best.metrics <- mapply(
		cbind, best.metrics, object.index = 1:length(objects), SIMPLIFY = FALSE
	)
	best.metrics <- do.call(rbind, best.metrics)
	# �œK�ȃ��f���̎w�W���v�Z
	best.index <- get.best.metrics.index(best.metrics[metrics])
	best.metrics <- best.metrics[best.index, ]
	# ���f�����\�z
	object.index <- best.metrics[, "object.index"]
	metrics.index <- best.metrics[, "metrics.index"]
	make.models <- function(objects, object.index, metrics.index){
		return(construct.model(objects[[object.index]], metrics.index))
	}
	models <- mapply(
		make.models, object.index, metrics.index,
		MoreArgs = list(objects = objects), SIMPLIFY = FALSE
	)
	class(models) <- "cv.best.models"
	return(models)
}

