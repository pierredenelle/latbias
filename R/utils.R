# Controls #####################################################################
controls <- function(args = NULL, data = NULL,
                     type = "strict_positive_integer") {
  
  lstype <- c("strict_positive_integer",
              "character_or_positive_integer",
              "positive_numeric",
              "logical")
  
  if(!(type %in% lstype)){
    stop("Control type not defined!", call. = FALSE)
  }
  
  # Strict positive integer ###################################################
  if (type == "strict_positive_integer") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
           call. = FALSE)
    } else {
      if (args %% 1 != 0) {
        stop(paste0(deparse(substitute(args)), " must be an integer."),
             call. = FALSE
        )
      } else {
        if (args <= 0) {
          stop(paste0(deparse(substitute(args)),
                      " must be strictly higher than 0."), call. = FALSE)
        }
      }
    }
  }
  
  # Character or positive integer ##############################################
  if (type == "character_or_positive_integer") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.character(args) && !is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), 
                  " must be a character string or a positive integer."),
           call. = FALSE
      )
    }
    if (is.numeric(args)) {
      if (args %% 1 != 0) {
        stop(paste0(deparse(substitute(args)), " must be an integer."),
             call. = FALSE
        )
      }
      if (args <= 0) {
        stop(paste0(deparse(substitute(args)),
                    " must be strictly higher than 0."), 
             call. = FALSE)
      }
    }
  }
  
  # Positive numeric ##########################################################
  if (type == "positive_numeric") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
           call. = FALSE)
    } else {
      if (args < 0) {
        stop(paste0(deparse(substitute(args)), " must be higher than 0."),
             call. = FALSE
        )
      }
    }
  }
  
  # Logical ###################################################################
  if (type == "logical") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.logical(args) || is.na(args)) {
      stop(paste0(deparse(substitute(args)), " must be a logical."),
           call. = FALSE
      )
    }
  }
}
