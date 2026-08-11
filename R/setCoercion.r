#' setCoercion
#' @description sets a coercion method for from an input class to a target class.
#' 
#' @param inputClass character string specifying the input class to coerce from.
#' @param targetClass character string specifying the target class to coerce to.
#' @param coercionFunction function defining the coercion logic from inputClass to targetClass
#' 
#' @details coercion methods are stored in a private cache environment for O(1) lookup, which is initialised upon loading the safeR package.
#' 
#' @examples
#' intToNum <- function(x) as.numeric(x)
#' setCoercion("integer", "numeric", intToNum)
##################
### definition ###
##################
## public namespace function for setting coercion methods for arbitrary classes
# generic + type error
#' @export
methods::setGeneric(
    "setCoercion",
    function(
        inputClass,
        targetClass,
        coercionFunction
    ) {
        stop("typeError: 'coercionFunction' must be a function")
    }
)

# method for function input
#' @rdname setCoercion
#' @aliases setCoercion,function-method
#' @export
methods::setMethod(
    "setCoercion",
    signature = c(coercionFunction = "function"),
    function(
        inputClass,
        targetClass,
        coercionFunction
    ) {
        # create or retrieve environment for target class in the coercion cache (envs for O1 lookup)
        if (!exists(targetClass, envir = .coercionEnvCache)) {
            env <- new.env(parent = emptyenv())
            assign(targetClass,
                env,
                envir = .coercionEnvCache
            )
        } else {
            env <- get(
                targetClass,
                envir = .coercionEnvCache,
                inherits = FALSE
            )
        }
        # store coercion function in env for input class
        assign(
            inputClass,
            coercionFunction,
            envir = env
        )
    }
)

## internal function to coerce an object to a target class following user-defined coercion logic
# generic + error in no coercion method 
methods::setGeneric(
    ".safeRCoerce", 
    function(
        x,
        targetClass
    ) {
    stop("typeError: 'targetClass' must be a character string")
    }
)

# define method for target class
methods::setMethod(
    ".safeRCoerce",
    signature = c(targetClass = "character"),
    function(x, targetClass) {
        # get input class
        xClass <- class(x)
        # skip coercion if input class is the same as target class
        if (xClass == targetClass) return(x)
        tryCatch(
            {
                env <- get(
                    targetClass,
                    envir = .coercionEnvCache,
                    inherits = FALSE
                )
                # get coercion function for input class
                coercionFunction <- get(
                    xClass,
                    envir = env
                )
                # apply coercion function to x
                return(coercionFunction(x))
            },
            # if not found, throw error
            error = function(e) {
                .coercionError(x, targetClass)
            }
        )
    }
)

# sub-function for throwing coercion error
.coercionError <- function(
    x,
    targetClass
) {
    stop(sprintf(
        "no coercion method defined for %s to %s",
        class(x),
        targetClass
    ))
}