#' lock
#' @name lock
#' 
#' @description Functions for locking the class and/or value of an object.
#' 
#' @param x object to be locked
#' @param lockValue single boolean determining whether to lock the value of \code{x}. default is \code{FALSE}.
#' @param ... extra args for shortcut wrappers (not useful for users)
#'
#' @returns NULL
#' 
#' @details \code{x} will be replaced with an active binding linking \code{x} to the newly created
#' environment '\code{xLocked}'. When \code{x} is called, a function is run. If evaluating
#' (i.e. \code{y <- x}), this function extracts the current value of \code{x} from \code{xLocked}.
#' If assigning (i.e. \code{x <- y}), the input is compared against the original class of \code{x}
#' or the original value of \code{x}, depending on the type of lock.
#' 
#' Locked objects will only remain locked within the environment they are created within. For example,
#' for a value-locked object \code{x} and function \code{func <- function(x) x <- y}, \code{func} will succeed,
#' but an error will be thrown if the output of \code{func} is assinged to \code{x}.
#' 
#' \code{lockClass()} and \code{lockValue()} are wrappers for \code{lock(x, lockValue = FALSE)} and
#' \code{lock(x, lockValue = TRUE)} respectively.
#' 
#' @examples
## type lock
#' x <- 10
#' lockClass(x)
#' # value is mutable
#' x <- 5
#' # type is not
#' try(
#'     x <- "notANumeric"
#' )
#' 
#' ## value lock
#' y <- 10
#' lockValue(y)
#' # value and type immutable
#' try(
#'     y <- 5
#' )
#' try(
#'     y <- "notANumeric"
#' )
#' 
#' ## used within function, but locked outside
#' z <- 10
#' func <- function(z) z <- 20
#' lockValue(z)
#' # function works in isolation
#' func(z)
#' # fails if output assigned to z
#' try(
#'     z <- func(z)
#' )
#' 
#' ## locked and used within function
#' w <- 10
#' func <- function(w) {
#'     lockClass(w)
#'     try(
#'         w <- "notANumeric"
#'     )
#' }
#' func(w)
##################
### Definition ###
##################
# main
#' @export
lock <- function(
    x,
    lockValue = FALSE,
    ...
) {
    # home env
    args <- list(...)
    if ("env" %in% names(args)) env <- args$env else env <- parent.frame()
    if ("name" %in% names(args)) name <- args$name else name <- deparse(substitute(x))
    # call type-safe wrapper
    # (see R/wrappers.r for caching logic)
    out <- .lockInner(
        x = x,
        lockValue = lockValue,
        env = env,
        name = name
    )
}

# wrappers for value and type locking
#' @rdname lock
#' @export
lockClass <- function(x) lock(
    x,
    lockValue = FALSE,
    env = parent.frame(),
    name = deparse(substitute(x))
)

#' @rdname lock
#' @export
lockValue <- function(x) lock(
    x,
    lockValue = TRUE,
    env = parent.frame(),
    name = deparse(substitute(x))
)

# internal function (actually does things)
.lockInner <- function(
    x,
    lockValue,
    env,
    name
) {
    # create env in parent frame
    lockedBox <- new.env()
    lockedBox$val <- x
    lockedBox$type <- class(x)
    assign(
        paste0(
            name,
            "Locked"),
        lockedBox,
        envir = env
    )
    # lock env
    lockEnvironment(
        lockedBox,
        bindings = lockValue
    )

    # remove original
    remove(
        list = name,
        envir = env
    )

    valErr <- paste0(
        "'",
        name,
        "' is locked to value of '",
        lockedBox$val,
        "'"
    )

    # bind original's namespace to env
    makeActiveBinding(
        name,
        # function for assigning/evaluating env$value
        function(lockedObj) {
            # if eval (no arg), get current value
            if(missing(lockedObj)) {
                lockedObj <- lockedBox$val
            # stop (with useful error message) if value is locked
            } else if (lockValue) {
                stop(valErr)
            # else update value
            } else {
                # 1) check class compatibility (will coerce if possible)
                lockedObj <- .safeRCoerce(
                    lockedObj,
                    lockedBox$type
                )
                # 2) check class compatibility (will throw error if not compatible)
                methods::validObject(lockedObj)
                # 3) update value
                lockedBox$val <- lockedObj
            }
            # return value silently
            invisible(lockedObj)
        },
        env
    )
}