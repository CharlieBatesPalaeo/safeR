#' easyPar
#' 
##################
### definition ###
##################
#' @export
setGeneric(
    "easyPar",
    function(
        func,
        iterables,
        iterType = "pairwise",
        nCores = 1
    ) {
        stop("typeError: 'func' must be a function")
    }
)

# method for function
#' @rdname easyPar
#' @aliases easyPar,function-method
#' @method easyPar function
#' @export
setMethod(
    "easyPar",
    signature(func = "function"),
    definition = function(
        func,
        iterables,
        iterType,
        nCores
    ) {
        # grab and run validation wrapper
        out <- .easyParInner(
            func = func,
            iterables = iterables,
            iterType = iterType,
            nCores = nCores
        )
    }
)

# internal function (actually does things)
.easyParInner <- function(
    func,
    iterables,
    iterType, # "pairwise" or "combinatorial"
    nCores #1 ## check this ##
) {
    # get user-defines functions
    userFuncs <- .getUserFuncs(func)
    # get fixed args
    fixedArgs <- .getFixedArgs(
        func,
        iterables
    )

    # set up subprocess environment
    cl <- parallel::makeCluster(nCores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    # load safeR
    parallel::clusterEvalQ(
        cl,
        {
            library("safeR")
            invisible(NULL)
        }
    )
    parallel::clusterExport(
        cl,
        varlist = userFuncs,
        envir = .GlobalEnv
    )
    parallel::clusterExport(
        cl,
        varlist = fixedArgs,
        envir = parent.frame()
    )
    #### temp
    parallel::clusterEvalQ(
        cl,
        {
            setClass(
                "posInt",
                contains = "integer",
                validity = function(object) {
                    if (object < 0) stop("must be positive") else TRUE
                }
            )

            safeR::setCoercion(
                "numeric",
                "posInt",
                function(x) {
                    if (length(x) != 1 || !is.numeric(x))
                        stop("must be a single numeric value")
                    as(x, "posInt")
                }
            )

            invisible(NULL)
        }
    )

    # create list of iterables
    iterablesMatrix <- switch(
        iterType,
        pairwise = do.call(
            rbind,
            do.call(
                Map,
                c(list(f = function(...) c(...)),iterables)
            )
        ),
        combinatorial = as.matrix(stats::setNames(
            do.call(
                expand.grid,
                iterables
            ),
            names(iterables)
        )
    ))
    
    # create func wrapper
    parFunc <- function(rowIter) {
        mc <- match.call(
            func,
            call = as.call(c(
                list(quote(func)),
                as.list(rowIter)
            )),
            expand.dots = FALSE
        )
        eval(mc)
    }

    # run in parallel
    res <- parallel::parApply(
        cl,
        iterablesMatrix,
        1,
        parFunc
    )

    # return results
    return(res)
}

# sub-function for recursively extracting user-defined functions
.getUserFuncs <- function(
    func
) {
    seen <- c()
    # recursive function

    recursiveFunc <- function(
        func
    ) {
        # get globals of func
        funcs <- codetools::findGlobals(func, merge = FALSE)$functions

        # filter out base R functions
        user_funcs <- funcs[!vapply(
            funcs,
            exists,
            logical(1),
            envir = baseenv(),
            inherits = FALSE
        )]
        # add new funcs to seen
        user_funcs <- setdiff(user_funcs, seen)
        seen <<- c(seen, user_funcs)

        # recurse on new funcs
        for (f in user_funcs) {
            recursiveFunc(get(f, envir = .GlobalEnv))
        }
    }
    recursiveFunc(func)
    return(seen)
}

# sub-function for extracting fixed args
.getFixedArgs <- function(
    func,
    iterables
) {
    # pull fixed args from func definition
    funcArgs <- formals(func)
    fixed = c()
    for (arg in names(funcArgs)) {
        # non-iterables
        if (!(arg %in% names(iterables))) {
            # error if no default
            if (identical(funcArgs[[arg]], quote(expr = ))) {
                stop(paste0("argError: 'fixed argument ", arg, "' is missing default value"))
            }
            # evaluate default in parent frame
            fixed <- append(fixed, deparse(funcArgs[[arg]]))
        }
    }
    return(fixed)
}