#' safePar
#' @description parallelisation for functions with (and without) safeR validation logic.
#' 
#' @param func function to be executed in parallel.
#' @param iterables named list of iterables to be passed into func. Names must match argument
#' names of \code{func}.#
#' @param iterType character string specifying how to iterate over the iterables. Must be one of
#' "pairwise" or "combinatorial".
#' @param nCores positive integer specifying the number of cores to use for parallelisation.
#' 
#' @return [safeParResults-class] object containing the results of the parallelised function calls.
#' 
#' @details Parallelisation is sub-process-based as this is suitable for all operating systems and is more predictable than forking,
#' especially in terms of memory footprint.
#' 
#' Fixed arguments for \code{func} (i.e. those not specified in \code{iterables}) should be set as default values
#' in the function definition.
#' 
#' User-defined global objects, functions, and classes utilised by \code{func}, or any lower-level functions called by \code{func},
#' will be automatically extracted and passed into child processes. Likewise, the parent safeR environment, including global options 
#' and defined coercion methods will be passed into child processes. This allows for safeR validation logic to be applied to
#' functions executed in parallel.
#' 
#' Error handling can be set using the global option \code{safeParErrorMode}, which can be set to either "first" or "all". In "first
#' mode, the first error encountered will be returned to the user, while in "all" mode, all errors will be returned to the user after
#' all iterations have completed. This can be changed with [setSafeParErrorMode()]. If using \code{"all"}, per-child-process error
#' messages can be accessed using [safeParTraceback()].
#'
#' @examples
## define positive integer class with numeric coercion method
#' setClass(
#'     "posInt",
#'     contains = "integer",
#'     validity = function(object) {
#'         if (object < 0) stop("must be positive") else TRUE
#'     }
#' )
#' setCoercion(
#'     "numeric",
#'     "posInt",
#'     function(x) {
#'         if (length(x) != 1 || !is.numeric(x)) stop("must be a single numeric value")
#'         as(x, "posInt")
#'     }
#' )
#' ## arbitrary function to validate
#' A = 10
#' testWrap <- (function(
#'     x = 1 ~ posInt,
#'     y = 2 ~ posInt,
#'     a = A
#' ) {as.numeric(x + y + a)}) %->% "posInt"
#' ## run in parallel
#' testPairwise <- safePar(
#'     func = testWrap,
#'     iterables = list(
#'         x = c(1,2,3),
#'         y = c(4,5,6)
#'     ),
#'     iterType = "pairwise",
#'     nCores = 2
#' )
#' testCombinatorial <- safePar(
#'     func = testWrap,
#'     iterables = list(
#'         x = c(1,2,3),
#'         y = c(4,5,6)
#'     ),
#'     iterType = "combinatorial",
#'     nCores = 2
#' )
##################
### definition ###
##################
#' @export
methods::setGeneric(
    "safePar",
    function(
        func,
        iterables,
        iterType = "pairwise",
        nCores = parallel::detectCores()
    ) {
        stop("typeError: 'func' must be a function")
    }
)

# method for function
#' @rdname safePar
#' @aliases safePar,function-method
#' @export
methods::setMethod(
    "safePar",
    signature(func = "function"),
    definition = function(
        func,
        iterables,
        iterType,
        nCores
    ) {
        # validate nCores
        if (!is.numeric(nCores) || length(nCores) != 1 || nCores < 1) {
            stop("argError: 'nCores' must be a positive integer")
        }
        # run internal function
        out <- .easyParInner(
            func = func,
            iterables = iterables,
            iterType = iterType,
            nCores = nCores
        )
        return(out)
    }
)

# internal function (actually does things)
.easyParInner <- function(
    func,
    iterables,
    iterType, # "pairwise" or "combinatorial"
    nCores
) {
    ###########################
    # env and fixed variables #
    ###########################
    # get user-defined objects
    start <- Sys.time()
    userObjs <- .getUserObjs(func)
    end <- Sys.time()
    print(paste0("userObjs extraction time: ", round(end - start, 2), " seconds"))

    # get fixed args
    start <- Sys.time()
    fixedArgs <- .getFixedArgs(
        func,
        iterables
    )
    end <- Sys.time()
    print(paste0("fixedArgs extraction time: ",round(end - start, 2), 2, " seconds"))

    start <- Sys.time()
    # get coercion cache
    coercionCache <- get(".coercionEnvCache", envir = asNamespace("safeR"))
    coercions <- lapply(
        as.list(coercionCache, all.names = TRUE),
        function(targetEnv) {
            methods <- as.list(targetEnv, all.names = TRUE)

            lapply(methods, function(fun) {
                funEnv <- environment(fun)

                list(
                    fun = fun,
                    bindings = as.list(funEnv, all.names = TRUE)
                )
            })
        }
    )
    end <- Sys.time()
    print(paste0("coercions extraction time: ", round(end - start, 2), " seconds"))
    
    # get global safeR options
    start <- Sys.time()
    safeROptions <- list(
        safeR = getOption("safeR"),
        safeParErrorMode = getOption("safeParErrorMode")
    )
    end <- Sys.time()
    print(paste0("safeROptions extraction time: ", round(end - start, 2), " seconds"))

    tempFiles <- character()
    on.exit(unlink(tempFiles), add = TRUE)
    # create temp file
    env <- tempfile(fileext = ".rds")
    tempFiles <- c(tempFiles, env)
    
    start <- Sys.time()
    saveRDS(
        list(
            func = func,
            userObjs = userObjs,
            fixedArgs = fixedArgs,
            coercions = coercions,
            safeROptions = safeROptions
        ),
        file = env
    )
    end <- Sys.time()
    print(paste0("env extraction time: ", round(end - start, 2), " seconds"))  
    #############
    # iterables #
    #############
    # validate iterables
    if (!(is.list(iterables) & all(names(iterables) %in% names(formals(func))))) {
        stop("argError: 'iterables' must be a named list with names matching the arguments of 'func'")
    }
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
        )),
        stop("valueError: 'iterType' must be one of 'pairwise' or 'combinatorial'")
    )
    #####################
    # run sub-processes #
    #####################
    ## list for outputs
    resPaths <- list()
    errors <- list()
    ## init queues
    # iterables
    iterQueue <- dequer::queue()
    sapply(
        1:nrow(iterablesMatrix),
        function(i) dequer::pushback(iterQueue, i)
    )
    # worker queue
    workerQueue <- dequer::queue()
    sapply(
        1:nCores,
        function(i) dequer::pushback(workerQueue, i)
    )
    active <- list()
    ## run sub-processes
    while (length(iterQueue) > 0) {
        # get next iter row
        i <- dequer::pop(iterQueue)
        # wait for worker
        w <- NULL
        while (is.null(w)) {
            # check for finished workers
            for (a in names(active)) {
                if (!active[[a]]$is_alive()) {
                    # kill
                    active[[a]]$wait()
                    # check for errors
                    if (active[[a]]$get_exit_status() != 0) {
                        # depending on safeParErrorMode
                        if (getOption("safeParErrorMode") == "first") {
                            err <- active[[a]]$read_error_lines()
                            stop(
                                "Error occurred in iteration ", a, ":\n",
                                paste(err, collapse = "\n")
                            )
                        }
                        errors[[paste0("iter", a)]] <- active[[a]]$read_error_lines()
                    }
                    # add worker back to queue
                    dequer::pushback(workerQueue, as.numeric(a))
                    # mark as inactive
                    active[[a]] <- NULL
                }
            }
            try(
                w <- dequer::pop(workerQueue),
                silent = TRUE
            )
        }
        # create temp file for iterables
        iter <- tempfile(fileext = ".rds")
        tempFiles <- c(tempFiles, iter)
        saveRDS(
            as.list(iterablesMatrix[i, , drop = TRUE]),
            file = iter
        )
        # create temp file for output
        out <- tempfile(fileext = ".rds")
        tempFiles <- c(tempFiles, out)
        resPaths[[i]] <- out
        # run sub-process
        options(callr.traceback = TRUE)
        p <- processx::process$new(
            command = file.path(R.home("bin"), "Rscript"),
            args = c(
                system.file(
                    "scripts",
                    "safeParChild.r",
                    package = "safeR"
                ),
                env,
                iter,
                out
            ),
            stdout = "|",
            stderr = "|"
        )
        # mark as active
        active[[as.character(w)]] <- p
    }
    # wait for remaining workers to finish
    while (length(active) > 0) {
        # check for finished workers
        for (a in names(active)) {
            if (!active[[a]]$is_alive()) {
                # kill
                active[[a]]$wait()
                # check for errors
                if (active[[a]]$get_exit_status() != 0) {
                    # depending on safeParErrorMode
                    if (getOption("safeParErrorMode") == "first") {
                        err <- active[[a]]$read_error_lines()
                        stop(
                            "Error occurred in iteration ", a, ":\n",
                            paste(err, collapse = "\n")
                        )
                        }
                    errors[[paste0("iter", a)]] <- active[[a]]$read_error_lines()
                }
                # mark as inactive
                active[[a]] <- NULL
            }
        }
    }
    ##################
    # error handling #
    ##################
    if (length(errors) > 0) {
        # store errors in the package namespace
        env <- get(".safeRState", envir = asNamespace("safeR"))
        env$parErrors <- errors
        # summary error message
        stop(
            "Errors occurred in the following iterations: ",
            paste(names(errors), collapse = ", "),
            ". Use safeParTraceback() to view individual error messages."
        )
    }
    ## format results
    # new safeParResults object
    out <- methods::new(
        "safeParResults",
        func = func,
        fixedArgs = fixedArgs,
        iterables = iterables,
        iterType = iterType
    )
    # iterables as input indices
    iterablesIdx <- apply(
    iterablesMatrix,
    2,
    function(x) match(x, unique(x))
    )
    # combine with results
    results <- data.frame(
        iter = seq_len(nrow(iterablesIdx)),
        iterablesIdx,
        result = I(lapply(resPaths, readRDS))
    )
    # add to safeParResults object
    out@results <- results
    return(out)
}

# sub-function for recursively extracting user-defined objects
.getUserObjs <- function(
    func
) {
    #list of found objects
    found <- c()
    junk <- c()
    
    # recursive function
    recursiveFunc <- function(f) {
        # exit if not a function
        if (!is.function(f)) return()
        # get expressions in function body
        expressions <- .splitBody(body(f))
        # remove empty expressions
        expressions <- expressions[
            !vapply(
                expressions,
                identical,
                logical(1),
                quote(expr = )
            )
        ]
        # remove closures

        # add user-defined objects to found
        for (e in expressions) {
            e <- paste0(as.character(e), collapse = "")
            # check if junk or already found
            if (e %in% junk || e %in% names(found)) next
            # check if user-defined object
            tryCatch({
                found[[e]] <<- .getUserGlobal(e)
                # recurse
                recursiveFunc(found[[e]])
                next
            },
                error = function(e) NULL   
            )
            tryCatch({
                found[[e]] <<- .getUserClass(e)
                next
            },
                error = function(e) NULL   
            )
            # else add to junk
            junk <- c(junk, e)
        }
    }
    recursiveFunc(func)
    return(found)
}

# sub-function for splitting function body
.splitBody <- function(x) {
    # functions
    if (is.function(x))
        return(unlist(
            lapply(
                list(formals(x), body(x)),
                .splitBody
            ),
            recursive = FALSE
        ))
    # other expressions
    if (!is.call(x))
        return(list(
            if (is.character(x)) x
            else paste(deparse(x), collapse = "")
        ))
    unlist(
        lapply(as.list(x), .splitBody),
        recursive = FALSE
    )
}

# sub-function for identifying user-defined global objects
.getUserGlobal <- function(x) {
    if (
        exists(x, envir = .GlobalEnv, inherits = FALSE) &&
        !exists(x, envir = baseenv(), inherits = FALSE)
    ) {
        return(
            get(x, envir = .GlobalEnv, inherits = FALSE)
        )
    }
    stop()
}

# sub-function for identifying user-defined classes
.getUserClass <- function(x) {
    s4 <- methods::getClassDef(x, where = .GlobalEnv, inherits = FALSE)
    if (!is.null(s4))
        return(s4)

    if (exists(x, envir = .GlobalEnv, inherits = FALSE)) {
        obj <- get(x, envir = .GlobalEnv, inherits = FALSE)

        if (inherits(obj, "R6ClassGenerator"))
            return(obj)
    }
    stop()
}

# sub-function for extracting fixed args
.getFixedArgs <- function(
    func,
    iterables
) {
    # pull fixed args from func definition
    funcArgs <- formals(func)
    fixed = list()
    for (arg in names(funcArgs)) {
        # non-iterables
        if (!(arg %in% names(iterables))) {
            # error if no default
            if (identical(funcArgs[[arg]], quote(expr = ))) {
                stop(paste0("argError: 'fixed argument ", arg, "' is missing default value"))
            }
            # evaluate default in parent frame
            fixed[[funcArgs[[arg]]]] <- eval(funcArgs[[arg]], envir = parent.frame())
        }
    }
    return(fixed)
}