#' safePar
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
    nCores
) {
    ###########################
    # env and fixed variables #
    ###########################
    # get user-defined objects
    userObjs <- .getUserObjs(func)
    # get fixed args
    fixedArgs <- .getFixedArgs(
        func,
        iterables
    )
    # create temp file for Rscript
    env <- tempfile(fileext = ".rds")
    saveRDS(
        list(
            func = func,
            userObjs = userObjs,
            fixedArgs = fixedArgs
        ),
        file = env
    )
    #############
    # iterables #
    #############
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
    # create temp file for Rscript
    iters <- tempfile(fileext = ".rds")
    
    rowIter <- as.list(iterablesMatrix[1, ])
    names(rowIter) <- colnames(iterablesMatrix)
    saveRDS(
        rowIter,
        file = iters
    )

    ##################
    # run subprocces #
    ##################

    # test for cache copying
    tmp <- tempfile()
    cache <- get(".coercionEnvCache", envir = asNamespace("safeR"))

    coercions <- lapply(
        as.list(cache, all.names = TRUE),
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

    saveRDS(coercions, tmp)

    template <- "
        # load safeR
        library(safeR)

        # load env and fixed
        fixedPath <- commandArgs(trailingOnly = TRUE)[1]
        fixedVars <- readRDS(fixedPath)
        # func
        assign('func', fixedVars$func, envir = .GlobalEnv)

        userObjs <- fixedVars$userObjs
        for (nm in names(userObjs)) {
            obj <- userObjs[[nm]]
            assign(nm, obj, envir = .GlobalEnv)

            if (methods::isClassDef(obj)) {
                methods::assignClassDef(nm, obj, where = .GlobalEnv)
                methods::getClass(nm, where = .GlobalEnv)
            }
        }


        print(methods::isClass('posInt'))

        # load coercion cache
        cachePath <- commandArgs(trailingOnly = TRUE)[4]
        coercions <- readRDS(cachePath)
        cache <- get('.coercionEnvCache', envir = asNamespace('safeR'))

        for (target in names(coercions)) {
            targetEnv <- new.env(parent = .GlobalEnv)

            for (input in names(coercions[[target]])) {
                bundle <- coercions[[target]][[input]]
                funEnv <- list2env(bundle$bindings, parent = .GlobalEnv)

                fun <- bundle$fun
                environment(fun) <- funEnv
                assign(input, fun, envir = targetEnv)
            }

            assign(target, targetEnv, envir = cache)
        }

        # load iterables
        iterPath <- commandArgs(trailingOnly = TRUE)[2]
        iterVars <- readRDS(iterPath)
        list2env(iterVars, envir = .GlobalEnv)

        # load output
        outPath <- commandArgs(trailingOnly = TRUE)[3]

        # tmp cache
        tmpPath <- commandArgs(trailingOnly = TRUE)[4]
        tmpCache <- readRDS(tmpPath)

        # run func
        mc <- match.call(
            func,
            call = as.call(c(
                list(quote(func)),
                as.list(iterVars)
            )),
            expand.dots = FALSE
        )
        out <- eval(mc)

        # return output
        saveRDS(
            out,
            file = outPath
        )
    "
    run <- tempfile(fileext = ".R")
    writeLines(
        template,
        con = run
    )
    
    # test run
    out <- tempfile(fileext = ".rds")

    p <- processx::process$new(
        command = file.path(R.home("bin"), "Rscript"),
        args = c(
            run,
            env,
            iters,
            out,
            tmp ####
        ),
        stdout = "|",
        stderr = "|"
    )
    p$wait()
    cat(p$read_all_output())
    cat(p$read_all_error())

    print("after print...")

    if (p$get_exit_status() != 0) {
        cat(p$read_all_error())
        stop("Subprocess failed")
    }

    result <- readRDS(out)
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
            if (e %in% junk || e %in% found) next
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
    s4 <- getClassDef(x, where = .GlobalEnv, inherits = FALSE)
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