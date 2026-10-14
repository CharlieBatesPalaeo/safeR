#' safeParResults
#' @description Class to store results from [safePar()]
#' 
#' @slot func function that was run in parallel.
#' @slot fixedArgs list of fixed arguments passed to [safePar()].
#' @slot iterables named list of iterables passed to [safePar()].
#' @slot iterType character string indicating the type of iteration used in [safePar()].
#' One "pairwise" or "combinatorial".
#' @slot results matrix of results from [safePar()]. each row reads \code{iteration -> iterable indices -> result}.
##################
### definition ###
##################
#' @export
methods::setClass(
    "safeParResults",
    slots= list(
        func = "function",
        fixedArgs = "list",
        iterables = "list",
        iterType = "character",
        results = "data.frame"
    ),
    prototype = list(
        results = data.frame()
    )
)

# print method
methods::setMethod(
    "show",
    signature = "safeParResults",
    definition = function(object) {
        cat(
            "safeParResults object with",
            nrow(object@results),
            "results\n"
        )
        print(object@results)
    }
)

# View method
methods::setGeneric("View")
methods::setMethod(
    "View",
    signature(x = "safeParResults"),
    function(x, title = deparse(substitute(x))) {
        utils::View(x@results, title)
    }
)

#' Extract results by iteration index.
#'
#' @rdname safeParResults-class
#' @aliases [,safeParResults,numeric-method
#' @param x A safeParResults object.
#' @param i A numeric iteration index.
methods::setMethod(
    "[",
    signature(
        x = "safeParResults",
        i = "numeric"
    ),
    definition = function(x,i) {
        x@results[i,ncol(x@results)]
    }
)

