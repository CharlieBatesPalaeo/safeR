###########################################################################################################################
### This script is executed within safePar() subprocesses. It loads the parent safeR environment, extracts the required ###
### user-defined objects, and runs one iteration of the specified function. The output is written to a temporary file,  ###
### which is then read by the parent process.                                                                           ###
###########################################################################################################################

##############
# load safeR #
##############
library(safeR)
###########################
# load env and fixed args #
###########################
## load temp file
fixedPath <- commandArgs(trailingOnly = TRUE)[1]
fixedVars <- readRDS(fixedPath)
# extract func
assign('func', fixedVars$func, envir = .GlobalEnv)
# extract fixed args
userObjs <- fixedVars$userObjs
for (nm in names(userObjs)) {
    obj <- userObjs[[nm]]
    assign(nm, obj, envir = .GlobalEnv
    if (methods::isClassDef(obj)) {
        methods::assignClassDef(nm, obj, where = .GlobalEnv)
        methods::getClass(nm, where = .GlobalEnv)
    }
}
# extract coercion cache
cache <- get('.coercionEnvCache', envir = asNamespace('safeR'))
for (target in names(fixedVars$coercions)) {
    targetEnv <- new.env(parent = .GlobalEnv
    for (input in names(fixedVars$coercions[[target]])) {
        bundle <- fixedVars$coercions[[target]][[input]]
        funEnv <- list2env(bundle$bindings, parent = .GlobalEnv
        fun <- bundle$fun
        environment(fun) <- funEnv
        assign(input, fun, envir = targetEnv)
    }
    assign(target, targetEnv, envir = cache)
}
################## 
# load iterables #
##################
# read temp file
iterPath <- commandArgs(trailingOnly = TRUE)[2]
iterRow <- readRDS(iterPath)
iterRow <- list2env(iterRow, envir = .GlobalEnv)

###############
# load output #
###############
outPath <- commandArgs(trailingOnly = TRUE)[3]
################
# execute code #
################
mc <- match.call(
    func,
    call = as.call(c(
        list(quote(func)),
        as.list(iterVars)
    )),
    expand.dots = FALSE
)
out <- eval(mc)
###################
# write to output #
###################
# get idx
i <- as.numeric(commandArgs(trailingOnly = TRUE)[4])