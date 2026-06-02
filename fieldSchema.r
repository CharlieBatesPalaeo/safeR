#######################
### type safe field ###
#######################

##########
# schema #
##########
setClass(
    "fieldSchema",
    slots = c(
        type = "character",
        default = "ANY",
        validation = "function"
    ),
    prototype = list(
        type = "ANY",
        default = NULL,
        validation = function(x) return(TRUE)
    )
)