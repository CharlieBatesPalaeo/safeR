###########################################################################
### Utility file for storing validation wrappers for exported functions ###
###                                                                     ###
### an empty cache is initialised upon loading the package              ###
### wrappers are created and cached on first call                       ###
###########################################################################

# empty cache upon loading package
.validationWrapperCache <- new.env(parent = emptyenv())

# helper function to get wrapper from cache
.getValidationWrapper <- function(
    name,
    func,
    ...
) {
    # get from cache if exists
    if(exists(name, envir = .validationWrapperCache)) {
        return(get(name, envir = .validationWrapperCache))
    }   
    # otherwise create, cache, and return wrapper
    wrapper <- validateArgs(
        func,
        ...
    )
    assign(
        name,
        wrapper,
        envir = .validationWrapperCache)
    wrapper
}
