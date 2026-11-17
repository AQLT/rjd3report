.onLoad <- function(libname, pkgname) {
    # Dictionaries are reloaded by {rjd3x13} and {rjd3tramoseats} when they are loaded
    for (pkg in c("rjd3toolkit", "rjd3x13", "rjd3tramoseats")) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(sprintf("Loading {%s} failed", pkg), call. = FALSE)
        }
    }
    # If java >= 21, then reload dictionnaries
    has_java <- rjd3jars::check_java_version(silent = TRUE)
    if (has_java) {
        rjd3jars::reload_dictionaries()
        rjd3jars::reload_tsproviders()
        rjd3jars::reload_safactories()
    }
}
