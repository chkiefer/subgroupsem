.onLoad <- function(libname, pkgname) {
    # Load the required packages
    # requireNamespace(reticulate)
    # requireNamespace(lavaan)

    # Welcome message to be friendly
    version <- read.dcf(
        file = system.file("DESCRIPTION", package = pkgname),
        fields = "Version"
    )
    packageStartupMessage("This is ", paste(pkgname, version))
    packageStartupMessage(pkgname, " is BETA software! Please report any bugs.")
}