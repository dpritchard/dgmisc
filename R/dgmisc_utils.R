# Package Utils

## The startup method
.onAttach <- function(lib, pkg)  {
    packageStartupMessage('This is dgmisc (v. ', utils::packageDescription("dgmisc", field="Version"), ')', appendLF = TRUE)
}