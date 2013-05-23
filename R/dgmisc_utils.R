# Package Utils

## The startup method
.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is dgmisc (v. ", utils::packageDescription("frair", field="Version"), appendLF = TRUE)
}