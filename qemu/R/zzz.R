## friendly welcome message

.onAttach <- function(libname, pkgname) {
  ver <- packageVersion("qemu")
  packageStartupMessage(sprintf(
    "This is qemu v%s. For overview type \'?qemu\'", ver))
}


