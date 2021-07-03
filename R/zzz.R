.onAttach <- function(libname, pkgname) {
  if (Sys.getenv("QKEY") != "") {
    options(QKEY = Sys.getenv("QKEY"))
  }
  if (Sys.getenv("QBITURL") != "") {
    options(QBITURL = Sys.getenv("QBITURL"))
  }
}
