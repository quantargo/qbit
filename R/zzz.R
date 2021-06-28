.onAttach <- function(libname, pkgname) {
  if (Sys.getenv("QKEY") != "") {
    options(QKEY = Sys.getenv("QKEY"))
  }

  options(STAGE = Sys.getenv("STAGE", "dev"))
  if (options("STAGE") == "dev") {
    options(QBITURL = "https://api.quantargo.com/lambda-dev")
  } else if (options("STAGE") == "prod") {
    options(QBITURL = "https://api.quantargo.com/lambda")
  }
}
