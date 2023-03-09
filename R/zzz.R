.onAttach <- function(libname, pkgname) {
	info <- paste0(pkgname, " (v", utils::packageVersion(pkgname), "). For more information on the methodology, visit <https://www.kylebutts.github.io/did2s>")

	cit <- utils::citation(pkgname)
	txt <- paste(c(info, format(cit,"citation")),collapse="\n\n")
	packageStartupMessage(txt)
}
