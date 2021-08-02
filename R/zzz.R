.onAttach <- function(libname, pkgname) {
	cli::cli_alert_info("{pkgname} (v{utils::packageVersion(pkgname)}). For more information on the methodology, visit {.url https://www.kylebutts.com/did2s}")

	cit <- utils::citation(pkgname)
	txt <- paste(c(format(cit,"citation")),collapse="\n\n")
	packageStartupMessage(txt)
}
