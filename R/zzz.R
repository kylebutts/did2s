.onAttach <- function(libname, pkgname) {
	cli::cli_alert_info("{pkgname} (v{packageVersion(pkgname)}). For more information on the methodology, visit {.url https://www.kylebutts.com/did2s}")
	print(citation("did2s"))
}
