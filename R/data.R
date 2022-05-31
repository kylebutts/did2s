#' @title Simulated data with two treatment groups and homogenous effects
#'
#' @description Generated using the following call:
#'   \code{did2s::gen_data(panel = c(1990, 2020),
#'   g1 = 2000, g2 = 2010, g3 = 0,
#'   te1 = 2, te2 = 2, te3 = 0,
#'   te_m1 = 0, te_m2 = 0, te_m3 = 0)}
#'
#' @format A data frame with 31000 rows and 15 variables:
#' \describe{
#'   \item{unit}{individual in panel data}
#'   \item{year}{time in panel data}
#'   \item{g}{the year that treatment starts}
#'   \item{dep_var}{outcome variable}
#'   \item{treat}{T/F variable for when treatment is on}
#'   \item{rel_year}{year relative to treatment start. Inf = never treated.}
#'   \item{rel_year_binned}{year relative to treatment start, but <=-6 and >=6
#'   are binned.}
#'   \item{unit_fe}{Unit FE}
#'   \item{year_fe}{Year FE}
#'   \item{error}{Random error component}
#'   \item{te}{Static treatment effect = te}
#'   \item{te_dynamic}{Dynamic treatmet effect = te_m}
#'   \item{group}{String name for group}
#'   \item{state}{State that unit is in}
#'   \item{weight}{Weight from runif()}
#' }
"df_hom"

#' @title Simulated data with two treatment groups and heterogenous effects
#'
#' @description Generated using the following call:
#'   \code{did2s::gen_data(panel = c(1990, 2020),
#'   g1 = 2000, g2 = 2010, g3 = 0,
#'   te1 = 2, te2 = 1, te3 = 0,
#'   te_m1 = 0.05, te_m2 = 0.15, te_m3 = 0)}
#'
#' @format A data frame with 31000 rows and 15 variables:
#' \describe{
#'   \item{unit}{individual in panel data}
#'   \item{year}{time in panel data}
#'   \item{g}{the year that treatment starts}
#'   \item{dep_var}{outcome variable}
#'   \item{treat}{T/F variable for when treatment is on}
#'   \item{rel_year}{year relative to treatment start. Inf = never treated.}
#'   \item{rel_year_binned}{year relative to treatment start, but <=-6 and >=6
#'   are binned.}
#'   \item{unit_fe}{Unit FE}
#'   \item{year_fe}{Year FE}
#'   \item{error}{Random error component}
#'   \item{te}{Static treatment effect = te}
#'   \item{te_dynamic}{Dynamic treatmet effect = te_m}
#'   \item{state}{State that unit is in}
#'   \item{group}{String name for group}
#' }
"df_het"




#' @title Data from Cheng and Hoekstra (2013)
#'
#' @description
#'   State-wide panel data from 2000-2010 that has information on castle-doctrine,
#'   the so-called "stand-your-ground" laws that were implemented by 20 states.
#'
#' @format A data frame with 550 rows and 5 variables:
#' \describe{
#'   \item{sid}{state id, unit of observation}
#'   \item{year}{time in panel data}
#'   \item{l_homicide}{log of the number of homicides per capita}
#'   \item{effyear}{year that castle doctrine is passed}
#'   \item{post}{0/1 variable for when castle doctrine is active}
#'   \item{time_til}{time relative to castle doctrine being passed into law}
#' }
"castle"
