
<!-- README.md is generated from README.Rmd. Please edit that file -->

# did2s

<!-- badges: start -->
<!-- badges: end -->

The goal of did2s is to …

## Installation

You can install did2s from github with:

    devtools::install_github("kylebutts/did2s")

## Example

This is a basic example which shows you how to solve a common problem:

    library(did2s)

    # Load Example Dataset
    data("df_hom")

    # Static
    static <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(treat)", treat_var = "treat", cluster_vars = "state")
    #> The variable 'state::40' has been removed because of collinearity (see $collin.var).
    summary(static$estimate, .vcov = static$adj_cov)
    #> OLS estimation, Dep. Var.: adj
    #> Observations: 31,000 
    #> Standard-errors: Custom 
    #>               Estimate Std. Error   t value  Pr(>|t|)    
    #> (Intercept) 1.6300e-13 1.4500e-09  0.000113  0.999911    
    #> treat::TRUE 2.0451e+00 5.2864e-02 38.686000 < 2.2e-16 ***
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> RMSE: 1.7118   Adj. R2: 0.230614

    # Event-Study
    es <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(rel_year)", treat_var = "treat", cluster_vars = "state")
    #> The variable 'state::40' has been removed because of collinearity (see $collin.var).
    #> Warning in matrix(unlist(c), ncol = length(names(coef(second_stage))), byrow =
    #> T): data length [2555] is not a sub-multiple or multiple of the number of rows
    #> [70]
    summary(es$estimate, .vcov = es$adj_cov)
    #> OLS estimation, Dep. Var.: adj
    #> Observations: 31,000 
    #> Standard-errors: Custom 
    #>                Estimate Std. Error   t value  Pr(>|t|)    
    #> (Intercept)    0.060093   0.112989  0.531845   0.59785    
    #> rel_year::-19 -0.130490   0.109301 -1.193900  0.239745    
    #> rel_year::-18 -0.063008   0.133444 -0.472167  0.639439    
    #> rel_year::-17 -0.003222   0.101303 -0.031808  0.974787    
    #> rel_year::-16  0.102846   0.114588  0.897525  0.374947    
    #> rel_year::-15 -0.029929   0.117499 -0.254719  0.800279    
    #> rel_year::-14 -0.063421   0.120579 -0.525967  0.601889    
    #> rel_year::-13  0.001988   0.099998  0.019883  0.984238    
    #> rel_year::-12 -0.036453   0.098604 -0.369687  0.713615    
    #> rel_year::-11 -0.098209   0.113293 -0.866860  0.391321    
    #> rel_year::-10  0.002502   0.098264  0.025460  0.979818    
    #> rel_year::-9  -0.079275   0.098099 -0.808111  0.423929    
    #> rel_year::-8   0.032805   0.101266  0.323945  0.747711    
    #> rel_year::-7  -0.016863   0.105742 -0.159470  0.874122    
    #> rel_year::-6  -0.095366   0.105316 -0.905517  0.370752    
    #> rel_year::-5  -0.090389   0.108945 -0.829675  0.411772    
    #> rel_year::-4   0.012682   0.105405  0.120314  0.904852    
    #> rel_year::-3  -0.068470   0.092272 -0.742043  0.462507    
    #> rel_year::-2   0.027895   0.100894  0.276481  0.783639    
    #> rel_year::-1  -0.007489   0.105271 -0.071144  0.943647    
    #> rel_year::0    2.044500   0.105116 19.450000 < 2.2e-16 ***
    #> rel_year::1    1.992500   0.113204 17.601000 < 2.2e-16 ***
    #> rel_year::2    1.919400   0.109528 17.524000 < 2.2e-16 ***
    #> rel_year::3    1.910000   0.119237 16.019000 < 2.2e-16 ***
    #> rel_year::4    1.985600   0.123017 16.141000 < 2.2e-16 ***
    #> rel_year::5    1.933000   0.124477 15.529000 < 2.2e-16 ***
    #> rel_year::6    2.038800   0.117613 17.335000 < 2.2e-16 ***
    #> rel_year::7    1.930600   0.111878 17.256000 < 2.2e-16 ***
    #> rel_year::8    1.984700   0.132440 14.986000 < 2.2e-16 ***
    #> rel_year::9    2.129100   0.113181 18.811000 < 2.2e-16 ***
    #> rel_year::10   1.948500   0.118595 16.430000 < 2.2e-16 ***
    #> rel_year::11   1.970300   0.147185 13.387000  3.65e-16 ***
    #> rel_year::12   2.116600   0.154573 13.693000 < 2.2e-16 ***
    #> rel_year::13   1.849200   0.172245 10.736000   3.3e-13 ***
    #> rel_year::14   1.970100   0.155300 12.686000  2.03e-15 ***
    #> rel_year::15   2.057500   0.162567 12.656000  2.18e-15 ***
    #> rel_year::Inf -0.096264   0.148200 -0.649557  0.519786    
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> RMSE: 1.7109   Adj. R2: 0.230576
