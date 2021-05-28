
<!-- README.md is generated from README.Rmd. Please edit that file -->

# did2s

<!-- badges: start -->
<!-- badges: end -->

The goal of did2s is to estimate TWFE models without running into the
problem of staggered treatment adoption.

## Installation

You can install did2s from github with:

    devtools::install_github("kylebutts/did2s")

## Two-stage Difference-in-differences (Gardner 2021)

Researchers often want to estimate either a static TWFE model,

<img src="man/figures/twfe.png" width="400px" height="100%">

where *μ*<sub>*i*</sub> are unit fixed effects, *μ*<sub>*t*</sub> are
time fixed effects, and *D* <sub>it</sub> is an indicator for receiving
treatment, or an event-study TWFE model

<img src="man/figures/es.png" width="450px" height="100%">

where *D* <sub>it</sub><sup>k</sup> are lag/leads of treatment (k
periods from initial treatment date). Sometimes researches use variants
of this model where they bin or drop leads and lags.

However, running OLS to estimate either model has been shown to not
recover an average treatment effect and has the potential to be severely
misleading in cases of treatment effect heterogeneity (Borusyak,
Jaravel, and Spiess 2021; Callaway and Sant’Anna 2018; Chaisemartin and
D’Haultfoeuille 2019; Goodman-Bacon 2018; Sun and Abraham 2020).

One way of thinking about this problem is through the FWL theorem. When
estimating the unit and time fixed effects, you create a residualized
*ỹ* <sub>it</sub> which is commonly said to be “the outcome variable
after removing time shocks and fixed units characteristics”, but you
also create a residulaized *D̃* <sub>it</sub> or *D̃*
<sub>it</sub><sup>k</sup>. To simplify the literature, this residualized
treatment indicators is what creates the problem of interpreting *τ* or
*τ*<sup>*k*</sup>, especially when treatment effects are heterogeneous.

That’s where Gardner (2021) comes in. What Gardner does to fix the
problem is quite simple: estimate *μ*<sub>*i*</sub> and
*μ*<sub>*t*</sub> seperately so you don’t residualize the treatment
indicators. In the absence of treatment, the TWFE model gives you a
model for (potentially unobserved) untreated outcomes

<img src="man/figures/twfe_count.png" width="350px" height="100%">

Therefore, if you can ***consistently*** estimate *y* <sub>it</sub> (0),
you can impute the untreated outcome and remove that from the observed
outcome *y* <sub>it</sub>. The value of *y* <sub>it</sub> $ - $
<sub>it</sub> (0) should be close to zero for control units and should
be close to *τ* <sub>it</sub> for treated observations. Then, regressing
*y* <sub>it</sub> $ - $ <sub>it</sub> (0) on the treatment variables
should give unbiased estimates of treatment effects (either static or
dynamic/event-study). This is the same logic as the new paper Borusyak,
Jaravel, and Spiess (2021)

The steps of the two-step estimator are:

1.  First estimate *μ*<sub>*i*</sub> and *μ*<sub>*t*</sub> using
    untreated/not-yet-treated observations, i.e. the subsample with *D*
    <sub>it</sub>  = 0. Residualize outcomes:

<img src="man/figures/resid.png" width="350px" height="100%">

1.  Regress *ỹ* <sub>it</sub> on *D* <sub>it</sub> or *D*
    <sub>it</sub><sup>k</sup>’s to estimate the treatment effect *τ* or
    *τ*<sup>*k*</sup>’s.

Some notes:

### Standard Errors

First, the standard errors on *τ* or *τ*<sup>*k*</sup>’s will be
incorrect as the dependent variable is itself an estimate. This is
referred to the generated regressor problem in econometrics parlance.
Therefore, Gardner (2021) has developed a GMM estimator that will give
asymptotically correct standard errors. Details are left to the paper,
but are implemented in the R package

### Anticipation

Second, this procedure works so long as *μ*<sub>*i*</sub> and
*μ*<sub>*t*</sub> are ***consistently*** estimated. The key is to use
only untreated/not-yet-treated observations to estimate the fixed
effects. For example, if you used observations with *D* <sub>it</sub> $
= 1$, you would attribute treatment effects *τ* as “fixed
characteristics” and would combine *μ*<sub>*i*</sub> with the treatment
effects.

The fixed effects could be biased/inconsistent if there are anticipation
effects, i.e. units respond before treatment starts. The fix is fairly
simple, simply “shift” treatment date earlier by as many years as you
suspect anticipation to occur (e.g. 2 years before treatment starts) and
estimate on the subsample where the shifted treatment equals zero. The R
package allows you to specify the variable *D* <sub>it</sub>, if you
suspect anticipation, provide the shifted variable to this option.

### Covariates

This method works with pre-determined covariates as well. Augment the
above step 1. to include *X*<sub>*i*</sub> and remove that from *y*
<sub>it</sub> along with the fixed effects to get *ỹ* <sub>it</sub>.

## R Package

I have created an R package with the help of John Gardner to estimate
the two-stage procedure. To install the package, run the following:

    devtools::install_github("kylebutts/did2s")

To view the documentation, type `?did2s` into the console.

The main function is `did2s` which estimates the two-stage did
procedure. This function requires the following options:

-   `yname`: the outcome variable
-   `first_stage_formula`: formula for first stage, can include fixed
    effects and covariates, but do not include treatment variable(s)!
-   `treat_formula`: This should be the treatment variable or in the
    case of event studies, treatment variables.
-   `treat_var`: This has to be the 0/1 treatment variable that marks
    when treatment turns on for a unit. If you suspect anticipation, see
    note above for accounting for this.
-   `cluster_var`: Which variables to cluster on
-   `n_bootstraps`: How many clustered bootstraps to perform for
    standard errors

did2s returns a list with two objects:

1.  fixest estimate for the second stage with corrected standard errors.

### TWFE vs. Two-Stage DID Example

I will load example data from the package and plot the average outcome
among the groups. Here is one unit’s data:


    library(tidyverse)
    #> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
    #> ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    #> ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    #> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    #> ✓ readr   1.4.0     ✓ forcats 0.5.1
    #> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    #> x dplyr::filter() masks stats::filter()
    #> x dplyr::lag()    masks stats::lag()
    library(did2s)
    library(fixest)
    library(rmarkdown)

    # Load theme
    source("https://raw.githubusercontent.com/kylebutts/templates/master/ggplot_theme/theme_kyle.R")
    #> Loading required package: showtext
    #> Loading required package: sysfonts
    #> Loading required package: showtextdb

    # Load Data from R package
    data("df_het")

    # One observation
    df_het %>% select(unit, g, year, treat, rel_year, dep_var) %>% head(n = 31) %>% pander::pander()

| unit |  g   | year | treat | rel\_year | dep\_var |
|:----:|:----:|:----:|:-----:|:---------:|:--------:|
|  1   | 2000 | 1990 | FALSE |    -10    |  4.738   |
|  1   | 2000 | 1991 | FALSE |    -9     |  4.352   |
|  1   | 2000 | 1992 | FALSE |    -8     |  3.108   |
|  1   | 2000 | 1993 | FALSE |    -7     |  6.473   |
|  1   | 2000 | 1994 | FALSE |    -6     |   7.35   |
|  1   | 2000 | 1995 | FALSE |    -5     |   6.33   |
|  1   | 2000 | 1996 | FALSE |    -4     |  5.729   |
|  1   | 2000 | 1997 | FALSE |    -3     |  6.651   |
|  1   | 2000 | 1998 | FALSE |    -2     |  9.263   |
|  1   | 2000 | 1999 | FALSE |    -1     |  5.737   |
|  1   | 2000 | 2000 | TRUE  |     0     |  5.981   |
|  1   | 2000 | 2001 | TRUE  |     1     |  7.501   |
|  1   | 2000 | 2002 | TRUE  |     2     |  6.519   |
|  1   | 2000 | 2003 | TRUE  |     3     |  7.725   |
|  1   | 2000 | 2004 | TRUE  |     4     |  6.123   |
|  1   | 2000 | 2005 | TRUE  |     5     |  6.874   |
|  1   | 2000 | 2006 | TRUE  |     6     |  9.367   |
|  1   | 2000 | 2007 | TRUE  |     7     |  9.811   |
|  1   | 2000 | 2008 | TRUE  |     8     |  9.136   |
|  1   | 2000 | 2009 | TRUE  |     9     |  10.67   |
|  1   | 2000 | 2010 | TRUE  |    10     |  6.404   |
|  1   | 2000 | 2011 | TRUE  |    11     |   6.77   |
|  1   | 2000 | 2012 | TRUE  |    12     |  8.506   |
|  1   | 2000 | 2013 | TRUE  |    13     |  7.379   |
|  1   | 2000 | 2014 | TRUE  |    14     |  7.088   |
|  1   | 2000 | 2015 | TRUE  |    15     |  7.383   |
|  1   | 2000 | 2016 | TRUE  |    16     |  6.786   |
|  1   | 2000 | 2017 | TRUE  |    17     |  9.125   |
|  1   | 2000 | 2018 | TRUE  |    18     |  8.458   |
|  1   | 2000 | 2019 | TRUE  |    19     |  6.371   |
|  1   | 2000 | 2020 | TRUE  |    20     |  5.908   |

Here is a plot of the average outcome variable for each of the groups:

    # Plot Data 
    df_avg <- df_het %>% 
      group_by(group, year) %>% 
      summarize(dep_var = mean(dep_var), .groups = 'drop')

    # Get treatment years for plotting
    gs <- df_het %>% 
      filter(treat == TRUE) %>% 
      pull(g) %>% unique()
        
        
    ggplot() + 
        geom_line(data = df_avg, mapping = aes(y = dep_var, x = year, color = group), size = 1.5) +
        geom_vline(xintercept = gs - 0.5, linetype = "dashed") + 
        theme_kyle(base_size = 16) +
        theme(legend.position = "bottom") +
        labs(y = "Outcome", x = "Year", color = "Treatment Cohort") + 
        scale_y_continuous(expand = expansion(add = .5)) + 
        scale_color_manual(values = c("Group 1" = "#d2382c", "Group 2" = "#497eb3", "Group 3" = "#8e549f")) 

<div class="figure">

<img src="man/figures/README-plot-df-het-1.png" alt="Example data with heterogeneous treatment effects" width="100%" />
<p class="caption">
Example data with heterogeneous treatment effects
</p>

</div>

### Estimate Two-stage Difference-in-Differences

First, lets estimate a static did:


    # Static
    static <- did2s(df_het, 
                    yname = "dep_var", first_stage_formula = ~i(state) + i(year), 
                    treat_formula = ~i(treat), treat_var = "treat", 
                    cluster_var = "state")
    #> → Starting 250 bootstraps at cluster level: state

    fixest::esttable(static)
    #>                              static
    #> Dependent Var.:                 adj
    #>                                    
    #> (Intercept)     1.78e-15 (6.01e-14)
    #> treat = TRUE      2.380*** (0.0278)
    #> _______________ ___________________
    #> S.E. type                    Custom
    #> Observations                 31,000
    #> R2                          0.28957
    #> Adj. R2                     0.28955

Then, let’s estimate an event study did:


    # Event Study
    es <- did2s(df_het,
                yname = "dep_var", first_stage_formula = ~i(state) + i(year), 
                treat_formula = ~i(rel_year), treat_var = "treat", 
                cluster_var = "state")
    #> → Starting 250 bootstraps at cluster level: state

    fixest::esttable(es)
    #>                                es
    #> Dependent Var.:               adj
    #>                                  
    #> (Intercept)       0.0495 (0.0797)
    #> rel_year = -19    0.1055 (0.1180)
    #> rel_year = -18   -0.0065 (0.1214)
    #> rel_year = -17    0.0303 (0.1142)
    #> rel_year = -16    0.0529 (0.1126)
    #> rel_year = -15    0.1670 (0.1160)
    #> rel_year = -14    0.1213 (0.1096)
    #> rel_year = -13    0.0445 (0.1162)
    #> rel_year = -12    0.0404 (0.1166)
    #> rel_year = -11    0.1482 (0.1130)
    #> rel_year = -10    0.0458 (0.1105)
    #> rel_year = -9     0.0018 (0.0993)
    #> rel_year = -8     0.0383 (0.0994)
    #> rel_year = -7     0.1048 (0.0928)
    #> rel_year = -6    -0.0274 (0.0999)
    #> rel_year = -5    -0.0143 (0.0994)
    #> rel_year = -4    -0.1003 (0.0963)
    #> rel_year = -3    -0.0589 (0.0963)
    #> rel_year = -2    -0.0406 (0.0890)
    #> rel_year = -1     0.0684 (0.0930)
    #> rel_year = 0    1.678*** (0.1290)
    #> rel_year = 1    1.703*** (0.1167)
    #> rel_year = 2    1.822*** (0.1234)
    #> rel_year = 3    1.869*** (0.1103)
    #> rel_year = 4    1.890*** (0.1204)
    #> rel_year = 5    2.096*** (0.1217)
    #> rel_year = 6    2.131*** (0.1239)
    #> rel_year = 7    2.298*** (0.1174)
    #> rel_year = 8    2.363*** (0.1175)
    #> rel_year = 9    2.570*** (0.1156)
    #> rel_year = 10   2.631*** (0.1281)
    #> rel_year = 11   2.663*** (0.1524)
    #> rel_year = 12   2.622*** (0.1454)
    #> rel_year = 13   2.606*** (0.1515)
    #> rel_year = 14   2.705*** (0.1594)
    #> rel_year = 15   2.774*** (0.1582)
    #> rel_year = 16   2.645*** (0.1409)
    #> rel_year = 17   2.847*** (0.1668)
    #> rel_year = 18   3.081*** (0.1557)
    #> rel_year = 19   3.181*** (0.1472)
    #> rel_year = 20   3.259*** (0.1510)
    #> rel_year = Inf   -0.1104 (0.0812)
    #> _______________ _________________
    #> S.E. type                  Custom
    #> Observations               31,000
    #> R2                        0.30629
    #> Adj. R2                   0.30537

And plot the results:


    pts <- broom::tidy(es) %>%
        filter(str_detect(term, "rel_year::")) %>%
        select(rel_year = term, estimate, se = std.error) %>%
        mutate(
            rel_year = as.numeric(str_remove(rel_year, "rel_year::")),
            ci_lower = estimate - 1.96 * se,
            ci_upper = estimate + 1.96 * se,
            group = "Estimated Effect"
        ) %>%
        filter(rel_year <= 8 & rel_year >= -8)

    te_true <- df_het %>%
        # Keep only treated units
        filter(g > 0) %>%
        group_by(rel_year) %>%
        summarize(estimate = mean(te + te_dynamic)) %>%
        mutate(group = "True Effect") %>%
        filter(rel_year >= -8 & rel_year <= 8)

    pts <- bind_rows(pts, te_true)

    max_y <- max(pts$estimate)

    ggplot() +
        # 0 effect
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = -0.5, linetype = "dashed") +
        # Confidence Intervals
        geom_linerange(data = pts, mapping = aes(x = rel_year, ymin = ci_lower, ymax = ci_upper), color = "grey30") +
        # Estimates
        geom_point(data = pts, mapping = aes(x = rel_year, y = estimate, color = group), size = 2) +
        # Label
        geom_label(data = data.frame(x = -0.5 - 0.1, y = max_y + 0.25, label = "Treatment Starts ▶"), label.size=NA,
                   mapping = aes(x = x, y = y, label = label), size = 5.5, hjust = 1, fontface = 2, inherit.aes = FALSE) +
        scale_x_continuous(breaks = -8:8, minor_breaks = NULL) +
        scale_y_continuous(minor_breaks = NULL) +
        scale_color_manual(values = c("Estimated Effect" = "#013ef5", "True Effect" = "#eb3f25")) +
        labs(x = "Relative Time", y = "Estimate", color = NULL, title = NULL) +
        theme_kyle(base_size = 16) +
        theme(legend.position = "bottom")
    #> Warning: Removed 17 rows containing missing values (geom_segment).

<div class="figure">

<img src="man/figures/README-plot-es-1.png" alt="Event-study plot with example data" width="100%" />
<p class="caption">
Event-study plot with example data
</p>

</div>

## References

<div id="refs" class="references hanging-indent">

<div id="ref-Borusyak_Jaravel_Spiess_2021">

Borusyak, Kirill, Xavier Jaravel, and Jann Spiess. 2021. “Revisiting
Event Study Designs: Robust and Efficient Estimation,” 48.

</div>

<div id="ref-Callaway_SantAnna_2018">

Callaway, Brantly, and Pedro H. C. Sant’Anna. 2018.
“Difference-in-Differences with Multiple Time Periods and an Application
on the Minimum Wage and Employment.” *arXiv:1803.09015 \[Econ, Math,
Stat\]*, August. <http://arxiv.org/abs/1803.09015>.

</div>

<div id="ref-deChaisemartin_DHaultfoeuille_2019">

Chaisemartin, Clement de, and Xavier D’Haultfoeuille. 2019. *Two-Way
Fixed Effects Estimators with Heterogeneous Treatment Effects*. w25904.
National Bureau of Economic Research. <https://doi.org/10.3386/w25904>.

</div>

<div id="ref-Gardner_2021">

Gardner, John. 2021. “Two-Stage Difference-in-Differences.” Working
Paper. <https://jrgcmu.github.io/2sdd_current.pdf>.

</div>

<div id="ref-Goodman-Bacon_2018">

Goodman-Bacon, Andrew. 2018. *Difference-in-Differences with Variation
in Treatment Timing*. w25018. National Bureau of Economic Research.
<https://doi.org/10.3386/w25018>.

</div>

<div id="ref-Sun_Abraham_2020">

Sun, Liyang, and Sarah Abraham. 2020. “Estimating Dynamic Treatment
Effects in Event Studies with Heterogeneous Treatment Effects,” 53.

</div>

</div>
