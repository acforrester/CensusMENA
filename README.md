
# ReadMe

This is a replication archive for my 2023 Cato Institute Briefing paper ["Middle Eastern or North African in U.S. Government Surveys: A Preview of MENA Demographics."](https://www.cato.org/briefing-paper/middle-eastern-or-north-african-us-government-surveys-preview-mena-demographics)

> Forrester, A.C. 2023. Middle Eastern or North African in U.S. Government Surveys: A Preview of MENA Demographics. *Briefing Paper No. 166*. Washington, DC: Cato Institute.

I subsequently wrote two blog posts on [*Cato at Liberty*](https://www.cato.org/blog)
- An [introductory piece](https://www.cato.org/blog/there-were-38-million-middle-eastern-or-north-africans-united-states-2021)
- Some [confirmation from U.S. Census Bureau analysis](https://www.cato.org/blog/census-bureau-analysis-supports-catos-mena-analysis-0)

## Usage

### Workspace

``` r
sessionInfo()
#> R version 4.1.2 (2021-11-01)
#> Platform: x86_64-apple-darwin17.0 (64-bit)
#> Running under: macOS Big Sur 10.16
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.0.dylib
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.1.2  fastmap_1.1.0   cli_3.6.1       tools_4.1.2    
#>  [5] htmltools_0.5.4 rstudioapi_0.13 yaml_2.3.7      rmarkdown_2.20 
#>  [9] knitr_1.42      xfun_0.36       digest_0.6.31   rlang_1.1.0    
#> [13] evaluate_0.20
```

### Scripts

- [000-libraries.R](code/000-libraries.R)
  - Load/install all packages and set some options
- [001-data-prep.R](code/001-data-prep.R)
  - Load the data, clean it, and setup databases
- [002-demographic.R](code/002-demographic.R)
  - Tabulate the selected demographic variables
- [003-economic.R](code/003-economic.R)
  - Tabulate the selected economic variables
- [004-export-data.R](code/004-export-data.R)
  - Combine the tabular data and prep them for export
