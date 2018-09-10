---
title: "Play with Maps"
runtime: shiny
output: html_document
---


  
  ```
  ## Loading required package: wrapr
  ```
  
  ```
  ## Loading required package: data.table
  ```
  
  ```
  ## data.table 1.10.4.3
  ```
  
  ```
  ##   The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way
  ```
  
  ```
  ##   Documentation: ?data.table, example(data.table) and browseVignettes("data.table")
  ```
  
  ```
  ##   Release notes, videos and slides: http://r-datatable.com
  ```
  
  ```
  ## 
  ## Attaching package: 'data.table'
  ```
  
  ```
  ## The following object is masked from 'package:wrapr':
  ## 
  ##     :=
  ```
  
  ```
  ## Loading required package: seas
  ```
  
  ```
  ## Loading required package: magrittr
  ```
  
  ```
  ## Loading required package: tidyverse
  ```
  
  ```
  ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
  ```
  
  ```
  ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
  ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
  ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
  ## ✔ readr   1.1.1     ✔ forcats 0.3.0
  ```
  
  ```
  ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
  ## ✖ dplyr::between()   masks data.table::between()
  ## ✖ tidyr::extract()   masks magrittr::extract()
  ## ✖ dplyr::filter()    masks stats::filter()
  ## ✖ dplyr::first()     masks data.table::first()
  ## ✖ dplyr::lag()       masks stats::lag()
  ## ✖ dplyr::last()      masks data.table::last()
  ## ✖ purrr::set_names() masks magrittr::set_names()
  ## ✖ purrr::transpose() masks data.table::transpose()
  ```
  
  ```
  ## Loading required package: broom
  ```
  
  ```
  ## Loading required package: knitr
  ```
  
  ```
  ## Loading required package: kableExtra
  ```
  
  ```
  ## Loading required package: pander
  ```
  
  ```
  ## Loading required package: lubridate
  ```
  
  ```
  ## 
  ## Attaching package: 'lubridate'
  ```
  
  ```
  ## The following objects are masked from 'package:data.table':
  ## 
  ##     hour, isoweek, mday, minute, month, quarter, second, wday, week, yday, year
  ```
  
  ```
  ## The following object is masked from 'package:base':
  ## 
  ##     date
  ```
  
  ```
  ## Loading required package: RPostgreSQL
  ```
  
  ```
  ## Loading required package: DBI
  ```
  
  ```
  ## Loading required package: tmap
  ```
  
  ```
  ## Loading required package: tmaptools
  ```
  
  ```
  ## Loading required package: grid
  ```
  
  ```
  ## Loading required package: readstata13
  ```
  
  ```
  ## Loading required package: ordinal
  ```
  
  ```
  ## 
  ## Attaching package: 'ordinal'
  ```
  
  ```
  ## The following object is masked from 'package:dplyr':
  ## 
  ##     slice
  ```
  
  ```
  ## Loading required package: DataCache
  ```
  
  ```
  ## 
  ## Attaching package: 'DataCache'
  ```
  
  ```
  ## The following object is masked from 'package:lubridate':
  ## 
  ##     now
  ```
  
  ```
  ## Loading required package: shiny
  ```
  
  ```
  ## 
  ## Attaching package: 'shiny'
  ```
  
  ```
  ## The following object is masked from 'package:pander':
  ## 
  ##     p
  ```

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" for="target_category">target category</label>
<input class="js-range-slider" id="target_category" data-min="1" data-max="4" data-from="1" data-step="1" data-grid="true" data-grid-num="3" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="33.3333333333333" data-data-type="number"/>
</div><!--/html_preserve-->

<!--html_preserve--><div id="out7c38be9ff68d6ac7" class="shiny-plot-output" style="width: 100% ; height: 400px"></div><!--/html_preserve-->


