---
title: "Report on Data 1"
output: html_document
---


# Analyse exploratoire des donn´ees
## Data Summary

Below is a table showing the number of missing values and the number of unique values for each column in Data 1.
- size: `

```
## 612 31
```

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="outabd2187b06397e2a" style="width:100%;height:auto;"></div><!--/html_preserve-->
## Class Distribution

This section presents a bar plot showing the count of rows in the data frame `df` for each class (0 and 1).

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
### relation variable categorial/churn
- there is no catgeorical vriables
### relation categorical variable churn 
- this are the correlations between each numerical variable and the class
<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="out79825ed504931fbb" style="width:100%;height:auto;"></div><!--/html_preserve-->
#### plotting the biggest 3 

```
## [1] "V14" "V12" "V17"
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 122 rows containing missing values (`geom_bar()`).
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
#### plotting the least 3 

```
## [1] "V20" "V22" "V15"
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 134 rows containing missing values (`geom_bar()`).
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
### Correlation matrix
![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
## Conclusion:
- blah blah blah
# Prediction de churn
- we are just going to use v2, v4, v8 and v11 since the dataset is very large
