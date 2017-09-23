Hw01
================
Sierra Park
9/16/2017

``` r
#load data file
load("data/nba2017-salary-points.RData")

#list the available objects
ls()
```

    ## [1] "experience" "player"     "points"     "points1"    "points2"   
    ## [6] "points3"    "position"   "salary"     "team"

Research Question
=================

1 A bit of data preprocessing
=============================

``` r
salary <- round(salary / 100000, 2)
experience[experience == "R"] <- 0
experience <- strtoi(experience)
level = c("C", "SF", "PF", "SG", "PG")
label = c("center", "small_fwd", "power_fwd", "shoot_guard", "point_guard")
position <- ordered(position, levels = level, labels = label)
table(position)
```

    ## position
    ##      center   small_fwd   power_fwd shoot_guard point_guard 
    ##          89          83          89          95          85

2 Scatterplot of Points and Salary
==================================

``` r
plot(points, salary, pch =6, col= "red", cex = 1, xlab = "Points", ylab = "Salary (in millions)", main = "Scatterplot of Points and Salary")
```

![](hw01-sierra-park_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

3 Correlation between Points and Salary
=======================================

``` r
n <-  length(points)
xbar <- sum(points)/n
ybar <- sum(salary)/n
xvar <- sum((points-xbar)**2)/(n-1)
yvar <- sum((points-salary)**2)/(n-1)
xsd <- xvar**0.5
ysd <- yvar**0.5
cov <- sum((points-xbar)*(salary-ybar))/(n-1)
cor <- cov/(xsd*ysd)
```

4 Simple Linear Regression
==========================

``` r
b1 <- cor*ysd / xsd #slope of the regression
b0 <- ybar - b1*xbar #intercept of the regression
Yhat <- b0 + b1*points
b1
```

    ## [1] 0.08556726

``` r
b0
```

    ## [1] 15.09852

``` r
summary(Yhat) #summary statistics of Y hat
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   15.10   28.45   52.06   61.87   81.84  233.98

The regression equation: &gt; = \[0.0856x + 15.099\]\] The slope (0.0856) means that one unit increase in x-value (points) corresponds to 0.0856 unit increase in y-value (salary). The intercept means that when x-value is 0, meaning that the player does not score any points, the salary is still 15.09842.

``` r
yhat1 = b1 * 0 + b0
yhat2 = b1 * 100 + b0
yhat3 = b1 * 500 + b0
yhat4 = b1 * 1000 + b0
yhat5 = b1 * 2000 + b0
```

5 Plotting the regression line
==============================

``` r
plot(points, salary, col = "blue", xlab = "Points", ylab = "Salary in Hundred Thousands", main = "Regression and Lowess lines")
abline(a = b0, b = b1, col = "red", lwd = 3) #regression line
#abline(lm(salary/1000000 ~ points), col = "red")
lines(lowess(points, salary)) #lowess line
text(2400, 19000000, lwd = 2, col = "red", cex = 0.75, label = "regression")
text(2400, 29000000, cex = 0.75, label = "lowess")
```

![](hw01-sierra-park_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

``` r
obs <- salary
pred <-  b1*obs + b0
resid <- obs - pred
summary(resid) #vector of residuals 
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -15.053  -3.339  16.907  41.477  69.487 268.037

``` r
sum(resid^2) #the residual sum of squares 
```

    ## [1] 2347736

``` r
sum((obs - ybar)**2) #TSS
```

    ## [1] 1900349

``` r
1 - sum(resid^2) / sum((obs - ybar)**2) #R^2
```

    ## [1] -0.2354235

7 Exploring Position and Experience
===================================

``` r
plot(experience, salary, xlab = "Years of Experience", ylab = "Salary in Hundred Thousands", main = "Scatterplot with lowess smooth")
#lines(lowess(experience, salary))
lines(lowess(experience, salary), col = "red") #lowess line
```

![](hw01-sierra-park_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

``` r
#install.packages("scatterplot3d")
library("scatterplot3d")
shapes <- c(16, 17, 18)
shapes <- shapes[as.numeric(iris$Species)]
scatterplot3d(points, experience, salary, main = "3D Scatter Plot", color = "red")
```

![](hw01-sierra-park_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

``` r
boxplot(salary~position, xlab = "Position", ylab = "Salary in hundred thousands")
```

![](hw01-sierra-park_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png) By looking at the box plot, we know that the average salary is low, yet the range stretches out far, with a few outliers in all positions except the center position. The distribution seems to be skewed right, with a long right tail. From the scatterplot, at the extreme ends of the experience, the salary is the lowest but at the middle range, the salary is the highest. From the boxplot, position does not seem to be related with salary.

8. Comments and Reflections
===========================

-   It was hard to create graphs and calculate residuals.
-   The easy part was creating x- and y-labels.
-   Git is confusing.
-   I need help on creating the graphs.
-   I spent 5 hours on this HW.
-   The most time consuming part was understanding how residuals and the graph works.
-   I still do not understand how lowess function works.
-   Creating labels for the lines of graphs.
-   It was exciting to be able to draw 3D plots.
