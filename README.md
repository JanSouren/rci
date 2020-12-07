# rci

## About
Reliable Change Index (RCI) is a concept in measurement and assessment. The statistic addresses two concerns: (1) For a given client, is the change in scores on a specific psychometric instrument across 2 measurement points reliable; and (2) Is the extent of change large enough such that it is clinically significant? The `rci` package provides a convenient solution to RCI computations. The package includes the various RCI formulas available in the academic literature.

## Installation
To install the development version, you need to run the following code:
```
devtools::install_github("dtyk/rci")
```

## Usage
The `rci` function takes in four arguments: (1) a data frame, (2) column containing pre-test scores, (3) column containing post-test scores, and (4) the RCI formula to be used.

The formulas are:

* *JT*: Jacobson & Truax, 1991
* *CM*: Christensen & Mendoza, 1986
* *I*: Iverson et al., 2003
* *L*: Lewis et al., 2007
* *M*: McSweeney et al., 1993
