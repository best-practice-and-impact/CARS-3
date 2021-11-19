![R-CMD-check](https://github.com/best-practice-and-impact/CARS-3/workflows/R-CMD-check/badge.svg)
![test-coverage](https://github.com/best-practice-and-impact/CARS-3/workflows/test-coverage/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/best-practice-and-impact/CARS-3/main/freq_tables/graph/badge.svg)](https://codecov.io/gh/best-practice-and-impact/CARS-3?branch=main)

# CARS-3

This repository contains the source code for the processing, analysis and presentation of the Coding in Analysis and Research Survey (CARS) 2021. You can see the current state of the analysis on the [live site](https://best-practice-and-impact.github.io/CARS-3/).

# Setup

You will not be able to build the site without access to the survey data. However, key functionality can be found in the carsurvey3 package.

This project uses functionality from the previous wave of the survey. You can install by running the following code in R:

```
install.packages("remotes")

remotes::install_github("best-practice-and-impact/CARS2")
```

Alternatively, you can manually download the source code from the [GitHub repository](https://github.com/best-practice-and-impact/CARS2).

To install the 2021 analysis package, run the following code with the root directory as the working directory:

```
install.packages("devtools")

devtools::install("carsurvey3")
```

If you are working on the project and have access to the API, set up CARS_TOKEN and CARS_SECRET as environment variables. Do not do this within the repository. 

If you do not work on this project but would like to test the functionality, we provide a dummy dataset in the carsurvey3 package.

# Building the site

To generate the site, open the RProject file for the project run the script main.R. If you would like to do this with the dummy dataset, assign it to an object named "data" and run the second part of the main script.

# Adding or modifying content

## Packages

To alter key functionality, make changes to the carsurvey3 or carsurvey2 packages. Note that carsurvey3 only contains functionality used to generate this year's tables and output pages. Most functionality has been reused from CARS 2020 (wave 2). These functions should be expanded but not altered, as they are used to generate the CARS 2020 site.

## Rmarkdown and HTML

The site content can be found in the rmarkdown/ directory. To add new pages to the main site, add .rmarkdown files to the rmarkdown/main/ directory.

Remember to update the navigation bar when adding new pages, by editting the _site.yml file. To make these changes, add new settings under "left", for example: 

```
navbar:
  title: "My Website"
  left:
    - text: "Home"
      href: index.html
    - text: "Page 1"
      href: page1.html
```
where text refers to the name of the page and href refers to the relative path to the page's address. 

Styles for the main pages and the template filtered pages can be found in the "style.css" files in rmarkdown/main/ and rmarkdown/summary_template.
