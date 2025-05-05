# ** QAQC Shiny App **
## Overview  
This application provides an interactive web interface for examining and cleaning time-series stream gauge data. Users can visualize, identify, and replace erroneous measurements with `NA` values, and download both the original and cleaned datasets for further analysis.

## Purpose 
Stream gauge data collected at the Hubbard Brook Experimental Forest are prone to errors from natural debris like ice, leaves, and branches. Existing tools for cleaning these datasets are outdated and inefficient. This app was built to streamline the workflow with modern interactive features, reducing human error and time spent on manual correction.

## Link to Use Data  
Researchers can upload their own `.csv` files via the app. To obtain official stream data, visit: [https://hubbardbrook.org/data/dataset/](https://hubbardbrook.org/data/dataset/)

## App Features  
- Upload `.csv` file of time-series data  
- Select data column(s) to clean and reference/comparison column(s)  
- Interactive plot generation  
- Timeline adjustment with zoom and pan tools  
- Brushing to select and edit suspicious data points  
- Replace values with `NA`  
- Save and download cleaned datasets  
- Export includes both original and modified values for transparency  

## How Does It Work?

1.  Upload a `.csv` file containing time-series data\
2.  Select the columns to inspect and optionally choose comparison columns for reference\
3.  An interactive plot is generated\
4.  Users adjust the timeline and zoom levels to focus on specific periods\
5.  Suspect data points can be selected (brushed) and marked as `NA`\
6.  Users can save changes and download a cleaned dataset with edits preserved

## How to Install

This app runs in [R](https://www.r-project.org/) using the **Shiny** package.\
To install required packages:

``` r
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "shinyWidgets", "plotly"))
```

## How to Run

First, download the code from online servers

## Known Issues

-   Large files may slow down rendering.
-   Brushing and zooming may occasionally overlap in behavior.
-   Syncing between multiple plots can sometimes lag slightly.

## Author

Developed by students at Virginia Tech University, Alex Turse and Claire Porter.
