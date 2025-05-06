# ** QAQC Shiny App **
## Overview  
This application gives users an easy-to-use web interface for looking at and fixing time-series stream gauge data. Users can view the data in different ways to spot errors or strange values. They can mark those data points and replace them with `NA` values to keep the data structure while showing those values are not usable. The app also lets users compare the original and cleaned data side by side to make sure the changes look right. Both the original and cleaned datasets can be downloaded for use in other programs or reports.


## Purpose 
Stream gauge data collected at the Hubbard Brook Experimental Forest are often affected by natural debris such as ice, leaves, and branches, which can cause spikes, dropouts, or other errors in the recorded measurements. These errors make it difficult to analyze long-term trends or perform accurate hydrologic modeling without first cleaning the data. While there are existing tools available for this task, many are outdated, lack user-friendly interfaces, and require significant manual effort, which increases the chances of human error and slows down the overall data processing workflow. This application was developed to address these challenges by providing a modern, interactive platform that simplifies and speeds up the data cleaning process. With tools for visual inspection, point-and-click error marking, zooming, and automated data previewing, users can more easily identify and fix problems in the dataset. As a result, the app helps improve data quality while reducing the time and effort required for manual correction.


## Link to Use Data  
Users and researchers can upload their own `.csv` files via the app. To obtain official stream data, visit: [https://hubbardbrook.org/data/dataset/](https://hubbardbrook.org/data/dataset/)

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

## How to Install & Run

Download the code from the GitHub repository.\

This app runs in [R](https://www.r-project.org/) using the **Shiny** package.\
The application will install all required packages on first run, so it may take longer than subsequent launches\


``` r
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "shinyWidgets", "plotly"))
```

## How to Run



## Known Issues

-   Large files may slow down rendering.
-   Brushing and zooming may occasionally overlap in behavior.
-   Syncing between multiple plots can sometimes lag slightly.

## Author

Developed by students at Virginia Tech University, Alex Turse and Claire Porter.
