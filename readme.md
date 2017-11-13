
##This package contains functions on data from US National Highway Traffic Safety Administration.

###Dependent packages

This package contains the functions that depends on various packages like readr, dplyr, magrittr, tidyr and graphics. Please run the following commands to install the dependent packages.


###Functions:

`fars_read`:

This is a simple function that expects a csv filename as argument, checks if the file exists and if it exists it reads the file to an object and converts it to a data frame.

`make_filename`:

This is a simple function which expects a year as an argument, converts the year to integer, constructs the file name by appending year to the filename and returns it.

`fars_read_years`:

This function that expects a list of years, for each year in the list, construct the file name. Once the file name is created, read the content of file to the dataframe, create new field year and return month and year fields from dataframe.

`fars_summarize_years`:

This function expects a list of years, reads the content from all the files of these years using the internal method fars_read_years for each year in the list and saves the result to a data frame. This dataframe is then processed to get a a final dataframe having the summary with list of months and total number of records for each year.

`fars_map_state`:

This function expects a valid state number and year. This function uses internal method "make_filename" to construct a file name, fars_read for reading the file to a dtaframe. Once the dataframe is populated, this function validates if the user passed state number exists in the dataframe and filters the records that match the state number. The coordinates in the dataframe are filtered and draws a graph based on the range of latitudes and longitudes and plots the points.

`Data in the package`

There are three files containing the accident details between 2013 and 2015. The data files are located in the "data" directory.