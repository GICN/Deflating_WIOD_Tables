# Deflating WIOD tables

This project deflates the input-output tables of World Input-Output Database (WIOD) released in 2016, using the WIOD sectoral price deflators released in February 2018. 

This dabatase provides input-output tables from 2008 to 2014 for 43 countries, plus the rest of the world. Data for 56 sectors are classified according to the International Standard Industrial Classification revision 4 (ISIC Rev. 4). 
The tables adhere to the 2008 version of the SNA.

This code is written in R.

## Executing code

1. Copy the WIOD tables, in R format, to the folder `inputs/WIOD_Nov16/R_Files`. The WIOD tables can be found at the url: http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip.
2. Go to `code` folder and execute the file `Deflate_tables.R` to deflate the WIOD input-output tables.
3. For the documentation, execute `checks.R`, then go to the documentation folder and knit `documentation.Rmd` file.
