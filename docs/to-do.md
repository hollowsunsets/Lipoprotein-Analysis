## June
### 6/12/2017 - 6/16/2017
- [ ] Create starting documentation for dmen-vis
- [ ] Understand logic for `app.R`
- [ ] Understand logic for `multiplot.R`
- [ ] create `scan_conform()` to generalize `scan_conform10_14` and `scan_conform7_10`
- [ ] Print error messages instead of passing NULL for invalid files
- [ ] Graph generation is recalled every time a file is passed in - make it so it only runs when all three files are present
- [ ] Make `generateCSV` dynamic - create new file based on given user name 
- [ ] Make `generateCSV` create file with similar format as given file

### 6/19/2017 - 6/23/2017

### 6/26/2017 - 6/30/2017



## July
### 7/3/2017 - 7/7/2017

### 7/10/2017 - 7/14/2017

### 7/17/2017 - 7/21/2017

### 7/24/2017 - 7/28/2017


## August
### 7/31/2017 - 8/4/2017

### 8/7/2017 - 8/11/2017

### 8/14/2017 - 8/18/2017

### 8/21/2017 - 8/25/2017


## Notes: 

### 6-14-2017

Should this be made into a tool that generates plots on scans? That would be more useful that an application specifically designed for analyzing one specific type of data. Plots can be isolated based on given parameters. I shouldn't need to completely understand the data to be able to code a useful application for this. 

Ideally, an exploratory data analysis tool.

Inputs:
* Range of rows and columns to analyze (be able to enter in a subset of data instead of having to pre-emptively adapt the file to fit the program)
	- can help with multiple formats; program doesn't work with appropriate format currently
* Multiple data files - be able to generate multiple inputs and retrieve specific columns and rows from each

Stretch:
* Multiple kinds of plots that can be generated (not just line)
* Support different file types than just CSV
* Apply operation to entire given dataset or subset of given dataset 
	- be able to save this dataset for future operations, while also preserving integrity of original

