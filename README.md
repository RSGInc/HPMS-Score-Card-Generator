# HPMS-Score-Card-Generator
A data pipeline QA/QC and visualization tool for FHWA to use with Highway Performance Monitoring System data provided by states.

## How to create scorecards

### Import data from Socrata

Socrata is at https://datahub.transportation.gov/

Find the each dataset's JSON Endpoint by clicking the "Export" button on the right side of the page, and then the SODA API option.  The API Endpoint is shown in a box. 

Copy each dataset API Endpoint (JSON) and paste into [db_import/hpms_database_import_2020.R](db_import/hpms_database_import_2020.R)

This script downloads the data from Socrata to a local cache, then it writes the data to a stage table in MS SQL Server.  Finally, after checking the consistency of the data, it copies the data from the stage table to the production table.

### Import data from MS SQL Server

The scorecard generator runs off of data "imported" from MS SQL Server into locally stored RDS files in the `data/` directory. The import process includes a number of data cleaning and reformatting steps.  

There is one subfolder for each state and `+National` directory.  Each state has one `*.rds` file per year.  The national data is stored as one subfolder for each year, and one rds file per data item. 

The data can be imported automatically for each state as part of the scorecard generation process in [_RunBatch.R](_RunBatch.R), or the interactive app in [_RunApp.R](_RunApp.R).  Both use the function `ImportData` defined in [app/import_from_db.R](app/import_from_db.R).

### Update national data

For each data item, the scorecard shows the distribution of the current year data, the previous year data, and the national distribution of the previous year data. 

The national data can be recalculated from the previous years' imported state rds files as described above using the interactive app in [_RunApp.R](_RunApp.R) or the code below.

```{r}
# Load Code
codefiles = c(Sys.glob('app/*.R'), Sys.glob('functions/*.R'))
invisible(sapply(X =codefiles , FUN = source))

updateNation(years = 2020) # should be 2 years prior to submission deadline

```

### Create scorecards

The easiest way to create scorecards is to use [_RunBatch.R](_RunBatch.R).  It requires the following settings to manually applied in the code:

```
reimport <- TRUE                    # Should data be re-imported from the server?
year_selection <- 2020              # Data year to display
year_compare <- 2019                # Previous year for comparison
submission_deadline <- '2021-06-15' # Submission deadline for timeliness score
```

The the script can be called for all states with 
```
Rscript _RunBatch.R ALL
```

Alternatively a subset of states can be imported with

```
Rscript _RunBatch.R PA,NY,NH,VT
```

The PDF files are saved to the `output/` directory.

When all states are completed, an HTML summary file can be created using [app/HPMS_score_summary.Rmd](app/HPMS_score_summary.Rmd).

```
Rscript -e "rmarkdown::render(input = 'app/HPMS_score_summary.Rmd', output_dir='output', output_file='HPMS_summmary.html')"
```