# Development History

This file documents additional information not yet ported to, nor ever very likely to be ported to, the main R documentation.   

# Package changes

## v. 1.1
- Start incrementing and reporting version numbers
- Removed functional response code (now part of [frair][frair])

## v. 1.2
- Added error_bar

# Specific function history

## bb_parse.R
```
v. 1.0 	2013-01-06, DP and GB
		Initial development.
v. 1.1	2013-01-14, DP
		Major refactoring for package.  Name change from parse.bb.ca.
v. 1.2	2013-03-13, GB
		Updated the column name for 'Date' (closes issue #2).
```

## bb_plot_ca.R
```
v. 1.0	2012-12-30, GB and DP 
		Initial coding and development.
v. 1.1	2013-01-06, DP
		Refactoring for external parsing function.
v. 1.2	2013-01-10, GB
		Added disturbance as an input so as has timepoints with mine water discharge or undermining could be highlighted in plot.
v. 1.3	2013-01-14, DP
		Significant refactoring for inclusion in package.  Name change from plot.bb.ca.
v. 1.4	2013-03-13, GB (also DP)
		Fixed x-axis labelling issue (closes issue #1)
```

## bb_plot_spp_rich.R
```
v. 1.0 	2013-01-03, GB
		Initial development.
v. 1.1	2013-01-14, DP
		Major refactoring for package.  
		Code cleanup. Name change from plot.bb.spp.rich
		Proportional width of PNG based on number of sampling units.
```

## pim_read.R
```
v. 1.0	2013-02-07, GB and DP
		PIMs analysis workflow.
		Starting point: field data sheet.
		End point: data in long format.
v. 1.1	2013-02-27, GB
		Modifications to the output file (order of columns).
		Adding three new columns: Year, Season, Method Code 
		to the alldata and allmeta data output files.
v. 1.2	2013-03-07 GB
		7/03/13
		Corrected the output: Species/steps now match with strata/condition!
		Minor change: CJ no longer wants season or year, so have removed!
```

[frair]: https://github.com/dpritchard/frair
