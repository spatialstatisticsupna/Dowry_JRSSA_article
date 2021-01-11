# R code

This folder contains the necessary R code to fit the spatio-temporal models described in Vicente et al. (2020), and to reproduce the results.

Slight modifications have been introduced in order to be compatible with the 20.01.08 testing version of INLA.
More precisely, we eliminate the redundant constraints in the extraconstr argument of the INLA::inla() function.

The [data_JRSSA.RData](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/data_JRSSA.RData) file contains the following R objects:

- ```data```: contains the data set used. It is a dataframe with the following variables,
	- **dist**: Districts
	- **year**: Year (2001:2014)
	- **state**: Satte (Uttar Pradesh)
	- **obs**: Number of dowry deaths
	- **pop_linear**: Female population between 15 and 49 years (linear interpolation)
	- **x1**: Sex ratio (Number of women per 1000 men)
	- **x5**: Murder (per 100000 people)
	- **x6**: Burglary (per 100000 people) (is the entering of a building or residence with the intention to commit a theft or any felonious crime) 

- ```carto_up```: SpatialPolygonDataFrame object with the cartography of the 70 districts (year 2001) of Uttar Pradesh


The file [fit_models_JRSSA.R](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/fit_models_JRSSA.R) allows you to fit all the models described in the paper.


[reproduce_paper_JRSSA.R](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/reproduce_paper_JRSSA.R) allows to reproduce tables and figures in Section 4 of the paper and in the supplementary material once the file [fit_models_JRSSA.R](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/fit_models_JRSSA.R) has been executed.
