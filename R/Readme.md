# R code

This folder contains the necessary R code to fit the spatio-temporal models described in Vicente et al. (2019), and to reproduce the results.

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

- ```carto_india```: SpatialPolygonDataFrame object with the cartography of the 33 states (without islands) of India

- ```carto_up```: SpatialPolygonDataFrame object with the cartography of the 70 districts (year 2001) of Uttar Pradesh

- ```rates_india```: crude rates (per 100,000 women between 15 and 49 years old) in India between 2001 and 2014


The file [fit_models_JRSSA.R](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/fit_models_JRSSA.R) allows you to fit all the models described in the paper.


[reproduce_paper_JRSSA.R](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/reproduce_paper_JRSSA.R) permit to reproduce the results given in the paper, from the resultados obtained in [fit_models_JRSSA.R](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/fit_models_JRSSA.R).
