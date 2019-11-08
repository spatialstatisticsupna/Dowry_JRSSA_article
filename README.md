# Crime against women in India: unveiling spatial patterns and temporal trends of dowry deaths in the districs of Uttar Pradesh
This repository contains the supplementary material and R code to fit the models described in the paper entitled _"Crime against women in India: unveiling spatial patterns and temporal trends of dowry deaths in the districs of Uttar Pradesh"_ (Vicente et al., 2019).

## Table of contents
- [Supplementary Material](#Supplementary-Material)
- [R code](#R-code)
- [References](#References)

# Supplementary-Material

## Description of the covariates 
- Covariates:
	- x0: Political party of the Chief Minister ruling Uttar Pradesh during the study period: Bharatiya Janata Party (BJP) during 2001; Bahujan Samaj Party (BSP) during 2002-2003 and 2007-2011; Samajwadi Party (SP) during 2004-2006 and 2012-2014 (Source: https://www.mapsofindia.com/uttar-pradesh/chief-ministers.html or https://en.wikipedia.org/wiki/List_of_chief_ministers_of_Uttar_Pradesh)
	- x1: sex ratio. Number of females per 1000 males (Source: Office of the Registrar General and  Census Commissioner, India. (http://censusindia.gov.in)
	- x2: population density (People/Km2) (Source: Office of the Registrar General and  Census Commissioner, India. http://censusindia.gov.in)
	- x3: female literacy rate (Source: Office of the Registrar General and  Census Commissioner, India. (http://censusindia.gov.in)
	- x4: per capita income referenced to year 2004 (Source: Directorate of Economics And Statistics Government Of Uttar Pradesh. (http://updes.up.nic.in)
	- x5: number of murders per 100000 inhabitants (Source: Open Government Data Platform India. https://data.gov.in)
	- x6: number of burglaries per 100000 inhabitants (Source: Open Government Data Platform India. https://data.gov.in)

- [Figure S.1.](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/figures/fig_s1.pdf) Geographical pattern of SMRs of dowry deaths (left column) for years 2001 and 2011 and spatial pattern of sex ratio (center column), and murders (right column) in the same years


## Fitted models with seven covariates
We fitted Model (2), see Vicente et al. (2019), with the seven covariates introduced in the previous section. Uniform distributions on the positive real line have been considered for the standard deviations in the ICAR and LCAR priors whereas default PC-priors have been chosen for DCAR and BYM2 priors. 
Results with loggamma priors on the log-precisions were almost identical. 
**Table S.1** displays the posterior means for the fixed effects, their standard deviations, the medians and 95% credible intervals obtained from a Type II interaction model with a RW1 prior for time and an ICAR for space. 
Only sex ratio (x1), murders (x5), and burglaries (x6) have a significant effect.

- **Table S.1.** Posterior means, standard deviations and medians of the complete set of fixed effects together with a 95\% credible interval. Results correspond to a type II interaction model with a RW1 for time and an ICAR prior for space

| 		| mean | sd  | median | 95% C.I.| 
|:---	| ---: |---: |   ---: |  :---: |
|![equation](http://www.sciweavers.org/tex2img.php?eq=%5Calpha&bc=White&fc=Black&im=gif&fs=12&ff=arev&edit=0) (x0 BJP)          |-9.918 | 0.134 | -9.917 | (-10.189;-9.653)|
|x0 (BSP)                |-0.115 | 0.139 | -0.116 | (-0.393; 0.166) |
|x0 (SP)                 |-0.119 | 0.161 | -0.120 | (-0.437; 0.208) |
|x1 (sex ratio)          |-0.098 | 0.046 | -0.098 | (-0.187; -0.007)|
|x2 (population density) |-0.027 | 0.032 | -0.027 | (-0.088; 0.036) |
|x3 (female literacy)    |-0.013 | 0.047 | -0.012 | (-0.107; 0.080)	|
|x4 (per capita income)  |-0.021 | 0.030 | -0.021 | (-0.079; 0.038) |
|x5 (murders)            | 0.086 | 0.020 |  0.087 | (0.046; 0.127) 	|
|x6 (burglaries)         | 0.059 | 0.016 |  0.059 | (0.027; 0.091) 	|


## Fitted models with sex ratio, murders, and burglaries
We fitted Model (2), see Vicente et al. (2019), with sex ratio (x1), murders (x5), and burglaries (x6) covariables. Model selection criteria (DIC, WAIC, and LS) for the complete set of models are displayed in **Table S.2**.

- **Table S.2.** Model selection criteria for different models that include covariates x1 (sex ratio), x5 (murders), and x6 (burglaries). Posterior deviance, effective number of parameters, DIC, WAIC, and logarithmic score (LS)

| delta	| xi   | gamma | Mean Post D |  pD     | DIC      | WAIC     |  LS   |
|:---	| :--- | :---  |  		---: | ---:    | ---:     | ---:     | ---:  |
| Additive 	| LCAR | RW1  | 6395.885 | 80.780  | 6476.665 | 6526.998 | 3.331 |
| 	 		| 	   | RW2  | 6398.123 | 80.721  | 6478.844 | 6529.528 | 3.333 |
| 	 		| BYM2 | RW1  | 6396.004 | 80.555  | 6476.559 | 6526.815 | 3.331 |
| 	 		|      | RW2  | 6398.945 | 80.381  | 6479.326 | 6529.932 | 3.333 |
| 	 		| DCAR | RW1  | 6395.971 | 80.576  | 6476.547 | 6526.814 | 3.331 |
| 	 		|      | RW2  | 6398.490 | 80.479  | 6478.969 | 6529.592 | 3.333 |
| 	 		| ICAR | RW1  | 6396.305 | 80.445  | 6476.749 | 6526.933 | 3.331 |
| 	 		|      | RW2  | 6398.532 | 80.399  | 6478.931 | 6529.456 | 3.333 |
| Type I 	| LCAR | RW1  | 5910.966 | 373.519 | 6284.485 | 6282.234 | 3.265 |
| 	 		| 	   | RW2  | 5910.259 | 375.852 | 6286.111 | 6283.357 | 3.267 |
| 	 		| BYM2 | RW1  | 5911.895 | 372.516 | 6284.411 | 6282.702 | 3.265 |
| 	 		|      | RW2  | 5911.295 | 375.076 | 6286.371 | 6284.193 | 3.267 |
| 	 		| DCAR | RW1  | 5911.559 | 372.843 | 6284.401 | 6282.470 | 3.265 |
| 	 		|      | RW2  | 5911.703 | 374.500 | 6286.203 | 6284.320 | 3.267 |
| 	 		| ICAR | RW1  | 5909.720 | 374.601 | 6284.321 | 6281.370 | 3.265 |
| 	 		|      | RW2  | 5909.500 | 376.450 | 6285.950 | 6282.819 | 3.267 |
| Type II 	| LCAR | RW1  | 5920.320 | 250.821 | 6171.141 | 6180.192 | 3.173 |
| 	 		|      | RW2  | 6024.166 | 188.375 | 6212.540 | 6239.436 | 3.197 |
| 	 		| BYM2 | RW1  | 5920.648 | 250.339 | 6170.987 | 6180.179 | 3.173 |
| 	 		|      | RW2  | 6011.647 | 173.036 | 6184.683 | 6209.669 | 3.183 |
| 	 		| DCAR | RW1  | 5920.348 | 250.632 | 6170.980 | 6180.063 | 3.173 |
| 	 		|      | RW2  | 6018.133 | 180.201 | 6198.335 | 6225.267 | 3.190 |
| 	 		| ICAR | RW1  | 5919.921 | 251.062 | 6170.983 | 6179.748 | 3.173 |
| 	 		|      | RW2  | 6024.504 | 187.706 | 6212.210 | 6239.283 | 3.197 |
| Type III 	| LCAR | RW1  | 6041.854 | 296.840 | 6338.694 | 6389.834 | 3.295 |
| 	 		|      | RW2  | 6040.729 | 299.279 | 6340.009 | 6391.203 | 3.297 |
| 	 		| BYM2 | RW1  | 6043.487 | 295.506 | 6338.993 | 6390.593 | 3.295 |
| 	 		|      | RW2  | 6044.852 | 296.398 | 6341.250 | 6393.687 | 3.297 |
| 	 		| DCAR | RW1  | 6044.560 | 294.635 | 6339.195 | 6391.107 | 3.295 |
| 	 		|      | RW2  | 6045.597 | 295.546 | 6341.143 | 6393.773 | 3.297 |
| 	 		| ICAR | RW1  | 6041.228 | 296.752 | 6337.981 | 6388.921 | 3.295 |
| 	 		|      | RW2  | 6041.709 | 297.898 | 6339.607 | 6391.059 | 3.296 |
| Type IV 	| LCAR | RW1  | 5961.355 | 239.326 | 6200.682 | 6220.245 | 3.191 |
| 	 		|      | RW2  | 6035.495 | 181.733 | 6217.228 | 6245.927 | 3.198 |
| 	 		| BYM2 | RW1  | 5962.362 | 238.369 | 6200.730 | 6220.316 | 3.191 |
| 	 		|      | RW2  | 6036.828 | 180.823 | 6217.651 | 6246.592 | 3.199 |
| 	 		| DCAR | RW1  | 5962.322 | 238.230 | 6200.552 | 6220.387 | 3.191 |
| 	 		|      | RW2  | 6036.165 | 181.177 | 6217.343 | 6246.174 | 3.199 |
| 	 		| ICAR | RW1  | 5961.686 | 238.813 | 6200.499 | 6219.989 | 3.191 |
| 	 		|      | RW2  | 6036.107 | 181.544 | 6217.652 | 6246.037 | 3.198 |


In Figure S.2. risk trends, standardized mortality ratios (SMRs) and credible intervals are displayed for three different districts in Uttar Pradesh: Aligarth, Kheri, and Varanasi. On the left, we show the relative risk estimates with the additive model (solid black line) and the credible intervals (grey band). We also display the SMRs (solid orange line) and the estimated relative risk estimate with the Type II interaction model (purple line). On the right, we show the relative risk estimates with the type II interaction model (solid purple line) and the credible intervals (grey band). We also display the SMRs (solid orange line) and the estimated relative risk estimate with the additive model (black line). The credible intervals for the additive model are too narrow (indicating underdispersion of the predictive distribution as suggested by the PIT histogram) and the estimated relative risks does not track the SMRs very well for Alligarth and Kheri. It is observed that the estimated relative risks with the type II interaction model track the SMRs much better and the credible intervals are wider. The estimated relative risk for the district of Varanasi are nearly identical with both models. In general, for those districts with rather flat trends, both models produce similar estimates. However, for those districts with increasing or decreasing trends, the Type II interaction model leads to more satisfactory results than the additive model.


- [Figure S.2.](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/figures/fig_s2.pdf) Relative risk temporal trends for three different districts in Uttar Pradesh: Aligarth, Kheri, and Varanasi. On the left, estimated relative risk for the additive model (solid black line) with corresponding credible bands in grey together with the SMRs (solid orange line) and the estimated relative risk with the Type II interaction model (solid purple line). On the right, estimated relative risk for the Type II interaction model (solid purple line) with corresponding credible bands in grey together with the SMRs (solid orange line) and the estimated relative risk with the additive model (solid black line)

Figure S.3. displays the estimated relative risks with a Type II interaction model with an ICAR spatial prior vs. the same Type II interaction model with LCAR, DCAR, and BYM2 spatial priors.
The estimated relative risks are identical.

- [Figure S.3.](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/figures/fig_s2.pdf) Dispersion plots of the final relative risks obtained with the Type II interaction model with an ICAR spatial prior vs. Type II interaction models with LCAR (top left), DCAR (top right), and BYM2 (bottom)


Estimated dowry deaths rates (per 100,000 women aged between 15 and 49) for districts of Aligarh, Kanpur Dehat, Kheri, Shrawasti, Sitapur, and Varanasise are displayed in Table S.3.

- **Table S.3.** Estimated incidence rate of dowry deaths by year per 100,000 women aged between 15 and 49 in districts Aligarh, Kanpur Dehat, Kheri, Shrawasti, Sitapur, and Varanasi

| District 		| 2001 | 2002 | 2003 | 2004 | 2005 | 2006 | 2007 | 2008 | 2009 | 2010 | 2011 | 2012 | 2013 | 2014 |
| :--- 			| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
|Aligarh 		| 7.552 | 6.173 | 4.584 | 5.918 | 5.865 | 7.070 | 8.678 | 9.868 | 10.560 | 10.656 | 10.768 | 10.336 | 10.822 | 10.771 |
|Kanpur Dehat 	| 11.882 | 9.608 | 6.596 | 7.934 | 7.568 | 8.698 | 8.725 | 8.435 | 7.109 | 6.613 | 6.947 | 6.567 | 6.811 | 7.155 |
|Kheri 			| 11.888 | 9.112 | 6.018 | 7.318 | 6.771 | 6.796 | 6.775 | 6.241 | 5.498 | 5.188 | 5.613 | 5.362 | 5.441 | 5.458 |
|Shrawasti 		| 3.182 | 2.669 | 2.052 | 2.608 | 2.674 | 3.102 | 3.737 | 4.181 | 4.312 | 4.715 | 5.186 | 5.145 | 5.404 | 5.273 |
|Sitapur 		| 11.046 | 9.625 | 6.428 | 8.005 | 7.011 | 6.957 | 7.309 | 7.391 | 6.948 | 5.715 | 5.693 | 5.121 | 5.119 | 4.941 |
|Varanasi 		| 4.593 | 4.038 | 2.878 | 3.443 | 2.965 | 3.337 | 3.935 | 4.047 | 3.686 | 3.523 | 3.817 | 3.722 | 3.953 | 4.022 |

# R code
R code to fit the spatio-temporal models described in the paper, and to reproduce the results, has been included [here](https://github.com/spatialstatisticsupna/Dowry_JRSSA_article/blob/master/R/).

# References
Vicente, G., Goicoa, T., Fernandez-Rasines, P., and Ugarte, M.D. (2019). Crime against women in India: unveiling spatial patterns and temporal trends of dowry deaths in the districs of Uttar Pradesh. 
```diff
- PONER REFERENCIA BIEN
```