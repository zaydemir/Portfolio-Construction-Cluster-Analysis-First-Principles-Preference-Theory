# Portfolio Construction Cluster Analysis First-Principles Preference Theory
This repository contains the code of the "Portfolio Construction Using First Principles Preference Theory and Machine Learning" article that was published in the Journal of Financial Data Science. The paper is accessible through this link:

https://jfds.pm-research.com/content/2/4/105.short

There are three R scripts: R_clusterAnalysis.R, R_EM_algorithm.R and R_NIG_Simulations_cloud.R. I will next briefly outline the purpose of each of these scripts.

# Machine Learning Based Cluster Analysis of Randomized Portfolios (R_clusterAnalysis.R)
This script performs a cluster analysis of randomized portfolios based on sector returns over a class of Fixed Income and Equity returns. The purpose of this script is to identify classes of portfolios that can be distinguished from one another based on their return characteristics defined by various moments of their return distributions, such as cumulative return, information ratio, skew, kurtosis, or timing of loss.

We employ both non-spherical (e.g. k-Means and hierarchical) and spherical (e.g. dbscan, optics, hdbscan) cluster techniques and discuss their validity based on various tests and graphical inspection. We address the portfolio selection problem based on utility theory. We specify various utility functions for that purpose and discuss their respective relevance and implications. The utility specificiations are Prospect Theory, CRRA including log-utility, and quadratic utility.

The table below summarizes the various tests and visual inspections conducted and outputted by this script:


Purpose	Output File Name
Determine the optimal number of clusters. 	R_NonSphericalOptimalNCluster_2000_norm.pdf
Multiple statistics used (within sum of squares, gap, silhouette width etc).	
Will be used as input for k-means and hierarchical cluster analysis.	
Determine wheter we can apply cluster analyses to this data	hopkinsStat.csv
Visual inspection of k-Means and HC clusters	R_nonSphericalCluster_inspection_1000_norm.pdf
Visual inspection of dbscan clusters	R_SphericalCluster_inspection_10000_norm.pdf
Portfolio permance by cluster technique type and various performance metrics.	R_folioPerformance_inspection_10000_norm_CPTUtility_100.pdf
User can choose the utility specification and rerun the report.	
XLS Worksheet with 3 tabs	R_folioCompositionSortedByUtilityCPTUtility_100.xlsx
Tab "Number of Positions": number of asset classes in the portfolio by clustering dimensions.	
Tab "FI vs Equity": portfolio split into FI and Equity by clustering method and dimension.	
Tab "FI Composition - FTQ - Rates": portfolio breakdown within FI sectors.	
User can choose the utility specification and rerun the report.	
Visual inspection of sector weights and utility rendered by clustering technique.	R_folioExposures_inspection_10000_norm_CPTUtility_100.pdf
User can choose the utility specification and rerun the report.	
Cluster similarity tests based on various methods (Rand, HA, FM, Jaccard) for each cluster method	clusterSimilarityTests.csv
Portfolio sector allocations as averages for respective clusters from the randomized samples.	R_optimalSectorWeights.csv
By cluster method, cluster dimensions, utility specification.	
![image](https://user-images.githubusercontent.com/66026542/205455183-3ea4fedb-73d4-4354-9bce-ef75180b56be.png)




Lastly, we provide a truncated data file of monthly total returns by the sectors that we included in our analysis for two months only for copyright reasons. Inspection of this file should, however, help anyone who is interested to run the script to get an idea of what the data inputs look like and create their own datasets. It's important to note that we are analyzing financial data in the form of total returns.

# Parameter Estimation of Multivariate Stochastic Processes With Skew and Fat Tails And Portfolio Simulations On Local Machine (R_NIG_EM_algorithm.R)
This script reads the total return data that was used to run the cluster analysis, and estimates the parameters of a multivariate NIG model (normal inverse Gaussian) with the EM algorithm (expectation-maximization algorithm).

Based on the parameter estimates, we simulate multivariate time series and apply those to various clustered portfolio definition in a Monte Carlo setting. We track in-sample and out-of-sample portfolio metrics associated with each cluster portfolio and each utility specification. We set the default parameters such as the number of Monte Carlo simulation runs to very small number for the user's familiarization purposes. For a fully fledged Monte Carlo analysis, we recommend to leverage on cloud computing exploiting parallel processing.

# Cloud-based Monte Carlo Portfolio Simulation Based the Multivariate Normal Inverse Gaussian Processes (R_NIG_Simulations_cloud.R)
Run the Monte Carlo simulations based on selected cluster analysis based portfolios and simulated multivariate NIG processes on the cloud. Tracking of same metrics as in the previous script above on the local machine (R_NIG_EM_algorithm.R).



