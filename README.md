# Simario features #

  * Perform discrete-time dynamic simulation, i.e.: transform a single set of micro-units
    * transformations can occur each iteration
    * transformations include results from
      * logistic,  binomial, Poisson, negative binomial, and normal regression models with coefficients specified via a file
      * transformations according to discrete probabilities specified in code or a file
  * Generate descriptive statistics from the results of each iteration including frequencies, means, quantiles, and summaries
    * Statistics can be generated for the whole population, or subsets
    * Statistics can be grouped by base variables (ie: variables that don’t change during the simulation)
  * Perform multiple simulation runs and average tracked descriptive statistics across multiple runs
  * Scenario testing via the modification of simulation variables so the flow-on effects can be observed
    * continuous variables can be modified before the simulation begins
    * categorical variables can be modified before the simulation begins, or during the simulation for specific iterations
  * For details on performance and limits see PerformanceLimits
  * Available as an R package

# Limitations / further work #
  * Areas of development not yet explored:
    * Scenario testing via the modification of model parameters, rather than simulation variables
    * Continuous time simulation and simulation with event queues
    * Simulation with large populations
    * Simulation with more than one population at a time
    * Calibration and alignment

Developed by the [Centre of Methods and Policy Application in the Social Sciences (COMPASS)](http://www.compass.auckland.ac.nz/) within the Faculty of Arts of The University of Auckland. A component of the [New Zealand Social Science Data Service (NZSSDS)](http://www.nzssds.org.nz/). Supported by the [Health Research Council of New Zealand (HRC)](http://www.hrc.govt.nz/) and the [Science and Innovation (MSI) section of the Ministry of Business, Innovation & Employment (MBIE)](http://www.msi.govt.nz/).
