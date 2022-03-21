README 

### folders:

### SI14_Ortega_interpretation:
 
- Simulation of Ortega Lorenz curves to investigate what features of the income distribution the parameters capture: 
The simulation can be rerun using the code simulation_ortega_lorenz_curves.R which also produces relevant figures that can be found in the supplementary material.

- Simulation of Ortega Lorenz curves to determine relationship between Ortega parameters and 90/50, 50/10 ratios: The simulation can be rerun using the code percentile_ratios_ortega_parameters.R

- Rshiny_Ortega_LC.R provides an Rshiny tool that facilitates interpretation of the Ortega parameters. 

- plotting the derivatives of the Ortega Lorenz curve function: plots_derivatives_ortega.R

### SI10_AIC_detection: 

-  Simulation study how often AICc can detect the true data generating Lorenz curve model. In the respective code, the only variation in the code is the true income share generating model. We have three separate codes such that we can use three machines at the same time. In the resutls .csv files, V1 is the model with minimum AIC_c for that simulation run and V2 gives the number of simulated data points (actual sample size is then V2 +2, because boundary values of 0 and 1 are added) 
1. Code for Ortega being the true income share generating model: ortega_sim.R ; results: ortega_output_sim.csv
2. Code for GB2 being the true income share generating model: gb2_sim.R ; results: gb2_output_sim.csv
3. Code for Wang being the true income share generating model: wang_sim.R ; results: wang_output_sim.csv