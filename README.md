This is the GitHub repository for the paper "Measuring Inequality Beyond the Gini Coefficient May Clarify Conflicting Findings" by Blesch et al. (2022) The  paper can be divided into three parts. The respective folders each contain a specific README file that guides through reproducing our results. 

1st Part. Lorenz curve estimation using MLE and NLS on a US county and state level. Relevant code can be found in the respective folder including .csv files with estimated Ortega parameters on a US county and state level. You can browse through the [interactive visualization](https://kristinblesch.github.io/) of these estimates, or you can have a look at this Figure for a static preview:
<img width="466" alt="us_map_figure" src="https://user-images.githubusercontent.com/48204979/159120592-f3b90233-6ba5-4bd9-8e15-02725acd61b4.png">

2nd Part. Ortega Lorenz curve simulation to investigate what features about the income distribution each of the parameters capture. We provide relevant code in this repository and an [interactive RShiny tool](https://kristinb.shinyapps.io/Rshiny_gamma_2/) to facilitate understanding the Ortega parameters. 

3rd Part. An exploratory study correlating the Ortega parameters to other county-level characteristics. Relevant code is provided in the respective folder. 
