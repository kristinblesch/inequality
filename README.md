This is the GitHub repository for the paper "Measuring Inequality Beyond the Gini Coefficient May Clarify Conflicting Findings" by Kristin Blesch, Jon M. Jachimowicz, & Oliver Hauser (2022). The paper can be divided into three parts. The respective folders each contain a specific README file that guides through reproducing our results. 
<hr style="border:2px solid gray"> </hr>

**Part 1** Lorenz curve estimation using MLE and NLS on a US county and state level. Relevant code can be found in the respective folder including .csv files with estimated Ortega parameters on a US county and state level. You can browse through the [interactive visualization](https://kristinblesch.github.io/) of these estimates, or you can have a look at the following Figure for a static preview, where Panel **A** depicts the Gini coefficient, Panel **B** Ortega parameter alpha and Panel **C** Ortega parameter gamma on a US county level. 

<img width="466" alt="us_map_figure" src="https://user-images.githubusercontent.com/48204979/159120592-f3b90233-6ba5-4bd9-8e15-02725acd61b4.png">
<hr style="border:2px solid gray"> </hr>

**Part 2** Ortega Lorenz curve simulation to investigate what features about the income distribution each of the parameters capture. We provide relevant code in this repository and an [interactive RShiny tool](https://kristinb.shinyapps.io/Rshiny_gamma_2/) to facilitate understanding the Ortega parameters and enable users to visually compare various Lorenz curves.
<img width="1089" alt="rshiny_screenshot" src="https://user-images.githubusercontent.com/48204979/159294790-48bd64b1-4f69-499e-a0d3-8502fd2bba51.png">
&nbsp;   &nbsp;   &nbsp;   &nbsp;   &nbsp;  
<hr style="border:2px solid gray"> </hr>

**Part 3** An exploratory study correlating the Ortega parameters to other county-level characteristics. Relevant code is provided in the respective folder. 
As a visualization of our main results, consider Figure 5 from the paper: 

<img width="466" alt="gini_misses_correlations" src="https://user-images.githubusercontent.com/48204979/159290789-45aea1e2-ba38-454c-98f0-381b581e5436.png">

<sub> **Figure 5**: A two-parameter Ortega approach reveals significant correlations between inequality and policy outcomes that the Gini coefficient misses. Note that the confidence level is 0.9995, using a Bonferroni correction. The figure shows the sub-sample of covariates (33 out of 100) for which the Pearson correlations between county-level variables were not significantly related to the Gini coefficient but exhibited at least one statistically significant (partial) correlation with the Ortega parameters. Abbreviations: M - male; F - female; Q - income quartile; Frac. - fraction; raceadj. - race adjusted </sub>
