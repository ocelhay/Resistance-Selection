withMathJax("

$$C(t) = \frac{dose.k_{a}.F_{a}}{V.k_{a} - CL} \Big[ \exp{(-\frac{CL}{V}.t)}-\exp{(-k_{a}.t)} \Big]$$

- $C(t)$ is the concentration of the drug in body.
- $t = 0$ is the time of ingestion of a dose $dose$ (drug quantity in the gut).
- $k_{a}$ is the absorption parameter. (unit = / hour)
- $F_{a}$ is the fraction absorbed (bioavailability)
- $V$ is the volume of drug in body. (unit = liter) https://en.wikipedia.org/wiki/Volume_of_distribution 
- $CL$ is the clearance rate (unit = L/hour)

")


span(strong("Absorption: "), "absorption paramters", withMathJax("$$k_{a}$$")),
                              withMathJax("The half life is $$t_{1/2} = \\frac{0.693.V}{CL}$$"),