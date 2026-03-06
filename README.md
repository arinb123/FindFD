## Summary ##
FindFD is an R package to identify valid front-door sets in directed acyclic graphs (DAGs). Using DAGs defined via dagitty, the package enables researchers to specify exposure and outcome nodes and systematically search for front-door identification strategies.

## Why this package? ##
- Legible, explicit output: mediators are printed in user-friendly format.
- Speed and accuracy as priorities.

## Example usage:

find_fd takes in parameters:

(required)
- `dag` (dagitty object) : the graph in which to find mediators
- `X` (string) : the exposure node name
- `Y` (string) : the outcome node name

(optional)
- `verbose=TRUE` (boolean) : whether to print output during intermediary steps

find_fd outputs:
- printed information about mediators
- returns a list of potential mediators as strings

#### INPUT:
```r
dag1 <- dagitty::dagitty("dag {
X -> M
M -> Y
X [pos=\"0,0\"]
  M [pos=\"1,0\"]
  Y [pos=\"2,0\"]
}")

find_fd(dag1, "X", "Y", verbose=FALSE)
```
#### OUTPUT:
```
==== Valid front-door sets ====
  {M} 
```
Explanation: M is a valid front-door between X and Y.
#### INPUT:
```r
dag2 <- dagitty::dagitty("dag {
A -> B
B -> C
U1 -> A
U1 -> B
U2 -> B
U2 -> C
U1 [pos=\"0,1\"]
  A  [pos=\"0,0\"]
  B  [pos=\"1,0.5\"]
  C  [pos=\"2,0\"]
  U2 [pos=\"2,1\"]
}")

find_fd(dag2, "A", "C", verbose=TRUE)
```
#### OUTPUT:
```
Candidate nodes: B
Total candidate sets: 1
No valid front-door sets found.
No valid front-door                
```
Explanation: {B} would be a valid front-door set, but identification requires conditioning. Because of confounders {U1, U2}, there are no unconditional front door sets.

## Citations

Glynn, A. N., & Kashin, K. (2018). Front-door versus back-door adjustment with unmeasured confounding: Bias formulas for front-door and hybrid adjustments with application to a job training program.
https://doi.org/10.1080/01621459.2017.1398657

Pearl, J. (2009) _Causality: Models, Reasoning and Inference_. 2nd Edition, Cambridge University Press, Cambridge.
https://doi.org/10.1017/CBO9780511803161

Textor, J., van der Zander, B., Gilthorpe, M. S., Liśkiewicz, M., & Ellison, G. T. H. (2016). Robust causal inference using directed acyclic graphs: the R package 'dagitty'. _International Journal of Epidemiology, 45_(6), 1887–1894.
https://doi.org/10.1093/ije/dyw341

Thoemmes, F., & Kim, Y. (2023). Bias and Sensitivity Analyses for Linear Front-Door Models. _Methodology, 19_(3), Article e9205.
https://doi.org/10.5964/meth.9205

