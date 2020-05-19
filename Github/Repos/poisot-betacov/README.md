# poisot-betacov

Method 1 - k-NN (files named `Tanimoto`)

Method 2 - LF (files named `LinearFilter`)

The script requires *Julia 1.3* to run - there is a `Project` and a `Manifest`
file already, so starting from a fresh *Julia* install all that is required
to get the correct packages is:

~~~ julia
import Pkg
Pkg.activate(".")
Pkg.instantiate()
include(joinpath("scripts", "knn_lf.jl"))
~~~
