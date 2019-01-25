import Pkg
let
    pkgs = ["Revise","BenchmarkTools"]
    for pkg in pkgs
    if Base.find_package(pkg) === nothing
        Pkg.add(pkg)
    end
    end
end

using Revise
using BenchmarkTools
using Statistics
# using Profile
using LinearAlgebra
