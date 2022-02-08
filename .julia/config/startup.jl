import Pkg
let
    pkgs = ["Revise","BenchmarkTools", "Infiltrator", "AbbreviatedStackTraces"]
    for pkg in pkgs
    if Base.find_package(pkg) === nothing
        Pkg.add(pkg)
    end
    end
end

# using Revise
using BenchmarkTools
using Statistics
using Profile
Profile.init(n=10000000, delay=0.001) # profile for 10x longer than default
using LinearAlgebra
using Infiltrator
using Test

using Revise

ENV["JULIA_STACKTRACE_MINIMAL"] = true
using AbbreviatedStackTraces
