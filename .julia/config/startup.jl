import Pkg
let
    pkgs = ["Revise","BenchmarkTools", "Infiltrator"]
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

atreplinit() do repl
    try
        @eval using Revise
        @async Revise.wait_steal_repl_backend()
    catch
    end
end
