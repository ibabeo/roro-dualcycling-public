
#############################################################################
# JuMP
# An algebraic modeling language for Julia
# See http://github.com/JuliaOpt/JuMP.jl
#############################################################################
# Gurobi Solver
# Non-commercial academic licence
#############################################################################

using DataFrames, XLSX, Gurobi, JuMP, Test
using DataStructures, MathOptFormat
using ProgressMeter, Dates, LightGraphs
import MathOptInterface
using MathOptFormat

const MOI = MathOptInterface

function dualcycling(; verbose = true, rule = "m", instance="1", tugs = 4, cuts = 2)
    #path_to_input = joinpath(@__DIR__, "instances\\input\\"*rule*"_layout"*instance*".xlsx")
    path_to_input = joinpath(@__DIR__, "input\\input_"*instance*".xlsx")
    k = tugs     
######################################## Parameters #############################################
    #unloadlist = DataFrame(XLSX.readtable(path_to_input, unloadls)...)
    unloadlist = DataFrame(XLSX.readtable(path_to_input, "position_sequence")...)
    #u_id = unloadlist[:cargo_id]
    u_p = unloadlist[:position_onboard]
	numu= size(unloadlist, 1)
    
    loadlist = DataFrame(XLSX.readtable(path_to_input, "position_sequence")...)
    #l_id = loadlist[:cargo_id]
    l_p = loadlist[:position_onboard]
	numl= size(loadlist, 1)    
 
    
    #timestamps
    T = [1:numl+numu+1;]
    
    #precedence matrix for loading
    P_l = DataFrame(XLSX.readtable(path_to_input, "precedence_loading")...) 
    pred_l = P_l[:pred] 
    suc_l = P_l[:suc] 
    numpl = size(P_l,1)
   
    #precedence matrix for unloading
    P_u = DataFrame(XLSX.readtable(path_to_input, "precedence_unloading")...)
    pred_u = P_u[:pred] 
    suc_u = P_u[:suc]
    numpu = size(P_u,1)
    
######################################### Model #################################

    model = Model(with_optimizer(Gurobi.Optimizer, Cuts=cuts))
#################################### Decision variables ##########################################    
    # binary variable, x[i,t] == 1 if a cargo is unloaded from position i at timestamp t
    @variable(model, x[i in u_p, t in T], Bin) 
    for i in u_p
        JuMP.fix(x[i, 1], 0; force=true)
    end
    # binary variable, y[j,t] == 1 if a cargo is loaded onto position j at timestamp t
	@variable(model, y[j in l_p, t in T], Bin)
    # makespan and bound
    u_ub = 2*(numu+numl)+1
    u_lb = (numu+numl)/k+1
    @variable(model, u_lb <= u <= u_ub, Int)
    # truck variables 
    @variable(model, 0 <= wsq[t in T] <= 4, Int)
    JuMP.fix(wsq[1], 0; force=true)
    @variable(model, 0 <= wqs[t in T] <= 4, Int)
    @variable(model, 0 <= wqq[t in T] <= 4, Int)
    # minimize the makespan
    @objective(model, Min, u) 

###################################### Objective and Constraints ################################################
    @constraint(model, [i in u_p, t in T], t*x[i,t] <= u) #3
    @constraint(model, [j in l_p, t in T], t*y[j,t] <= u) #4

    #only load/unload once
    @constraint(model, [i in u_p], sum(x[i,t] for t in T) == 1) #6
    @constraint(model, [j in l_p], sum(y[j,t] for t in T) == 1) #7
 
    #precedence
    @constraint(model, [p in 1:numpu], sum((t-1)*x[suc_u[p],t] for t in T) >= sum(t*x[pred_u[p],t] for t in T)) #unloading #8
    @constraint(model, [p in 1:numpl], sum((t-1)*y[suc_l[p],t] for t in T) >= sum(t*y[pred_l[p],t] for t in T)) #load #9
    @constraint(model, [p in 1:numu], sum(t*y[u_p[p], t] for t in T) >= sum(t*x[u_p[p],t] for t in T)) #10
    
    #truck
    @constraint(model, [t in T[2:end]], sum(x[i,t] for i in u_p) + wsq[t] == sum(y[j,t-1] for j in l_p) + wqs[t-1]) #12
    @constraint(model, [t in T], sum(x[i,t] for i in u_p) + wsq[t] + sum(y[j,t] for j in l_p) + wqs[t] + wqq[t] == k) #13
    @constraint(model, [t in T], sum(x[i,t] for i in u_p) <= 2) #14
    @constraint(model, [t in T], sum(y[j,t] for j in l_p) <= 2) #15
    @constraint(model, sum(y[j,1] for j in l_p) + wqs[1] + wqq[1] == k) #16    

    mps_model = MathOptFormat.MPS.Model()
    MOI.copy_to(mps_model, JuMP.backend(model))
    filename = string(instance, ".mps")
    MOI.write_to_file(mps_model, filename)

end

instances = ["smd", "stt", "swd", "sud"]


for instance in instances
    dualcycling(instance=instance)
end