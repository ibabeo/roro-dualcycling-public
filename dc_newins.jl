
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

const MOI = MathOptInterface

function dualcycling(; verbose = true, rule = "m", instance="1", warmstart="greedy", timelimit=20, sort_method = "kids", rins=-1, tugs = 4, notes="", cuts = 2)
    path_to_input = joinpath(@__DIR__, "instances\\input\\"*rule*"_layout"*instance*".xlsx")
    k = tugs #total number of trucks
    #load and unload list
    #loadls = uppercase(instance[1])*"-"*uppercase(instance[2:3])*"-F-Sgl"
    #unloadls = uppercase(instance[1])*"-"*uppercase(instance[2:3])*"-F-Sgl"

    #RINS=-1 choose automatically. 0 shuts off rins
    filename = string(rule, "_", instance,"_", Dates.format(Dates.now(), "_yyyy_mm_dd_HHMM"), notes)
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
    cd("output")
    model = Model(with_optimizer(Gurobi.Optimizer, TimeLimit=timelimit, RINS=rins,LogFile=filename, Cuts=cuts))
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
#####################################Warm Start##################################################
    #Single cycling
    if warmstart == "single"
        pos_seq = DataFrame(XLSX.readtable(path_to_input, "position_sequence")...)

        ws_u = select(pos_seq, [:position_onboard, :dis_seq])  #warm start  _ unloading
        ws_l = select(pos_seq, [:position_onboard, :load_seq])

        pq_u = PriorityQueue()
        for (position_onboard, dis_seq) in eachrow(ws_u)
            pq_u[position_onboard] = dis_seq
        end

        pq_l = PriorityQueue()
        for (position_onboard, load_seq) in eachrow(ws_l)
            pq_l[position_onboard] = load_seq
        end

        
        t = 2

        while length(pq_u) > 0  
            if length(pq_u) < k/2 #if the cargo to be unloaded is less than the number of trucks available, then unload all the rest of cargo  
                for i in 1:length(pq_u)
                    pred_violation = 0 
                    being_discharged = dequeue_pair!(pq_u)[1]
                    set_start_value(x[being_discharged,t], 1) #then it gets discharged
                    length(pq_u) == 0 ? break : nothing
                    next_in_queue = peek(pq_u)[1]
                    for (suc, pred) in eachrow(P_u)
                        if (next_in_queue, being_discharged) == (suc, pred)
                            pred_violation = 1
                            break
                        end
                    end
                    
                    pred_violation == 1 ?  break : nothing #ternary operator like if-elseif-else
                end
                t = t + 1
            else 
                
                for i in 1:k/2
                    pred_violation = 0 
                    being_discharged = dequeue_pair!(pq_u)[1]
                    set_start_value(x[being_discharged,t], 1) #then it gets discharged
                    length(pq_u) == 0 ? break : nothing
                    next_in_queue = peek(pq_u)[1]
                    for (suc, pred) in eachrow(P_u)
                        if (next_in_queue, being_discharged) == (suc, pred)
                            pred_violation = 1
                            break
                        end
                    end
                    
                    pred_violation == 1 ?  break : nothing #ternary operator like if-elseif-else
                end
                t = t + 1
            end
        end

        while length(pq_l) > 0
            if length(pq_l) < k/2
                for i in 1:length(pq_l)
                    pred_violation = 0 
                    being_loaded = dequeue_pair!(pq_l)[1]
                    set_start_value(y[being_loaded,t], 1) #then it gets loaded
                    length(pq_l) == 0 ? break : nothing
                    next_in_queue = peek(pq_l)[1]
                    for (suc, pred) in eachrow(P_l)
                        if (next_in_queue, being_loaded) == (suc, pred)
                            pred_violation = 1
                            break
                        end
                    end
                    
                    pred_violation == 1 ?  break : nothing #ternary operator like if-elseif-else
                end
                t = t + 1
            else 
                
                for i in 1:k/2
                    pred_violation = 0 
                    being_loaded = dequeue_pair!(pq_l)[1]
                    set_start_value(y[being_loaded,t], 1) #then it gets loaded
                    length(pq_l) == 0 ? break : nothing
                    next_in_queue = peek(pq_l)[1]
                    for (suc, pred) in eachrow(P_l)
                        if (next_in_queue, being_loaded) == (suc, pred)
                            pred_violation = 1
                            break
                        end
                    end
                    pred_violation == 1 ?  break : nothing #ternary operator like if-elseif-else
                end
                t = t + 1
            end
        end
    elseif warmstart == "greedy"
        #assign each operation an id
        jobdict = Dict{String, Int}()
        for (i, j_u_p) in enumerate(u_p)
            push!(jobdict, j_u_p*"U" => i)
        end
        ii = length(jobdict)
        for (i, j_l_p) in enumerate(l_p)
            push!(jobdict, j_l_p*"L" => i+ii)
        end

        #build DAG - complete precedence (UU, LL, UL)
        DAG = DiGraph(length(jobdict))
        @showprogress 1 "Building graph" for (child, parent) in eachrow(P_u)
            add_edge!(DAG, jobdict[parent*"U"], jobdict[child*"U"])
        end
        for (child, parent) in eachrow(P_l)
            add_edge!(DAG, jobdict[parent*"L"], jobdict[child*"L"])
        end
        for job in u_p
            add_edge!(DAG, jobdict[job*"U"], jobdict[job*"L"])
        end
        jobdict_rev = Dict(value => key for (key, value) in jobdict)




        if sort_method == "kids"
            #sort by number of descendants
            job_pq_kids = Dict{Int64, Int64}()
            for i in vertices(DAG)
                d = dijkstra_shortest_paths(DAG, i).dists
                o = filter(x -> (x != typemax(x) && x != 0), d)
                s = length(o)
                push!(job_pq_kids, i => s)
            end
            job_pq_kids = sort(collect(job_pq_kids), by=x->x[2], rev = true)
            
            job_pq = Int64[]
            for (key, value) in job_pq_kids
                push!(job_pq, key)
            end
        elseif sort_method == "top"
            ##topological sort of DAG by depth first search
            job_pq = topological_sort_by_dfs(DAG) 
            #this gives a better warm start with 69 instead of 75
        end
        
        #assign values to warm start
        # potential bugs can happen when they cannot find a trailer that is not blocked by any.
        t = 1
        last_l = 0
        last_u = 0
        
        while length(job_pq) > 0
            un = Int64[] #to keep track of the number unloaded each timestamp
            l = Int64[] #-- loaded --
            t = t + 1
            #take a job and check if it is blocked by any
            for (pq_index, pq_value) in enumerate(job_pq)
                pred_violation = 0 #reset value
                operation_full = jobdict_rev[pq_value]
                operation = operation_full[1:end-1]
                string(operation_full[end]) == "U" ? mode = "U" : mode = "L"
                for job_b in job_pq
                    if job_b in vcat(un,l)
                        nothing
                    elseif job_b == pq_value
                        nothing
                    else
                        has_path(DAG, job_b, pq_value) ? (@goto label1) : nothing
                    end
                end
                @goto label3 #if exhausion, meaning not blocked by any, then go to label 3 and load/unload
                @label label3
                #if unloading 
                if mode == "U" 
                    if length(un) > 0
                        for pq_done in un
                            has_path(DAG, pq_done, pq_value) ? (@goto label1) : nothing
                        end
                    end
                    #if exhausion, check tug availability
                    length(vcat(un,l)) == k || length(un)>= k/2 || length(un) >= last_l + (k-last_l-last_u) ? (@goto label2) : nothing
                    #if not break, then discharge
                    set_start_value(x[operation,t], 1)
                    push!(un, pq_value)
                else #if loading
                    if length(l) > 0
                        for pq_done in l
                            jobdict_rev[pq_done][1:end-1] == jobdict_rev[pq_value][1:end-1] ? continue : nothing
                            has_path(DAG, pq_done, pq_value) ? (@goto label1) : nothing
                        end
                    end
                    length(vcat(un,l)) == k || length(l)>= k/2 || length(l) >= last_u + (k-last_l-last_u) ? (@goto label2) : nothing
                    set_start_value(y[operation,t], 1)
                    push!(l, pq_value)
                end
                @label label1
            end
            @label label2
            w = 0
            last_l = length(l)
            last_un = length(un)
            filter!(e->!(e in un), job_pq)
            filter!(e->!(e in l), job_pq)
        end
    end  
    #u_ub_updated = t
###################################### Objective and Constraints ################################################
    #set_upper_bound(u, u_ub_updated)

    cons_seq = ""
    
    @constraint(model, [i in u_p, t in T], t*x[i,t] <= u) #3
    cons_seq = cons_seq*"3,"
    @constraint(model, [j in l_p, t in T], t*y[j,t] <= u) #4
    cons_seq = cons_seq*"4,"

    #only load/unload once
    @constraint(model, [i in u_p], sum(x[i,t] for t in T) == 1) #6
    cons_seq = cons_seq*"6,"
    @constraint(model, [j in l_p], sum(y[j,t] for t in T) == 1) #7
    cons_seq = cons_seq*"7,"
    
    
    #precedence
    @constraint(model, [p in 1:numpu], sum((t-1)*x[suc_u[p],t] for t in T) >= sum(t*x[pred_u[p],t] for t in T)) #unloading #8
    cons_seq = cons_seq*"8,"
    @constraint(model, [p in 1:numpl], sum((t-1)*y[suc_l[p],t] for t in T) >= sum(t*y[pred_l[p],t] for t in T)) #load #9
    cons_seq = cons_seq*"9,"
    @constraint(model, [p in 1:numu], sum(t*y[u_p[p], t] for t in T) >= sum(t*x[u_p[p],t] for t in T)) #10
    cons_seq = cons_seq*"10,"
    
    #truck
    @constraint(model, [t in T[2:end]], sum(x[i,t] for i in u_p) + wsq[t] == sum(y[j,t-1] for j in l_p) + wqs[t-1]) #12
    cons_seq = cons_seq*"12,"
    @constraint(model, [t in T], sum(x[i,t] for i in u_p) + wsq[t] + sum(y[j,t] for j in l_p) + wqs[t] + wqq[t] == k) #13
    cons_seq = cons_seq*"13,"
    @constraint(model, [t in T], sum(x[i,t] for i in u_p) <= 2) #14
    cons_seq = cons_seq*"14,"
    @constraint(model, [t in T], sum(y[j,t] for j in l_p) <= 2) #15
    cons_seq = cons_seq*"15,"
    @constraint(model, sum(y[j,1] for j in l_p) + wqs[1] + wqq[1] == k) #16    
    cons_seq = cons_seq*"16,"

    JuMP.optimize!(model)
    
    obj = Int(JuMP.objective_value(model))
    
    if verbose
        open(filename*".txt", "a") do io
            write(io, cons_seq)
            write(io, "\nRESULTS:\n")
            for t in 1:obj
                write(io, "TIME=$(T[t])	Wsq[$(T[t])] = $(JuMP.value(wsq[t]))	Wqs[$(T[t])] = $(JuMP.value(wqs[t]))	Wqq[$(T[t])] = $(JuMP.value(wqq[t]))")
                for i in u_p
                    if ((JuMP.value(x[i,t]))>0.0)
                        write(io, " unload[$(i),$(T[t])] = $(JuMP.value(x[i, t]))\t")
                    end 
                
                    if ((JuMP.value(y[i,t]))>0.0)
                        write(io, "	load[$(i),$(T[t])] = $(JuMP.value(y[i, t]))\t")
                    end                 
                end
                write(io, "\n")
            end
        end
        open(filename*"_tbl.txt", "a") do io
            for i in u_p
                write(io, i)
                for t in 1:obj
                    (JuMP.value(x[i,t]))>0.0 ? write(io, "  u   $(t)") : nothing
                    (JuMP.value(y[i,t]))>0.0 ? write(io, "  l   $(t)") : nothing
                end
                write(io, "\n")
            end
        end
    end
    cd("..")

end

