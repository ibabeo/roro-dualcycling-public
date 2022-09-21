using XLSX, DataFrames

layouts = XLSX.readxlsx("instance generation - layout.xlsx") #read all the sheets
sheetnames = XLSX.sheetnames(layouts)
for name in sheetnames[1:end]
    println("constructing precedence matrix for "*name)
    layout = DataFrame(layouts[name][:])
    sz = size(layout)
    rows = sz[1]
    cols = sz[2]
    loading = DataFrame(pred = String[], suc = String[])
    unloading = DataFrame(pred = String[], suc = String[])
    pos_seq = DataFrame(position_onboard = String[], dis_seq = Int[], load_seq = Int[])
    ttl = sum([sum(col) for col = eachcol(layout.>0)])

    u = ttl
    l = 1
    for row in 1:rows
        for col in 1:cols
            slot = layout[col][row]
            if slot != 0
                push!(pos_seq, ["slot"*string(slot), u, l])
                u = u-1
                l = l+1
                if row-1 > 0
                    slot_fm = layout[col][row-1]
                    if slot_fm != 0 
                        push!(loading, ["slot"*string(slot_fm),"slot"*string(slot)])
                    end
                end
                
                if row+1 <= rows
                    slot_am = layout[col][row+1]
                    if slot_am != 0
                        push!(unloading, ["slot"*string(slot_am),"slot"*string(slot)])
                    end
                end
            end
        end
    end
    ##loading
    ##unloading
    ##in MIP we also say each slot needs to be unloaded first before it could be loaded
    filename = "m_"*name*".xlsx"
    XLSX.writetable(filename, 
                    position_sequence = (collect(DataFrames.eachcol(pos_seq)), DataFrames.names(pos_seq)),
                    precedence_loading = (collect(DataFrames.eachcol(loading)), DataFrames.names(loading)), 
                    precedence_unloading = (collect(DataFrames.eachcol(unloading)), DataFrames.names(unloading)))
end

