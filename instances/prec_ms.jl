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
                    # (if first column OR previous column/diagonal left-up has zero) AND it's not the last column AND the cargo to be loaded is not zero
                    if (col == 1 || layout[col-1][row-1] == 0) && col != cols && layout[col+1][row-1] != 0
                        slot_fs = layout[col+1][row-1]
                        if slot_fs != 0
                            push!(loading, ["slot"*string(slot_fs), "slot"*string(slot)])
                        end
                    # if not first column AND previous column/diagonal left-up has not zero
                    elseif col != 1 && layout[col-1][row-1] != 0
                        slot_fp = layout[col-1][row-1]
                        if slot_fp !=0
                            push!(loading, ["slot"*string(slot_fp), "slot"*string(slot)])
                        end
                    end
                end
                
                if row+1 <= rows
                    slot_am = layout[col][row+1]
                    if slot_am != 0
                        push!(unloading, ["slot"*string(slot_am),"slot"*string(slot)])
                    end
                    # (if last column OR next column/diagonal right-down has zero) AND it's not the first column AND the cargo to be unloaded is not zero
                    if (col == cols || layout[col+1][row+1] == 0) && col != 1 && layout[col-1][row+1] != 0
                        slot_ap = layout[col-1][row+1]
                        if slot_ap != 0 
                            push!(unloading, ["slot"*string(slot_ap), "slot"*string(slot)])
                        end
                    # if not last column AND  next column/diagonal right-down has not zero
                    elseif col != cols && layout[col+1][row+1] != 0
                        slot_as = layout[col+1][row+1]
                        if slot_as != 0
                            push!(unloading, ["slot"*string(slot_as), "slot"*string(slot)])
                        end
                    end
                end
            end
        end
    end
    ##loading
    ##unloading
    ##in MIP we also say each slot needs to be unloaded first before it could be loaded
    filename = "ms_"*name*".xlsx"
    XLSX.writetable(filename, 
                    position_sequence = (collect(DataFrames.eachcol(pos_seq)), DataFrames.names(pos_seq)),
                    precedence_loading = (collect(DataFrames.eachcol(loading)), DataFrames.names(loading)), 
                    precedence_unloading = (collect(DataFrames.eachcol(unloading)), DataFrames.names(unloading)))
end

