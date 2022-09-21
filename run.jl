include("dc_newins.jl")

rules = ["m"]
instances = ["1","2", "3", "4", "5", "6", "7", "8", "9", "10"]
notes="single"
for rule in rules
    for instance in instances
        dualcycling(rule= rule, instance = instance, notes=notes)
    end
end