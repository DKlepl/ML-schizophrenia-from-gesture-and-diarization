source("Scripts/feature_engineering_functions.R")

#Split all actigraphs into 10 seconds long csv files
split_all()

#remove splits that can't be matched
remove_no_match()

#create the coordination pairs
create_coordination_pairs()

#run parameter optimization
run_optimization(folder = "clean_data/Split_data/Coordination")
merge_parameters_all()
select_parameters()
