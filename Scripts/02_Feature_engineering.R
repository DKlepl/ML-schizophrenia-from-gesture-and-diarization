source("Scripts/feature_engineering_functions.R")

#Split all actigraphs into 10 seconds long csv files
split_all()

#create pairs of 10 seconds long splits to analyze coordination

#remove splits that can't be matched
remove_no_match()

#create the coordination pairs
create_coordination_pairs()
