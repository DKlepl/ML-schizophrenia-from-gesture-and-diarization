source("Scripts/feature_engineering_functions.R")

#split all files into single utterances and save them in separate files 
#sorted by utterance's interlocutor
split_single_all()

#pair all utterances with the utterance of the other interlocutor
coordination_split_all()
