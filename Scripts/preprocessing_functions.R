#read audio files in the folder
load_folder = function(path) {
  path = path
  all_files = list.files(path=path, full.names = T) 
  return(all_files)
}

get_info = function (f, type=c("a", "g")) {
  
  #subject ID
  filename = f
  id = as.character(parse_number(filename))
  
  #diagnosis
  if (startsWith(id,'1')==T) {
    diagnosis=1
  } else {
    diagnosis=0
  }
  info = data.frame(ID=as.numeric(id), diagnosis=diagnosis)
  
  #if the file is actigraph data then it includes also hand dominance
  if (type == "g") {
    if (grepl("Right", filename) == TRUE) {
      right = 1
    } else {right = 0}
    info$right = right
  }
  
  return(info)
}

read_audio = function(f) {
  audio = tuneR::readWave(f)
  test = audio@left[(0):(10*22050)]
  return(test)
}

segment_audio = function(sound) {
  
  #use soundgen's algorithm to find bursts of acoustic energy in the signal
  #it's intended for syllable segmentation but it works well
  seg = soundgen::segment(sound, samplingRate = 22050,windowLength = 40, plot =F)
  bursts = seg$bursts
  
  #how to identify which burst are claps?
  #calculate SD with a sliding window - the lowest = 2 bursts small interburst interval
  sd= evobiR::SlidingWindow("sd",bursts$interburstInt , 2, 1)
  sd = c(sd,rep(NA,2))
  bursts$sd = sd
  return(bursts)
}

#plot the found peaks to make allow sanity check
plot_seg_audio = function(audio, seg_audio) {
  windowLength_points = ceiling(40 * 22050 / 1000)
  
  sound_downsampled = seewave::env(
    audio,
    f = 22050,
    envt = "hil",
    msmooth = c(windowLength_points, 80),
    fftw = FALSE,
    plot = FALSE
  )
  timestep = 1000 / 22050 * (length(audio) / length(sound_downsampled))
  envelope = data.frame(time = ((1:length(
    sound_downsampled
  ) - 1) * timestep), value = sound_downsampled)
  
  p = plot(
    x = envelope$time,
    y = envelope$value,
    type = "l",
    col = "green",
    xlab = "Time, ms",
    ylab = "Amplitude"
  )
  points(seg_audio,
         col = "red",
         cex = 3,
         pch = 8)
}

#let the algoeithm choose the first clap and perform sanity check
sanity_check_audio = function(seg) {
  choice = which.min(seg[,4])
  
  #construct checking question for user
  ask = paste0("I picked the ", choice, "th red cross as the first clap. What would be your choice, sir? ")
  check="no response"
  
  check = as.numeric(readline(ask))
  
  if (choice != check) {
    choice = check
  }
  return(choice)
}

#get the time of all the claps and construct an output
get_output_audio = function(inf, segmented, first_clap) {
  three_claps = data.frame(audioclap1 = segmented[first_clap, 1], 
                           audioclap2 = segmented[first_clap+1, 1], 
                           audioclap3 = segmented[first_clap+2, 1])
  #get the time in seconds
  three_claps = three_claps/1000
  output = cbind(inf, three_claps)
  return(output)
}

get_claps_audio = function(f) {
  info = get_info(f, type = "a")
  audio = read_audio(f)
  extracted = segment_audio(sound = audio)
  plot_seg_audio(audio, extracted)
  final_choice = sanity_check_audio(extracted)
  one_file = get_output_audio(inf=info, 
                        segmented = extracted, 
                        first_clap = final_choice)
  return(one_file)
}

read_actigraph = function(path) {
  data = read.csv(path)
  data = na.omit(data)
  return(data)
}

find_peaks = function(data) {
  x_left = as.data.frame(pracma::findpeaks(data$PsychologistJerkLeft, minpeakheight = 2, npeaks = 100, minpeakdistance = 20))
  x_right = as.data.frame(pracma::findpeaks(data$PsychologistJerkRight, minpeakheight = 2, npeaks = 100, minpeakdistance = 20))
  
  peak_permutation = expand.grid(x_left$V2,x_right$V2)
  peak_permutation$diff = abs(peak_permutation$Var1 - peak_permutation$Var2)
  peak_permutation = peak_permutation[peak_permutation$diff<100,]
  peak_permutation = peak_permutation[order(peak_permutation$Var1),]
  unique_peaks = unique(peak_permutation$Var1)
  
  return(unique_peaks)
}

sanity_check_actigraph = function(peaks) {
  if (length(peaks)>3) {
    sd= evobiR::SlidingWindow("sd", peaks , 3, 1)
    continue = sd[1]<100
  } else if (length(peaks)==3) {
    continue = sd(unique_peaks)<100
  } else {continue=F}
  return(continue)
}

get_output_actigraph = function(inf , peaks, check) {
  if (check==T) {
    three_claps = data.frame(gestclap1 = peaks[1], 
                               gestclap2 = peaks[2], 
                               gestclap3 = peaks[3])
    three_claps = three_claps/100
  } else {three_claps = data.frame(gestclap1 = NA, 
                                   gestclap2 = NA, 
                                   gestclap3 = NA)
  }
  
  output = cbind(inf, three_claps)
  
  return(output)
}

get_claps_actigraph = function(f) {
  info = get_info(f, type="g")
  actigraph = read_actigraph(path=f)
  extracted = find_peaks(data=actigraph)
  continue = sanity_check_actigraph(peaks = extracted)
  one_file = get_output_actigraph(info, extracted, continue)
  
  return(one_file)
}

get_claps_folder = function (folder_path, type=c("audio", "actigraph")) {
  files = load_folder(folder_path)
  #place to store all claps
  claps = data.frame()
  progress = 1
  for (file in files) {
    if (type == "audio") {
      one_output = try(get_claps_audio(file))
    } else if (type == "actigraph") {
      one_output = try(get_claps_actigraph(file))
    } else {print("Invalid 'type' input.")
      }
    
    #rbind the output to the big dataframe
    claps = try(rbind(claps, one_output))
    
    print(progress)
    progress = progress+1
  }
  
  #save the output
  if (type == "audio") {
    write.csv(claps, "clean_data/audio_claps.csv")
  } else {
    write.csv(claps, "clean_data/gesture_claps.csv")
  }
}
