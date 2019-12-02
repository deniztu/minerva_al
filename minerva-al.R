################################
### Minerva Model Equations ####
################################

# probe           Feature vector for probe event (A).
# memory          Memory matrix with columns representin features and rows traces in memory (Mij).
# cue_features    A vector giving the indeces of features that are associated with cues.
# model           Model argument specifies the model either "Minerva AL" or "Minerva2".

probe_memory <- function (probe, memory, cue_features, model = "Minerva AL"){
  if(is.null(memory)) { # Empty memory
    echo <- runif(length(probe), -0.001, 0.001) # First trial is noise (p. 65, Jamieson, Crump, & Hannah, 2012)
    normalized_echo <- echo / max(abs(echo)) # Eq. 4, Jamieson, Crump, & Hannah (2012)
    return(normalized_echo)
  } else {
    # Compare only features associated with cues (p. 64, Jamieson, Crump, & Hannah, 2012)
    probe <- probe[cue_features]
    relevant_memory <- matrix(memory[, cue_features], ncol = length(cue_features))
    
    # calculate similarity according to model
    if (model == "Minerva AL"){
      similarity <- colSums(probe * t(relevant_memory)) / (sqrt(sum(probe^2) * rowSums(relevant_memory^2))) # simplified Eq. 7, Jamieson, Crump, & Hannah (2012)
    }
    else if(model == "Minerva2"){
      similarity <- colSums(probe * t(relevant_memory)) / colSums((probe != 0 | t(relevant_memory) != 0)) # Eq. 1, Hintzman (1984) or Eq. 1, Jamieson, Crump, & Hannah (2012)
    }
    # calculate activation
    activation <- similarity^3                          # Eq. 2, Jamieson, Crump, & Hannah (2012)
    
    # Calculate echo according to model
    echo <- colSums(activation * memory)                # Eq. 3, Jamieson, Crump, & Hannah (2012)
    if (model == "Minerva AL"){
      echo <- echo + runif(length(echo), -0.001, 0.001)   # Add noise (p. 64, Jamieson, Crump, & Hannah, 2012)
     }
    normalized_echo <- echo / max(abs(echo))                 # Eq. 4, Jamieson, Crump, & Hannah (2012)
    return(normalized_echo)
  }
}


# outcome             Feature vector for outcome event (X).
# normalized_echo     Normalized echo (C'j) produced by probe event (A).

expect_event <- function (outcome, normalized_echo) {
  #     expectancy <- sum(outcome * normalized_echo) / sum(outcome != 0) # Eq. 5, Jamieson, Crump, & Hannah (2010)
  expectancy <- sum(outcome * normalized_echo) / sum(outcome != 0 & normalized_echo != 0)   # Eq. 4, Jamieson, Crump, & Hannah (2012)
  return(expectancy)
}


# normalized_echo     Normalized echo (C'j) produced by probe event (A).
# event               Feature vector for the encountered event (e.g., E = A + X).
# p_encode            Probability with which a feature is encoded in memory (L).
# memory              Memory matrix with columns representing features and rows traces in memory (Mij).
# model               Model argument specifies the model either "Minerva AL" or "Minerva2".


learn <- function (normalized_echo, event, p_encode, memory, model = ("Minerva AL")) {
  if (p_encode < 1) { # Speeds up simulation
    encoding_error <- rbinom(length(event), 1, p_encode)
  } else {
    encoding_error <- rep(1, length(event))
  }
  if (model == "Minerva AL"){
    # Discrepency encoding
    memory <- rbind(memory, (event - normalized_echo) * encoding_error)    # Eq. 6, Jamieson, Crump, & Hannah, (2012)
    }
  else if(model == "Minerva2"){
    memory <- rbind(memory, event * encoding_error)
  }
  return(memory)
}