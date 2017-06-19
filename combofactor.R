combofactor <-
  function(pattern_vector,
           replacement_vector,
           data) {
    for (i in 1:length(pattern_vector))
    {
      levels <- levels(data)
      levels[which(pattern_vector[i] == levels)] <-
        replacement_vector[i]
      levels(data) <- levels
    }
    data
  }