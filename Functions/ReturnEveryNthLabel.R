# function that returns every Nth label
every_n_labeler = function(n = 3) {
  function (x) {
    ind = ((1:length(x)) - 1) %% n == 0
    x[!ind] = ""
    return(x)
  }
}