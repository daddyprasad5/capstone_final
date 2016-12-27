#performance test

source("ngramBootstrap.R")

op <- microbenchmark(
        getDistro = getDistro("this is a test", "a_test", triGram, biGram, uniGrams, POSDictionary, triplets),
        times=100L)

print(op)
qplot(y=time, data=op, colour=expr) + scale_y_log10()
qplot(y=time, data=op, colour=expr) + scale_y_log10()
