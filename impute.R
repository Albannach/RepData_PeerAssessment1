imputeValuesForNA = function() {
    n = 55;
    for (i in 1:100) {##1:nrow(rawData)) {
        j = rawData[i, "interval"]+1;
        if (floor(j/n)-round(j/n) == -1) {
            print(paste0(i, ' ',j, ' ', rawData[i,"steps"]));
        }
        else {
            print(i);
        }
    }
}