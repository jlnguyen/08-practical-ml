

a <- data.frame(a = 1, b = 4, c = 6, d = 5)
b <- a
b[c(2,4)] <- NA

b <- sapply(b, function(i) {
    if (is.na(b[i])) { b[i] <- NULL }
})


for (i in 1 : 3) {
    var <- b[[i]]
    if (any(is.na(var))) {
        b[i] <- NULL
    } else {
        b[i] <- var
    }
}