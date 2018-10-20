df <- data.frame(x = rnorm(1000, 2, 2))

# overlay histogram and normal density
ggplot(df, aes(x)) +
        geom_histogram(aes(y = stat(density))) +
        stat_function(
                fun = dnorm, 
                args = list(mean = mean(df$x), sd = sd(df$x)), 
                lwd = 2, 
                col = 'red'
        )

