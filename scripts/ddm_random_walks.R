

rm(list = ls())

library(ggplot2)
# set.seed(11)

backstep = -2
forwardstep = 2

for (i in 1:10000) {
        
        set.seed(i)
        print(paste0("Seed at ", i))
        
        n <- 10000
        x <- cumsum(sample(c(backstep, forwardstep), n, TRUE))
        if (any(x>100)) {
                df_x <- data.frame(pos = seq(1,7,length=length(x[1:which(x==100)])), val = x[1:which(x==100)], group="up")
        } else {
                next
        }
        
        # set.seed(11)
        n <- 10000
        y <- cumsum(sample(c(backstep, forwardstep), n, TRUE))
        if (any(y<(-100))) {
                df_y <- data.frame(pos = seq(1,5,length=length(y[1:which(y==(-100))])), val = y[1:which(y==(-100))], group="down")
        } else {
                next
        }
 
        df = rbind(df_x, df_y)
        
        # B&W
        # ggplot(df, aes(x = pos, y = val, color = group)) + geom_line() + theme_classic() + scale_color_manual(values=c("#999999", "#D3D3D3"))
        
        # Colour
        a <- ggplot(df, aes(x = pos, y = val, color = group)) + geom_line() + theme_void() + theme(legend.position="none") + scale_color_manual(values=c("darkturquoise", "darkorange2"))
        break
}

a

ggsave("ddm.pdf")


# Simulate skewed data based on https://web.stanford.edu/class/bios221/labs/simulation/Lab_3_simulation.html

reps <- 50000
nexps <- 5
rate <- 0.1
set.seed(0)
system.time(
        x1 <- replicate(reps, sum(rexp(n=nexps, rate=rate)))
)

ggplot(data.frame(x1), aes(x1)) + 
        theme_void() +
        stat_function(fun=function(x)dgamma(x, shape=nexps, scale=1/rate),
                      color="darkorange2", size=2)

ggsave("ddm_neg.pdf")

reps <- 50000
nexps <- 10
rate <- 0.1
set.seed(0)
system.time(
        x2 <- replicate(reps, sum(rexp(n=nexps, rate=rate)))
)

ggplot(data.frame(x2), aes(x2)) + 
        theme_void() +
        stat_function(fun=function(x)dgamma(x, shape=nexps, scale=1/rate),
                      color="darkturquoise", size=2)

ggsave("ddm_pos.pdf")