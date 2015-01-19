########################
# This code illusrates why correlation (pearson r) ranges from
# 0 to abs(1)
# basically showing some ggplot diagrams
# Author: Sebastian Sauer
########################





library(ggplot2)

seq <- seq(from = -5, to = 5)


x1 <- c(0,2,2,0, 0,1,1,0, 0,-2,-2,0, 0,-1,-1,0) # x-axis
y1 <- c(0,0,2,2, 0,0,1,1, 0,0,-2,-2, 0,0,-1,-1) # y-axis
g1 <- c("a1","a1","a1","a1", "a2","a2","a2","a2", # each letter = one dot
        "a3","a3","a3","a3", "a4","a4","a4","a4")
p1 <- c(1,2,3,4, 1,5,6,7) # number of point
l1 <- c("a1", "a2", "a3", "a4",  "a1", "a5", "a6", "a7",
        "a1", "a8", "a9", "a10",  "a1", "a11", "a12", "a13") # name of point


# same for second dataset
x2 <- c(0,2,2,0, 0,1,1,0, 0,-2,-2,0, 0,-1,-1,0)
y2 <- c(0,0,1,1, 0,0,2,2, 0,0,-1,-1, 0,0,-2,-2)
g2 <- c("b1","b1","b1","b1", "b2","b2","b2","b2",
        "b3","b3","b3","b3", "b4","b4","b4","b4")
p2 <- c(1,8,9,10, 1,11,12,13)
l2 <- c("b1", "b2", "b3", "b4",  "b1", "b5", "b6", "b7",
        "b1", "b8", "b9", "b10", "b1", "b11", "b12", "b13")


# put together
my.df <- data.frame(x1, y1, g1, l1, x2, y2, g2, l2)


# plot first data set
p0 <- ggplot(data = my.df, aes(x = x1, y = y1, group = g1, fill = g1))

# plot polygons
p1 <- p0 +
  #geom_polygon(alpha = .7) +
  #scale_fill_manual(values = c("dodgerblue4", "dodgerblue1", "dodgerblue4", "dodgerblue1")) +
  scale_color_manual(values = c("dodgerblue4", "dodgerblue1", "dodgerblue4", "dodgerblue1"),
                     guide = FALSE) +
  geom_polygon(aes(group = g1, alpha = 1, color = g1),
               fill = NA, size = 1.2, show_guide = FALSE)
p1
ggsave(p1, file = "p1.png")


# plot poylgons + points + names of points
p1c <- p0 + geom_vline(yintercept = 0, linetype = "longdash") +
  geom_hline(xintercept = 0, linetype = "longdash") +
  geom_point(aes(color = g1), size = 4, show_guide = FALSE) +
  scale_color_manual(values = c("dodgerblue4", "dodgerblue1", "dodgerblue4", "dodgerblue1"),
                     guide = FALSE) +
  geom_text(aes(x = x1, y = y1, label = l1), hjust=0, vjust=1, size=4) +
  scale_color_manual(values = c("dodgerblue4", "dodgerblue1", "dodgerblue4", "dodgerblue1"),
                     guide = FALSE) +
  geom_polygon(aes(group = g1, alpha = 1, color = g1),
               fill = NA, size = 1.2, show_guide = FALSE)
p1c
ggsave(p1c, file = "p1c.png")

# plot points
p1a <- p0 + geom_vline(yintercept = 0, linetype = "longdash") +
  geom_hline(xintercept = 0, linetype = "longdash") +
  geom_point(aes(color = g1), size = 4, show_guide = FALSE) +
  scale_color_manual(values = c("dodgerblue4", "dodgerblue1", "dodgerblue4", "dodgerblue1"),
                     guide = FALSE) +
  geom_text(aes(x = x1, y = y1, label = l1), hjust=0, vjust=1, size=4)
p1a
ggsave(p1a, file = "p1a.png")

# add normal abline
p1b <- p1a + geom_abline(intercept = 0, slope = 1, linetype = "dashed")
p1b
ggsave(p1b, file = "p1b.png")


# plot 4 points
d1 <- data.frame(x = c(1, 2, -1, -2), y = c(1, 2, -1, -2),
                       type = c("a", "b", "a", "b"),
                label = "a1", "a2", "a3", "a4")

p1d <- ggplot(d1, aes(x = x, y = y, color = type, fill = type)) +
  geom_vline(yintercept = 0, linetype = "longdash") +
  geom_hline(xintercept = 0, linetype = "longdash") +
  geom_point(size = 4, show_guide = FALSE) +
  scale_color_manual(values = c("dodgerblue4", "dodgerblue1")) +
  geom_text(aes(x = x, y = y, label = label), hjust=0, vjust=1, size=4, color = "black")
p1d
ggsave(p1d, file = "p1d.png")

# add normal abline
p1e <- p1d + geom_abline(intercept = 0, slope = 1, linetype = "dashed")
p1e
ggsave(p1e, file = "p1e.png")

########################
# plot *second* data set
########################


p0.2 <- ggplot(data = my.df, aes(x = x2, y = y2, group = g2, fill = g2))

# plot polygons
p2 <- p0.2 +
  #geom_polygon(alpha = .7) +
  #scale_fill_manual(values = c("dodgerblue4", "dodgerblue1", "dodgerblue4", "dodgerblue1")) +
  scale_color_manual(values = c("tomato4", "tomato1", "tomato4", "tomato1"),
                     guide = FALSE) +
  geom_polygon(aes(group = g2, alpha = 1, color = g2),
               fill = NA, size = 1.2, show_guide = FALSE)
p2
ggsave(p2, file = "p2.png")


# plot poylgons + points + names of points
p2c <- p0.2 + geom_vline(yintercept = 0, linetype = "longdash") +
  geom_hline(xintercept = 0, linetype = "longdash") +
  geom_point(aes(color = g2), size = 4, show_guide = FALSE) +
  scale_color_manual(values = c("tomato4", "tomato1", "tomato4", "tomato1"),
                     guide = FALSE) +
  geom_text(aes(x = x2, y = y2, label = l2), hjust=0, vjust=1, size=4)+
  scale_color_manual(values = c("tomato4", "tomato1", "tomato4", "tomato1"),
                     guide = FALSE) +
  geom_polygon(aes(group = g2, alpha = 1, color = g2),
               fill = NA, size = 1.2, show_guide = FALSE)
p2c
ggsave(p2c, file = "p2c.png")

# plot points
p2a <- p0.2 + geom_vline(yintercept = 0, linetype = "longdash") +
  geom_hline(xintercept = 0, linetype = "longdash") +
  geom_point(aes(color = g2), size = 4, show_guide = FALSE) +
  scale_color_manual(values = c("tomato4", "tomato1", "tomato4", "tomato1"),
                     guide = FALSE) +
  geom_text(aes(x = x2, y = y2, label = l2), hjust=0, vjust=1, size=4)
p2a
ggsave(p2a, file = "p2a.png")

# add normal abline
p2b <- p2a + geom_abline(intercept = 0, slope = 1, linetype = "dashed")
p2b
ggsave(p2b, file = "p2b.png")


# plot 4 points
d2 <- data.frame(x = c(2, 1, -2, -1), y = c(1, 2, -1, -2),
                 type = c("a", "b", "a", "b"),
                 label = "b1", "b2", "b3", "b4")

p2d <- ggplot(d2, aes(x = x, y = y, color = type, fill = type)) +
  geom_vline(yintercept = 0, linetype = "longdash") +
  geom_hline(xintercept = 0, linetype = "longdash") +
  geom_point(size = 4, show_guide = FALSE) +
  scale_color_manual(values = c("tomato4", "tomato1")) +
  geom_text(aes(x = x, y = y, label = label), hjust=0, vjust=1, size=4, color = "black")
p2d
ggsave(p2d, file = "p2d.png")

# add normal abline
p2e <- p2d + geom_abline(intercept = 0, slope = 1, linetype = "dashed")
p2e
ggsave(p2e, file = "p2e.png")


# compare linear and quadratic model
p3 <- ggplot(seq, aes(x = seq)) + stat_function(fun = function(x) x^2, colour =
                                                  "dodgerblue3", size = 2) +
  stat_function(fun = function(x) x, colour = "tomato3", size = 2) +
  xlim(c(0,5))
p3




########################
# some statistics
#######################
d1$x

# calculate sd of first dataset
sdx1 <- sqrt(sum((mean(d1$x) - d1$x)^2) / length (d1$x))
varx1 <- sum((mean(d1$x) - d1$x)^2) / length (d1$x)
identical(sdx1, sd(d1$x) * sqrt((length(d1$x)-1)/length(d1$x))) #check
meanx <- mean(d1$x)
sdx1

# calculate sd of second dataset
sdx2 <- sqrt(sum((mean(d2$x) - d2$x)^2) / length (d2$x))
varx2 <- sum((mean(d2$x) - d2$x)^2) / length (d2$x)
identical(sdx2, sd(d2$x) * sqrt((length(d2$x)-1)/length(d2$x))) #check
meanx <- mean(d2$x)
