if (!require(plyr)) {
  install.packages("plyr")
  library(plyr)
}
dir.create("results", showWarnings = FALSE)

df = read.csv("queue_levels.csv")
nodes = unique(df$node)
for (node in nodes) {
  pdf(paste("results/queue_level_", node, ".pdf"))
  node_df = df[df$node == node, ]
  plot(node_df$time, node_df$queue_level, type = 'l', xlab = "Time (s)",
       ylab = paste("Number of messages in buffer of node ", node))
  
  dev.off()
}
pdf("results/queue_level_average.pdf")
avg_df = ddply(df, .(time), summarise, queue_level = mean(queue_level))
plot(avg_df$time, avg_df$queue_level, type = 'l', xlab = "Time (s)",
     ylab = "Average number of messages in node buffers ")
dev.off()