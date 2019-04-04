if (!require(plotrix)) { 
  install.packages("plotrix")
  library("plotrix")
}

if (!require(plyr)) {
  install.packages("plyr")
  library("plyr")
}

dir.create("results", showWarnings = FALSE)


get_load <- function(param) {
  single_load = 2 / param;
  n_edges = 22;
  return (single_load * n_edges);
}

plot_with_wiskers <- function(param, mean, model_val, err, ylab, rev = TRUE) {
  param = get_load(param)
  if (rev) {
    suppressWarnings(plotCI(param, mean, err, xlab = "Load of the system (packets/s)",
                            ylab = ylab, pch = 20,  col = "red", scol = "black"))
    points(param, model_val, pch = 4, col = "blue")
  } else {
    plot(param, model_val, pch = 4, xlab = "Load of the system (packets/s)",
         ylab = ylab, col = "blue")
    suppressWarnings(plotCI(param, mean, err, 
                            pch = 20, add = TRUE, col = "red", scol = "black"))
  }
  lines(param, mean, lty = 2, col = "red")
  
  lines(param, model_val, lty = 3, col = "blue")
  
  legend(x="topleft", legend = c("0.99 CI", "Average", "Model"), 
         lty = c(1, 0, 0), pch = c(NA_integer_, 20, 4), col = c("black", "red", "blue"))
}

df = read.csv("simulation_result.csv")

### Get results from queue-net  model ###
source("queue_net.r")
queue_net_result = compute_queue_net_probs(unique(df$param))

###Plot results for each node####
nodes = unique(df$node)
params = unique(df$param)
confidence_level = 0.99
grouped_df = ddply(df, .(param, floor(seed / 10), node), summarise,
                   throughput_bits = mean(throughput_bits),
                   throughput_packets = mean(throughput_packets),
                   loss_rate = mean(loss_rate),
                   collision_rate = mean(collision_rate))


ci_error <- function(sample) {
  n = length(sample)
  return (-qt( (1-confidence_level) / 2, n - 1, 0) * sd(sample) / sqrt(n))
} 
for (node in nodes) {
  ci_with_df = ddply(grouped_df[grouped_df$node == node,], .(param), summarise,
                     throughput_bits_mean = mean(throughput_bits),
                     throughput_bits_err = ci_error(throughput_bits),
                     throughput_packets_mean = mean(throughput_packets),
                     throughput_packets_err = ci_error(throughput_packets),
                     loss_rate_mean = mean(loss_rate),
                     loss_rate_err = ci_error(loss_rate),
                     collision_rate_mean = mean(collision_rate),
                     collision_rate_err = ci_error(collision_rate))
  ci_with_df = ci_with_df[order(-ci_with_df$param), ]
  queue_net_node = queue_net_result[queue_net_result$node == node,]
  pdf(paste("results/throughput_bits_", node, ".pdf", sep = ""))
  plot_with_wiskers(ci_with_df$param, ci_with_df$throughput_bits_mean,
                    queue_net_node$throughput_bits, ci_with_df$throughput_bits_err,
                    paste("Throughput for node", node, "(bits/s)"))
  dev.off()
  pdf(paste("results/throughput_packets_", node, ".pdf", sep = ""))
  plot_with_wiskers(ci_with_df$param, ci_with_df$throughput_packets_mean,
                    queue_net_node$throughput_packets, ci_with_df$throughput_packets_err,
                    paste("Throughput for node", node, "(packets/s)"))
  dev.off()
  pdf(paste("results/loss_rate_", node, ".pdf", sep = ""))
  plot_with_wiskers(ci_with_df$param, ci_with_df$loss_rate_mean,
                    queue_net_node$loss_rate, ci_with_df$loss_rate_err,
                    paste("Loss rate for node", node))
  dev.off()
  pdf(paste("results/collision_rate_", node, ".pdf", sep = ""))
  plot_with_wiskers(ci_with_df$param, ci_with_df$collision_rate_mean,
                    queue_net_node$collision_rate ,ci_with_df$collision_rate_err,
                    paste("Collision rate for node", node))
  dev.off()
}

###Plot aggreagated results####

sumarised_df = ddply (df, .(seed, param), summarise,
                      throughput_bits = sum(throughput_bits),
                      throughput_packets = sum(throughput_packets),
                      loss_rate = mean(loss_rate),
                      collision_rate = mean(collision_rate))
grouped_df = ddply(sumarised_df, .(floor(seed / 10), param), summarise,
                   throughput_bits = mean(throughput_bits),
                   throughput_packets = mean(throughput_packets),
                   loss_rate = mean(loss_rate),
                   collision_rate = mean(collision_rate))

ci_with_df = ddply(grouped_df, .(param), summarise,
                   throughput_bits_mean = mean(throughput_bits),
                   throughput_bits_err = ci_error(throughput_bits),
                   throughput_packets_mean = mean(throughput_packets),
                   throughput_packets_err = ci_error(throughput_packets),
                   loss_rate_mean = mean(loss_rate),
                   loss_rate_err = ci_error(loss_rate),
                   collision_rate_mean = mean(collision_rate),
                   collision_rate_err = ci_error(collision_rate))
queue_net_result = ddply (queue_net_result, .(param), summarise,
                          throughput_bits = sum(throughput_bits),
                          throughput_packets = sum(throughput_packets),
                          loss_rate = mean(loss_rate),
                          collision_rate = mean(collision_rate),
                          clear_chanel_avg = mean(clear_transmition))
pdf("results/throughput_bits_total.pdf")
plot_with_wiskers(ci_with_df$param, ci_with_df$throughput_bits_mean,
                  queue_net_result$throughput_bits,ci_with_df$throughput_bits_err,
                  "Total throughput (bits/s)")
dev.off()
pdf("results/throughput_packets_total.pdf")
plot_with_wiskers(ci_with_df$param, ci_with_df$throughput_packets_mean,
                  queue_net_result$throughput_packets, ci_with_df$throughput_packets_err,
                  "Total Throughput (packets/s)")
dev.off()
pdf("results/loss_rate_average.pdf")
plot_with_wiskers(ci_with_df$param, ci_with_df$loss_rate_mean,
                  queue_net_result$loss_rate, ci_with_df$loss_rate_err,
                  "Average loss rate")
dev.off()
pdf("results/collision_rate_average.pdf")
plot_with_wiskers(ci_with_df$param, ci_with_df$collision_rate_mean,
                  queue_net_result$collision_rate ,ci_with_df$collision_rate_err, rev = FALSE,
                  "Collision rate average")
dev.off()

pdf("results/compare_to_aloha.pdf")
load_param = get_load(queue_net_result$param)
plot(load_param, queue_net_result$throughput_packets, xlab = "Load of the system (packets/s)",
       ylab = "Average probability of receiving", pch = 20,  col = "red")
lines(load_param, queue_net_result$throughput_packets, lty = 2, col = "red")
aloha_tp = compute_aloha_load(load_param)
points(load_param, aloha_tp, pch = 2, col = "green")
lines(load_param, aloha_tp, lty = 3, col = "green")
legend(x="topleft", legend = c("Model", "Aloha"),
       pch = c(20, 2), col = c("red", "green"))
dev.off()