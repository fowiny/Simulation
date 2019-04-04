buff_size = 64
vertexes = unique(df$node)
speed = (1024 * 1024)
mu = 1 / (((32 + 4733)/2) / speed)
ajacency_list = list("0" = c(2, 4, 5, 6), "1" = c(5, 9), "2" = c(0, 4, 5, 8), "3" = c(7, 8), 
                    "4" =  c(0, 2, 5, 6, 8), "5" = c(0, 1, 2, 4, 8), "6" = c(0, 4), "7" = c(3), "8" = c(2, 3, 4, 5), "9" = c(1))

prob <- function(size, ro) {
  pi_0 = (1-ro) / (1-ro^(buff_size+1))
  return (pi_0 * pi ^ (size))
}
loss_rate <- function(ro) {
  return (ro ^ buff_size * (1-ro)/(1-ro^(buff_size + 1)))
}
prob_receiving <- function(vertexes, ro) {
  length(vertexes) * (1 - prob(0, ro)) * prob(0, ro) ^ (length(vertexes))
}
prob_transmition <- function(vertexes, ro) {
  (1 - prob(0, ro) ^ (length(vertexes)))
}


compute_aloha_load <- function(load_param) {
  
  return (load_param * exp(-2 * load_param / mu))
}

compute_queue_net_probs <- function(t_max) {
  params_column = c()
  nodes_column = c()
  loss_rate_column = c()
  collision_rate_column = c()
  througput_packets_columns = c()
  througput_bits_columns = c()
  clear_transmition_columns = c()
  
  lambda = 2 / t_max
  for (i in 1:length(t_max)) {
    for (a in names(ajacency_list)) {
      vertex = as.numeric(a)
      params_column = c(params_column, t_max[i])
      nodes_column = c(nodes_column, vertex)
      ro = lambda[i] / mu
      
      loss_rate_column = c(loss_rate_column, loss_rate(ro))
      vertexes = ajacency_list[[a]]
      collision_rate_column = c(collision_rate_column,
                                1 - (prob_receiving(vertexes, ro))/prob_transmition(vertexes, ro))
      througput_bits_columns = c(througput_bits_columns, 8 * prob_receiving(vertexes, ro) * speed)
      througput_packets_columns = c(througput_packets_columns,
                                    prob_receiving(vertexes, ro) * mu)
      clear_transmition_columns = c(clear_transmition_columns, prob_receiving(vertexes, ro))
    }
  }
  
  df_result = data.frame(param = params_column, node = nodes_column, loss_rate = loss_rate_column, 
                         collision_rate = collision_rate_column,
                         throughput_packets = througput_packets_columns,
                         throughput_bits = througput_bits_columns,
                         clear_transmition = clear_transmition_columns)
  return (df_result)
}

