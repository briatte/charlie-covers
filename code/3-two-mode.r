#
# weighted bipartite matrix
#
bipartite_matrix <- function(d, keep) {

  nu = unique(d$num)
  th = themes(unlist(strsplit(d$tags, ";")))
  th[ !th %in% keep ] = simplify[ th[ !th %in% keep ] ]
  th = unique(th[ th != "Varia" ])

  M = matrix(0, nrow = length(th), ncol = length(nu))
  colnames(M) = paste0("n", nu)
  rownames(M) = th

  for(i in 1:nrow(d)) {

    kw = unlist(strsplit(d$tags[ i ], ";"))
    kw = themes(kw)
    kw[ !kw %in% keep ] = simplify[ kw[ !kw %in% keep ] ]
    kw = unique(kw[ kw != "Varia" ])
    nu = paste0("n", d$num[ i ])
    M[, nu ] = M[, nu ] + as.integer(rownames(M) %in% kw)

  }

  return(M[, colSums(M) > 0 ])

}

#
# bipartite network initialization
# @source https://github.com/pedroj/bipartite_plots
#
bipartite_network <- function(M, modes = c("A", "P")) {

  stopifnot(length(modes) == 2)
  x = dim(M)[1]
  y = dim(M)[2]
  net = network.initialize(x + y, bipartite = x, directed = FALSE)
  net = network.bipartite(M, net,
                          names.eval = list(rownames(M), colnames(M)))
  x = rep(modes[1], x)
  y = rep(modes[2], y)
  network::set.vertex.attribute(net, "mode", c(x, y))
  return(net)

}

#
# edge weights
# @source https://github.com/pedroj/bipartite_plots
#
edge_weights <- function(M, x = 30) {

  # Transpose.
  M = t(M)

  # Edge list and weights.
  M = cbind(expand.grid(dimnames(M))[2:1], as.vector(M))

  # Discard null weights.
  M = subset(M, M[, 3] != 0)

  # Scaled weights.
  M.scaled = x*log(M[, 3] + 1) / max(log(M[, 3] + 1))

  # Vector of edge weights.
  return(M.scaled)

}

#
# year-level bipartite networks (last one is for full period)
#
for(yr in 2009:2016) {

  d = read_csv("data/covers.csv")
  keep = c("Politique-Gauche", "Politique-Droite", "Extrême-droite", "Capitalisme", "Showbiz")

  if(yr < 2016) {
    M = bipartite_matrix(filter(d, year == yr), keep)
    yr = paste0("_", yr)
  }
  else {
    M = bipartite_matrix(d, keep)
    yr = ""
  }

  n = bipartite_network(M)

  primary = grepl("^n\\d", network.vertex.names(n))
  primary.label = ifelse(primary, "", network.vertex.names(n))

  g = ggnet(n, #mode = "kamadakawai",
        size = 0, segment.size = edge_weights(M, 2), segment.alpha = 1/5) +
    geom_point(aes(size = ifelse(primary, 1, 4)),
               color = ifelse(primary, "steelblue", "white"),
               alpha = .75) +
    geom_text(aes(label = primary.label),
              alpha = 1/3) +
    geom_text(aes(label = primary.label,
                  color = simplify[ network.vertex.names(n) ])) +
    scale_size_area(max_size = 6) +
    scale_color_manual("", values = colors) +
    guides(size = FALSE) +
    theme(text = element_text(size = 12),
          title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1)),
          legend.key = element_blank(),
          legend.position = "bottom")

  ggsave(paste0("plots/themes_network", yr, ".pdf"), g, width = 12, height = 12)
  assign(paste0("bipartite_plot", yr), g)

  #
  # conversion to tnet format
  #
  TM = data_frame()
  for(j in 1:nrow(M)) {
    r = M[ j, ]
    TM = rbind(TM, data_frame(i = rownames(M)[j],
                              j = colnames(M)[ r > 0 ],
                              w = r[ r > 0 ]))
  }
  th = sort(unique(TM$i))
  cv = sort(unique(TM$j))
  TM$i = as.integer(factor(TM$i, levels = th))
  TM$j = as.integer(factor(TM$j, levels = cv))
  TM = arrange(TM, i, j)
  TM = as.tnet(TM, type = "weighted two-mode tnet")

  #
  # weighted distance between themes
  #
  D = distance_tm(TM)
  rownames(D) = th[ attr(D, "nodes") ]
  colnames(D) = th[ attr(D, "nodes") ]
  DF = data_frame()
  for(j in 1:nrow(D)) {
    DF = rbind(DF, data.frame(i = rownames(D), j = colnames(D)[ j ], d = D[, j ]))
  }

  # average shortest distance
  sd = colnames(D)[ order(colMeans(D, na.rm = TRUE)) ]
  sd = gsub("-", "\n", sd)
  DF$i = factor(gsub("-", "\n", DF$i), levels = sd)
  DF$j = factor(gsub("-", "\n", DF$j), levels = sd)

  # colors for weighted distance, from the wesanderson package
  # @source https://github.com/karthik/wesanderson
  zissou = c("#F21A00", "#E1AF00", "#78B7C5", "#3B9AB2")
  names(zissou) = paste(" <", 1:4, "")

  qplot(data = DF, x = i, y = j,
        fill = cut(round(d, 1), 0:4, right = FALSE,
                   labels = paste(" <", 1:4, "")),
        geom = "tile") +
    scale_fill_manual("Distance pondérée", values = zissou) +
    geom_text(aes(label = round(d, 1)), color = "white") +
    labs(y = NULL, x = NULL) +
    theme_bw(14) +
    theme(panel.grid = element_blank(),
          title = element_text(size = rel(1)),
          axis.text.x = element_text(size = rel(1)),
          axis.text.y = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1)),
          legend.key = element_blank(),
          legend.position = "bottom")

  ggsave(paste0("plots/distance_matrix", yr, ".pdf"), width = 10, height = 10)

}

# #
# # table of all categories
# #
#
# # nombre de "unes"
# t = group_by(TM, i) %>% summarise(Unes = n_distinct(p))
# t$i = th
# names(t)[1] = "Catégorie"
#
# # nombre de mots-clés
# terms = unique(unlist(strsplit(d$tags, ";")))
# terms = themes(terms)
# terms = terms[ terms %in% rownames(D) ]
#
# k = data.frame(table(terms))
# names(k) = c("Catégorie", "Mots-clés")
#
# t = full_join(k, t, by = "Catégorie") %>% arrange(-Unes)
#
# xtable::xtable(t)
