#
# complete edge list
#
edges = data.frame()
for(i in 1:nrow(d)) {

  y = unique(unlist(strsplit(d$tags[ i ], ";")))
  e = expand.grid(i = y, j = y, stringsAsFactors = FALSE) %>%
    filter(i != j) %>%
    apply(., 1, function(x) paste0(sort(x), collapse = "///")) %>%
    unique
  if(length(e) > 0)
    edges = rbind(edges, data.frame(i = gsub("(.*)///(.*)", "\\1", e),
                                    j = gsub("(.*)///(.*)", "\\2", e),
                                    num = d$num[i],
                                    author = d$author[ i ],
                                    quarter = d$quarter[ i ],
                                    stringsAsFactors = FALSE))

}

#
# quarter-specific networks
#
for(k in sort(c(2008:2015, unique(d$quarter)))) {

  if(nchar(k) == 4)
    e = filter(edges, substr(quarter, 1, 4) == k)[, 1:2 ]
  else
    e = filter(edges, quarter == k)[, 1:2 ]

  cat("Temporal network", k, ":", nrow(e), "edges\n")

  e = data.frame(table(paste0(e$i, "///", e$j)))
  e = data.frame(i = gsub("(.*)///(.*)", "\\1", e$Var1),
                 j = gsub("(.*)///(.*)", "\\2", e$Var1),
                 n = e$Freq,
                 stringsAsFactors = FALSE)

  n = network(e[, 1:2 ], directed = FALSE)
  network::set.edge.attribute(n, "count", e$n)

  n %n% "period" = k
  n %n% "covers" = table(d$quarter)[ names(table(d$quarter)) == k ]

  t = simplify[ themes(network.vertex.names(n)) ]
  n %v% "theme" = t

  g = suppressMessages(ggnet(n, label.nodes = network.vertex.names(n),
        node.group = t, node.color = colors[ names(colors) %in% t ],
        size = 0, label.size = 4, segment.color = "grey75",
        segment.alpha = .5, segment.size = n %e% "count") +
    scale_color_manual("", values = colors, breaks = names(colors)) +
    theme(text = element_text(size = 12),
          title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1)),
          legend.key = element_blank(),
          legend.position = "bottom"))

  ggsave(paste0("plots/network_", k, ".pdf"), g, width = 9, height = 9)

  assign(paste0("plot_", k), g)
  assign(paste0("net_", k), n)
  assign(paste0("edges_", k), e)

}

#
# animated version
#
if(!file.exists("networks.gif"))
  saveGIF({
    for(i in sort(ls(pattern = "plot_\\d+_"))) {
      n = get(gsub("plot_", "net_", i))
      print(get(i) +
              ggtitle(paste(gsub("plot_(\\d{4})_(\\d)", "\\1/\\2", i), ":",
                            n %n% "covers", "'unes',", network.size(n), "mots-clés,",
                            network.edgecount(n), "co-occurrences\n")))
    }
  }, movie.name = "networks.gif", interval = 1, ani.width = 600, ani.height = 600)

#
# relative strength of categorical associations
#
p = data.frame()
for(j in ls(pattern = "^edges_\\d{4}_")) {

  e = get(j)

  e$i = simplify[ themes(e$i) ]
  e$j = simplify[ themes(e$j) ]

  e$u = apply(e[, 1:2 ], 1, function(x) paste0(sort(x), collapse = "-"))
  e = aggregate(n ~ u, sum, data = e)

  e$t = j
  p = rbind(p, e)

}

# limit associations to asymmetric ones with religion
p = group_by(p, t, u) %>%
  summarise(sum = sum(n)) %>%
  filter(grepl("Religion", u) & u != "Religion-Religion")

# append blank lines to show all time periods in plot
for(i in ls(pattern = "^edges_\\d{4}_")[ !ls(pattern = "^edges_\\d{4}_") %in% p$t ])
  p = rbind(p, data.frame(t = i, u = "Religion-Varia", sum = 0))

# recover information on the time period of each row
p = mutate(p, u = gsub("Religion-|-Religion", "", u),
         year = gsub("edges_(\\d{4})_\\d", "\\1", t),
         quarter = gsub("edges_\\d{4}_(\\d)", "\\1", t))

# proportion of ties between 'Religion' and other categories
g = qplot(data = p,
      x = quarter, fill = u, y = sum, stat = "identity", geom = "bar") +
  scale_fill_manual("", values = colors) +
  scale_x_discrete(breaks = 1:4) +
  facet_grid(u ~ year, scales = "free_x", space = "free_x") +
  labs(y = "Nombre de co-occurrences\n",
       x = "\nPériode annuelle : 1. janvier-mars 2. avril-juin 3. juillet-septembre 4. octobre-décembre") +
  guides(fill = FALSE) +
  theme_bw(14) +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = rel(1)),
        axis.text = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.position = "bottom")

ggsave("plots/themes_religion.pdf", g, width = 12, height = 10)

#
# author-specific networks (plots only)
#
for(k in names(table(d$author)[ table(d$author) > 50 ])) {

  e = filter(edges, author == k)[, 1:2 ]
  cat("Author network", k, ":", nrow(e), "edges\n")

  e = data.frame(table(paste0(e$i, "///", e$j)))
  e = data.frame(i = gsub("(.*)///(.*)", "\\1", e$Var1),
                 j = gsub("(.*)///(.*)", "\\2", e$Var1),
                 n = e$Freq,
                 stringsAsFactors = FALSE)

  n = network(e[, 1:2 ], directed = FALSE)
  network::set.edge.attribute(n, "count", e$n)

  n %n% "period" = k
  n %n% "covers" = table(d$quarter)[ names(table(d$quarter)) == k ]

  t = simplify[ themes(network.vertex.names(n)) ]
  n %v% "theme" = t

  g = suppressMessages(ggnet(n, label.nodes = network.vertex.names(n),
                             node.group = t, node.color = colors[ names(colors) %in% t ],
                             size = 0, label.size = 4, segment.color = "grey75",
                             segment.alpha = .5, segment.size = n %e% "count") +
                         scale_color_manual("", values = colors, breaks = names(colors)) +
                         theme(text = element_text(size = 12),
                               title = element_text(size = rel(1)),
                               legend.text = element_text(size = rel(1)),
                               legend.key = element_blank(),
                               legend.position = "bottom") +
    ggtitle(paste(k, ":", n %n% "covers", "'unes',", network.size(n), "mots-clés,",
                  network.edgecount(n), "co-occurrences\n")))

  ggsave(paste0("plots/network_", k, ".pdf"), g, width = 12, height = 12)

}

# done
