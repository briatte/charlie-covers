#
# get all relevant pages from Strips Journal
#
if(!file.exists("data/covers.csv")) {

  d = data.frame()
  for(i in seq(340, 0, -10)) {

    cat("Parsing page", sprintf("%2.0f", i / 10))
    file = paste0("data/pages/page-", i, ".html")

    if(!file.exists(file))
      h = try(download.file(paste0("http://stripsjournal.canalblog.com/tag/Les%20Unes%20de%20Charlie%20Hebdo/p",
                                   i, "-0.html"), file, mode = "wb", quiet = TRUE), silent = TRUE)

    if(!file.info(file)$size) {

      cat(": failed\n")
      file.remove(file)

    } else {

      h = htmlParse(file)

      addr = xpathSApply(h, "//div[@class='blogbody']//meta[@itemprop='url']/@content")
      date = xpathSApply(h, "//div[@class='blogbody']//meta[@itemprop='dateCreated']/@content")
      date = as.Date(substr(date, 1, 12))
      titl = xpathSApply(h, "//div[@class='blogbody']//h3", xmlValue)

      dat = gsub("(.*) - (.*) - (.*)", "\\3", titl)
      dat = gsub("(.*)- (.*)", "\\2", dat)
      dat = gsub("octore", "octobre", dat)
      dat = gsub("sept\\.", "sep.", dat)
      dat = parse_date_time(gsub("\\.", "", dat), "%d %m %y", locale = "fr_FR")
      dat = as.Date(dat)

      # fix one unparsable date
      dat[ addr == "http://stripsjournal.canalblog.com/archives/2012/11/14/26045046.html" ] =
        as.Date("2012-11-14")

      file = gsub("(.*) - (.*) - (.*)", "\\2 - \\1", titl)
      file = gsub("Charlie Hebdo Nª", "", file)
      file = paste(dat, file)

      kwd = xpathSApply(h, "//div[@class='blogbody']//h3/following-sibling::div[@class='itemfooter'][1]")
      kwd = lapply(kwd, xpathSApply, "a[@rel='tag']", xmlValue)

      # fix a few keywords
      kwd = lapply(kwd, function(x) {
        y = tolower(x[ !x %in% c("Charlie Hebdo", "Les Unes de Charlie Hebdo") ])
        y[ y == "aubry" ] = "martine aubry"
        y[ y == "crise économique 2009..." ] = "crise économique 2009"
        y[ y == "tapie" ] = "bernard tapie"
        y[ y == "emmigration" ] = "émigration"
        y[ y == "univercité" ] = "université"
        return(gsub("œ", "oe", y))
      })
      kwd = sapply(kwd, paste0, collapse = ";")

      aut = xpathSApply(h, "//div[@class='blogbody']//h3/following-sibling::div[@class='itemfooter'][2]")
      aut = lapply(aut, xpathSApply, "a[contains(@href, 'archives')]", xmlValue)
      aut = sapply(aut, head, 1)

      # img = xpathSApply(h, "//div[@class='blogbody']//h3/following-sibling::p//img/@src")
      img = xpathSApply(h, "//div[@class='blogbody']//h3/following-sibling::div[@class='itemfooter'][1]/following-sibling::p[2]")
      img = sapply(img, xpathSApply, "a/img/@src")
      img = sapply(img, function(x) ifelse(is.null(x), NA, x))

      # fix six parser errors
      img[ addr == "http://stripsjournal.canalblog.com/archives/2012/04/30/24141374.html" ] =
        "http://p0.storage.canalblog.com/06/27/177230/75268181.jpg"
      img[ addr == "http://stripsjournal.canalblog.com/archives/2013/11/20/28478698.html" ] =
        "http://p2.storage.canalblog.com/22/38/177230/91674307.jpg"
      img[ addr == "http://stripsjournal.canalblog.com/archives/2013/11/12/28419822.html" ] =
        "http://p1.storage.canalblog.com/15/42/177230/91450058_o.jpg"
      img[ addr == "http://stripsjournal.canalblog.com/archives/2013/11/05/28369503.html" ] =
        "http://p2.storage.canalblog.com/28/47/177230/91251269_o.jpg"
      img[ addr == "http://stripsjournal.canalblog.com/archives/2013/10/29/28316947.html" ] =
        "http://p4.storage.canalblog.com/44/10/177230/91032930_o.jpg"
      img[ addr == "http://stripsjournal.canalblog.com/archives/2015/01/12/31306656.html" ] =
        "http://p6.storage.canalblog.com/68/20/177230/101510919_o.png"

      file = paste0(file, gsub("(.*)\\.(gif|jpg|png)", ".\\2", img))

      d = rbind(d, data.frame(post_page = i,
                              post_url = addr, post_date = date, post_title = titl,
                              date = dat, tags = kwd, author = aut, image = img,
                              file, stringsAsFactors = FALSE))

      cat(":", sprintf("%3.0f", nrow(d)), "total covers\n")

    }

  }

  # fix three missing authors
  d$author[ d$author == "Index Dessinateurs" ] = NA
  d$author[ d$post_url == "http://stripsjournal.canalblog.com/archives/2009/10/28/30971837.html" ] =
    "Luz"
  d$author[ d$post_url == "http://stripsjournal.canalblog.com/archives/2014/05/21/30964112.html" ] =
    "Cabu"
  d$author[ d$post_url == "http://stripsjournal.canalblog.com/archives/2014/05/14/30964082.html" ] =
    "Cabu"

  write_csv(d, "data/covers.csv")

}

d = read_csv("data/covers.csv")

#
# download the actual covers (mostly .jpg)
#
for(i in which(!is.na(d$image))) {

  file = paste0("covers/", d$file[ i ])

  if(!file.exists(file))
    try(download.file(d$image[ i ], file, quiet = TRUE), silent = TRUE)

}

# dates
table(year(d$date), exclude = NULL)

# years and quarters
d$year = year(d$date)
d$quarter = paste0(d$year, "_", quarter(d$date))

# group all pre-2008Q4 covers
d$quarter[ d$year < 2008 | d$quarter == "2008_2" ] = "2008_3"

# group all 2015 covers in same quarter
d$quarter[ d$quarter == "2015_2" ] = "2015_1"

# numérotation
d$num = str_extract(d$post_title, "°\\d+")
d$num[ is.na(d$num) ] = str_extract(d$post_title[ is.na(d$num) ], "Charlie Hebdo \\d+")
d$num = gsub("\\D", "", d$num)
stopifnot(n_distinct(d$num) == nrow(d))

# authors
table(d$author, exclude = NULL)

# keywords
terms = unlist(strsplit(d$tags, ";"))

stopifnot(!is.na(themes(terms)))
stopifnot(!is.na(simplify[ themes(terms) ]))

cat(nrow(d), "covers,", n_distinct(terms), "unique keywords\n")
table(terms)[ table(terms) > quantile(table(terms), .99) ]

# themes
write_csv(d, "data/covers.csv")

#
# expanded thematic classification
#
t = data.frame(tag = unique(terms), theme = themes(unique(terms))) %>%
  arrange(tag)

head(t)

t$nums = NA
tags = strsplit(d$tags, ";")
for(i in t$tag) {

  k = lapply(tags, function(x) i %in% x)
  k = as.integer(d$num[ which(sapply(k, isTRUE)) ])

  if(length(k))
    t$nums[ t$tag == i ] = paste0(k [ order(k) ], collapse = ";")

}

write_csv(t, "data/tags.csv")

#
# occurrence of keywords per year/quarter
#
g = data.frame()
for(i in 1:nrow(d)) {

  y = unlist(strsplit(d$tags[ i ], ";"))
  g = rbind(g, data.frame(year = d$year[ i ],
                          q = d$quarter[ i ],
                          theme = simplify[ themes(y) ],
                          row.names = NULL,
                          stringsAsFactors = FALSE))

}
g = group_by(g, year, q, theme) %>%
  summarise(n = n()) %>%
  group_by(year, q) %>%
  mutate(sum = sum(n)) %>%
  mutate(prop = n / sum)

g = qplot(data = filter(g, year >= 2008),
      x = factor(substr(q, 6, 7)), y = prop, group = theme, fill = theme,
      stat = "identity", position = "stack", geom = "bar") +
  facet_grid(theme ~ year, scales = "free_x", space = "free_x") +
  scale_fill_manual("Thème", values = colors, breaks = names(colors)) +
  scale_y_continuous(labels = percent) +
  guides(fill = FALSE) +
  theme_bw(14) +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = rel(1)),
        axis.text = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.position = "bottom") +
  labs(y = "Proportion des mots-clés utilisés sur la période et contenus dans la catégorie\n",
       x = "\nPériode annuelle : 1. janvier-mars 2. avril-juin 3. juillet-septembre 4. octobre-décembre")

ggsave("plots/themes.pdf", g, width = 12, height = 12)

# done
