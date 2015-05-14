#
# colors for simplified themes, from the wesanderson package
# @source https://github.com/karthik/wesanderson
#
colors = c(
  "Politique" = "#5BBCD6",
  "International" = "#F98400",
  "Religion" = "#00A08A",
  "Violence" = "#FF0000",
  "Varia" = "#999999"
)

#
# expanded thematic classification
#
themes <- function(x) {

  y = rep(NA, length(x))

  # france
  y[ x %in% c("bretagne", "corse", "france", "guadeloupe", "marseille",
              "nice", "paris", "provence-alpes-côte d'azur",
              "régions françaises", "toulouse") ] = "France"

  # politique française
  # 'président' in France-only
  y[ x %in% c("clearstream", "elysée", "européennes", "grenelle", "hulot",
              "kouchner", "législatives", "ministre", "municipales",
              "président", "présidentielle", "présidentielle 2007",
              "présidentielle 2012", "présidentielle 2017", "primaires",
              "régionales", "remaniement", "sénat", "sénateur") ] = "Politique"

  y[ x %in% c("extrême droite", "florian philippot", "fn", "le pen",
              "marine le pen") ] = "Extrême-droite"

  y[ x %in% c("bayrou", "bernadette chirac", "besson", "borloo", "boutin",
              "chirac", "copé", "darcos", "dati", "de gaulle",
              "droite", "fillon", "frédéric mitterrand", "guéant", "hortefeux",
              "juppé", "lagarde", "luc chatel", "luc ferry", "mam",
              "nadine morano", "nathalie kosciusko-morizet", "patrick buisson",
              "rama yade", "roselyne bachelot", "sarkozy",
              "ump", "villepin", "woerth", "xavier darcos") ] = "Politique-Droite"

  y[ x %in% c("allègre", "bernard tapie", "cécile duflot", "christiane taubira",
              "cohn-bendit", "dsk", "eva joly", "fabius", "françois hollande",
              "françois mitterrand", "gauche", "jean-luc mélenchon",
              "jean-marc ayrault", "jean-yves le drian", "jérôme cahuzac",
              "les verts", "manuel valls", "martine aubry", "michel sapin",
              "montebourg", "najat belkacem", "pierre moscovici", "ps",
              "ségolène royal") ] = "Politique-Gauche"

  # capitalisme : banques, industrie
  # bonus is for traders (n = 1)
  y[ x %in% c("air france", "airbus", "banques", "bernard arnault", "bnp", "bonus",
              "bourse", "capitalisme", "crédit lyonnais", "crise", "crise économique 2009",
              "crise financière 2008", "entreprises", "florange", "fmi", "krach", "lcl",
              "les firmes", "medef", "parisot", "privatisation", "stock-options", "trader",
              "wall street") ] = "Capitalisme"

  # showbiz et assimilés
  # l'oréal is for bettencourt (n = 2)
  y[ x %in% c("anne sinclair", "bettencourt", "bigard", "carla bruni",
              "delarue", "dieudonné", "frigide barjot", "galliano", "gérard depardieu",
              "georges lautner", "houellebecq", "jean-pierre pernaut", "johnny hallyday",
              "l'oréal", "les riches",  "madonna", "michael jackson", "mireille mathieu",
              "miss france", "polanski", "raymond domenech", "valérie trierweiler",
              "yannick noah", "zemmour") ] = "Showbiz"

  # politique internationale
  y[ x %in% c("afghanistan", "ahmadinejad", "algérie", "allemagne", "bachar al-assad",
              "belgique", "ben ali", "berlusconi", "brésil", "bush", "durban", "egypte",
              "gaza", "grèce", "haïti", "irak", "israël", "israël-palestine", "japon",
              "kadhafi", "les dictatures", "libye", "mali", "maroc", "merkel", "mitt romney",
              "moubarak", "nigeria", "norvège", "obama", "palestine", "poutine", "pussy riot", "qatar",
              "révolution", "rio", "roumanie", "russie", "sotchi", "suisse",
              "syrie", "tunisie", "ukraine", "usa" ) ] = "International"

  # terrorisme
  y[ x %in% c("al-qaida", "11 septembre", "ben laden", "jihad", "mohamed merah",
              "terrorisme") ] = "Terrorisme"

  # religion
  y[ x %in% c("athées", "benoit xvi", "bible", "burka", "charia", "coran",
              "dieu", "évêque", "françois 1er", "intégrisme", "jesus", "laïcité", "lourdes",
              "mahomet", "opus dei", "pape", "prophète", "ramadan", "religion",
              "soeur emmanuelle", "torah", "vatican", "voile") ] = "Religion"

  # armée et police
  y[ x %in% c("14 juillet", "armée", "crs", "police", "taser") ] = "Armée/Police"

  # violence, répréhensible: délinquance, discrimination, crime, fascisme et totalitarisme
  # exclus: prisons, drogues
  y[ x %in% c("antisémitisme", "assassinat", "attentats", "discrimination",
              "colonialisme", "corruption",
              "délation", "guerre", "hitler", "homophobie", "incendie criminel",
              "massacre", "nazisme", "négationnisme", "népotisme", "otage",
              "parricide", "pédophilie", "phallocrate", "prostitution",
              "répression", "sexisme", "suicide", "torture", "totalitarisme",
              "tueur de masse", "violence", "xénophobie") ] = "Violence"

  # médias
  y[ x %in% c("cavanna", "closer", "le monde", "libération", "figaro",
              "france télévision", "hara-kiri", "journal", "journalisme",
              "presse", "radio", "radio france", "siné", "télé", "tf1") ] = "Medias"

  # residuals
  # 'bac' is the diploma, not the police unit
  # 'bizutage' is not necessarily violent
  # 'élections' and 'démocratie' might be national (Politique) or not (International)
  # 'femen' can be national or not
  y[ x %in% c("2022", "absentéisme", "agriculture", "alcoolisme",
              "alimentation", "amende", "anniversaire", "armes", "astérix",
              "aviation", "avion", "bac", "ballerine", "banditisme", "bateau",
              "batman", "bizutage", "bonne année",
              "budget", "ça déraille", "carbone", "caricature", "catastrophes naturelles",
              "charter", "chômage", "cinéma", "climat", "clonage", "clowns",
              "cocaïne", "coluche", "concept cars", "consommation",
              "crash test", "croissance",
              "démocratie", "derrick", "dimanche", "disparus", "drh", "drogue",
              "ebola", "école", "écologie", "économie",
              "écoute", "education nationale", "élections", "elvis",
              "émigration", "enseignement", "environnement",
              "épidémie", "essence", "été", "europe", "examen", "expulsions",
              "facebook", "famille", "farine animale", "femen",
              "foot", "france télécom", "gigolo", "glou glou", "grève", "grippe",
              "humanisme", "immigration", "impôts", "intempéries", "internet",
              "iron man", "jeanne d'arc", "jo", "journée de...",
              "justice", "la poste", "le cimetière", "livres", "logement", "lois",
              "malaise", "manifestations", "mariage",
              "mc donald's", "médicament", "mer", "mode",
              "noël", "nucléaire", "nudisme", "ouverture",
              "patrimoine", "pauvreté", "pétrole", "pharmaceutique",
              "philosophie", "pip", "pirates", "pitbulls",
              "plage", "pma", "pollution", "poséidon adventure",
              "préservatif", "prisons", "prof", "pub",
              "recyclage", "réforme", "régime",
              "rentrée", "retraite",
              "rigueur", "rolex", "rumeur", "santé", "science",
              "sdf", "sécurité", "sécurité sociale",
              "sexualité", "smic", "social",
              "sondages", "sport", "st valentin",
              "surpopulation", "taxe", "téléphone", "téléthon",
              "tempête", "théâtre", "train", "travail", "tva", "twitter",
              "université", "vacances", "vache folle",
              "vie privée", "voeux", "voiture") ] = "Varia"

  # check none missing
  stopifnot(!is.na(y))

  return(y)

}

#
# simplified thematic classification
#
simplify = c(
  "Armée/Police" = "Varia",
  "Capitalisme" = "Varia",
  "Extrême-droite" = "Politique",
  "France" = "Varia",
  "International" = "International",
  "Medias" = "Varia",
  "Politique" = "Politique",
  "Politique-Droite" = "Politique",
  "Politique-Gauche" = "Politique",
  "Religion" = "Religion",
  "Showbiz" = "Varia",
  "Terrorisme" = "Violence",
  "Varia" = "Varia",
  "Violence" = "Violence"
)

# sanity check
stopifnot(unique(simplify) %in% names(colors))
