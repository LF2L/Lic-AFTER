Questions <- function(){


  Questions =tibble(

    "Gestion.1"="Concevoir et mettre en œuvre des projets à caractère innovant",
    "Gestion.2"="Répondre aux exigences techniques, financières et juridiques des projets",

    "Moderacion.1"="Élaborer des stratégies émergentes pour intégrer les nouvelles connaissances et piloter l'ensemble du projet en temps réel",
    "Moderacion.2"="Aider à dessiner une vision collective, encourager les gens à partager des idées et à participer",
    "Moderacion.3"="Appliquer des méthodes et pratiques pédagogiques  pour assurer la progression des équipes et des participants",

    "Mediacion.1"= "Concevoir et mettre en œuvre des stratégies de résolution des conflits",

    "Networking.1" = "Établir des liens et des relations avec les organisations et les entreprises locales, les organismes de financement, des associations etc.",
    "Networking.2" = "Favoriser des rencontres pertinentes et fructueuse entre les personnes.",

    "Participacion.1" = "Repenser les modèles de projets traditionnels sur la base des approches participatives courantes",
    "Participacion.2" = "Mettre en place des dispositifs collaboratifs pour inviter les gens à participer aux activités du projet, les familiariser avec le projet, les accueillir et les orienter",

    "Comunicacion.1" = "Avoir de l'empathie et être ouvert aux autres points de vue en écoutant l'expérience des autres",
    "Comunicacion.2" = "Faire preuve de tolérance dans des situations de plus en plus complexes et contrariantes",

    "Autogestion.1" = "Croire en sa propre capacité à choisir une approche efficace pour mener à bien une tâche ou une activité dans des environnements de plus en plus complexes",
    "Autogestion.2" = "Tener tolerancia en situaciones cada vez más ambiguas y frustrantes",

    "Intercultural" = "Mettre en place des dispositifs permettant d'assurer l'inclusion de tous, quels que soient leur culture, leur âge, leur situation économique ou leur lieu de résidence",

    "Evaluacion" = "Concevoir et mettre en œuvre des mécanismes de saisie et d'analyse des données afin d'éclairer une stratégie donnée et de mettre en évidence ses résultats" ,

    "Met.Investigacion.1" = "Travailler avec d'autres disciplines/acteurs de l'écosystème social",
    "Met.Investigacion.2" = "Appliquer des méthodes de travail/recherche différentes de celles utilisées dans ma discipline",

    "Met.Diseño" ="Associer les idées et les connaissances de manière inédite, utiliser les techniques de design pour l'innovation",

    "TIC"= "Développer des prototypes technologiques appliqués via la programmation, la simulation, la modélisation, etc.",

    "Pens.Emprendedor.1"= "Gérer une entreprise ou une activité entrepreneuriale",
    "Pens.Emprendedor.2"= "Recombiner les atouts et les opportunités dans le cadre d'un processus d'incubation de projet",

    "Pens.Sistemico"= "Concevoir et mettre en œuvre des stratégies pour gérer la complexité, la pensée anticipative, et la culture du changement"

  )

  Questions = Questions %>% mutate(Parametro = c("")) %>% melt( id = "Parametro")
  Questions = tibble(Questions)

  names(Questions) = c("Parametro", "Variable",  "Descripcion")

  Questions <- Questions %>% mutate(
    Parametro = replace(Parametro, str_detect(Variable, "Gestion"), c("Gestion de projet")),
    Parametro = replace(Parametro, str_detect(Variable, "Moderacion"), c("Modération")),
    Parametro = replace(Parametro, str_detect(Variable, "Mediacion"), c("Médiation")),
    Parametro = replace(Parametro, str_detect(Variable, "Networking"), c("Networking")),
    Parametro = replace(Parametro, str_detect(Variable, "Participacion"), c("Participation")),
    Parametro = replace(Parametro, str_detect(Variable, "Comunicacion"), c("Communication")),
    Parametro = replace(Parametro, str_detect(Variable, "Autogestion"), c("Auto-organisation")),
    Parametro = replace(Parametro, str_detect(Variable, "Intercultural"), c("Interculturel")),
    Parametro = replace(Parametro, str_detect(Variable, "Evaluacion"), c("Évaluation")),
    Parametro = replace(Parametro, str_detect(Variable, "Met.Investigacion"), c("Méthodes de recherche et travail interdisciplinaire")),
    Parametro = replace(Parametro, str_detect(Variable, "Met.Diseño"), c("Méthodes de conception et pensée créative")),
    Parametro = replace(Parametro, str_detect(Variable, "TIC"), c("Techniques du numérique et des télécommunications")),
    Parametro = replace(Parametro, str_detect(Variable, "Pens.Emprendedor"), c("L'esprit entrepreneurial")),
    Parametro = replace(Parametro, str_detect(Variable, "Pens.Sistemico"), c("Pensée systémique")),
  )

  # Creation of the Roles
  Questions$Role.Climatelabs = c("")
  Questions = Questions %>% select(Role.Climatelabs, Parametro, Variable, Descripcion)


  Questions <- Questions %>% mutate(
    Role.Climatelabs = replace(Role.Climatelabs, str_detect(Parametro, paste(c("Gestion de projet", "Auto-organisation", "Évaluation" ) ,collapse = '|')), c("Manager")),
    Role.Climatelabs = replace(Role.Climatelabs, str_detect(Parametro, paste(c("L'esprit entrepreneurial", "Networking", "Communication" ) ,collapse = '|')), c("Visionario")),
    Role.Climatelabs = replace(Role.Climatelabs, str_detect(Parametro, paste(c("Modération", "Médiation", "Participation", "Interculturel") ,collapse = '|')), c("Facilitador")),
    Role.Climatelabs = replace(Role.Climatelabs, str_detect(Parametro, paste(c("Méthodes de recherche et travail interdisciplinaire", "Méthodes de conception et pensée créative",
                                                                               "Pensée systémique", "Techniques du numérique et des télécommunications" ) ,collapse = '|')),
                               c("Maker"))
    #  Role.Climatelabs = replace(Role.Climatelabs, str_detect(Parametro, paste(c( ) ,collapse = '|')), c("Especialista")),
  )

  Questions <- Questions %>% arrange(Role.Climatelabs)

  return(Questions)

  #write(Questions, "Questions.csv") # Use this command to exprt the table
}

