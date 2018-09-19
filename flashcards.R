### The flashcards notepad (apps and decks)

   ### The Flashcard functions ###

# variable lengths
# 3-sided
# 4-sided
# reverse 3
# reverse 4

### flashcards for many-sided cards (best for lists of variable lengths):
flashcards.many = function(yourlist){
  continue = readline(prompt = "Hit any button to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(7)
      for (j in 1:length(yourlist[[inum]])){
        print(yourlist[[inum]][j])
        Sys.sleep(10)
      }
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, names(yourlist)[inum]))
      for (j in 1:length(yourlist[[inum]])){
        show = readline(prompt = "Hit 'a' to show next side: ")
        if (show == "a"){
          print(yourlist[[inum]][j])
        } else{print("you suck")}
      }
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit any button to continue, enter to quit: ") 
    }
  }
  print("Peace out.")
}

### flashcards for 3 sided-cards:
flashcards3 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][1])
      Sys.sleep(5)
      print(yourlist[[inum]][2])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, names(yourlist)[inum]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][1])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(yourlist[[inum]][2])
      } else{print("Uh ... WTF.")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
    }
  }
  print("Peace out.")
}
### flashcards for 4-sided cards:
flashcards4 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][1])
      Sys.sleep(7)
      print(yourlist[[inum]][2])
      Sys.sleep(5)
      print(yourlist[[inum]][3])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, names(yourlist)[inum]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][1])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(yourlist[[inum]][2])
      } else{print("Uh ... WTF.")}
      show3 = readline(prompt = "Hit 'y' to show the other other other side: ")
      if (show3 == 'y'){
        print(yourlist[[inum]][3])
      } else{print("F U too then.")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("Peace.")
}

### reverse flashcards for 3-sided cards:
flashback3 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][2])
      Sys.sleep(7)
      print(yourlist[[inum]][1])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, yourlist[[inum]][2]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][1])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(names(yourlist)[inum])
      } else{print("Uh ... WTF.")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("F U.")
}
### reverse flashcards for 4-sided cards:
flashback4 = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(10)
      print(yourlist[[inum]][3])
      Sys.sleep(7)
      print(yourlist[[inum]][2])
      Sys.sleep(5)
      print(yourlist[[inum]][1])
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, yourlist[[inum]][3]))
      show1 = readline(prompt = "Hit 'y' to show other side: ")
      if (show1 == "y"){
        print(yourlist[[inum]][2])
      } else{print("Alrighty, then.")}
      show2 = readline(prompt = "Hit 'y' to show the other other side: ")
      if (show2 == 'y'){
        print(yourlist[[inum]][1])
      } else{print("Uh ... okay.")}
      show3 = readline(prompt = "Hit 'y' to show the other other other side (the front): ")
      if (show3 == 'y'){
        print(names(yourlist)[inum])
      } else{print("Then I'll see you in hell!")}
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("Have a nice day.")
}
############################################################################################

############################################################################################

                                   ### Flash cards ###

                                   ###################

## Ideas for future flashcard decks:
# The Enlightenment - the history, the philosophy, the people, the ideas, the values, the polemic (what were they reacting to or arguing against; i.e. not just what were they for, but what traditional ideas were they against)
# Socialism - socialism vs communism; empirical facts about socialist phenomena; misconceptions
# Capitalism - Chomsky facts about; history; ideas; values; misconceptions
# Political right vs left - what are the left wing ideas, phenomena, practices, etc.? what are the right wing ones? Have they ever changed? When did right and left emerge in political philosophy?

## SEPARATE FROM THE REST OF THE DECKS - USMLE step 1 (15 categories + high yield):
# biochem
# cardiology
# dermatology
# endocrinology
# gastroenterology
# hematology and oncology
# immunology
# microbiology - SEE ALSO 'PATHOGENS' DECK IN ALPHABETICAL LIST BELOW
# musculoskeletal
# nehprology / genitourinary
# neurology
# pathology
# pharmacology
# psychiatry
# public health sciences
# pulmonology / respiratory
# reproduction
# high yield


### ANYTHING FROM A ZOTERO ARTICLE WILL HAVE 'Zotero' AT BEGINNING OF CARD NAME (NAME SHOULD BE PRINCIPAL AUTHOR NAME AND YEAR, MAYBE ALSO TITLE KEY WORD):
## In alphabetical order by list (deck) name:
# ancient.names.3 (a list of ancient people(s) who were/are known by phonetically distinct names)
# BofM.EME.4 (Book of Mormon EME vocab passages)
# BofM.translation.facts.variable
# bythenumbers.3 (topic/question, number, source)
# climate.two.per.lecture (climate change Great Course lectures)
# definitions.variable
# geography.lists.variable
# medical.terminology.3 (term, definition, abbreviation)
###### name.ten.variable (name ten of any kind of thing - like name ten geniuses, name ten top-grossing companies, name ten overpaid CEOs, name ten life-saving vaccines, name ten of the biggest fossil fuel companies, etc.)
# pathogens.variable
# people.important.variable
# PharaohList.3 (a list of 30 famous or interesting pharaohs)
# physics.and.technology.variable (mostly of stuff I learn about in Science and Nature)
# political.facts.variable
# psalms.in.BofM.4
# quotes.variable (author and topic on front, quote on back; could have more than one side for long quotes or words to pay special attention to)
# random.facts.4 (a list of truly random facts--with topic name, two facts about it, and a source)
###### random.lists.variable (a list of random lists; the lists can be however long)
# science.corroboration.variable
# scriptures.variable (lists of scripture refences, with pertinent excerpts, on some topic)
# TreeLifeList.3 (68 of the dichotomous trees in the tree of life)

###### THESE TWO DECKS ARE VERY SIMILAR: NAME.TEN AND RANDOM.LISTS


##########################################################################################

# Public Health Problems
public.health = list(one = "filler",
                     neurodegenerative.disorders = c("Alzheimer's disease (AD) is the most common neurodegenerative disorder.","Parkinson's is the second most common one, currently affecting 4 million worldwide; WTF: that number is expected to double in the next few decades??? Wow.","Source: Chen-Plotkin et al., 'Finding useful biomarkers for Parkinson’s disease', in Science Translational Medicine, August 2018")
                     )

                         #########################
                         ### School Flashcards ###
                         #########################


SAS.notes = list(one = "filler",
                 what.is.TRUNCOVER.used.for = c(),
                 )



## Epidemiology I
# by chapter? - No. BY POWER POINT.
# All questions I don't have answers to yet will in their c() have this: ???

Epide.exam.1 = list(one = "filler",
                    unintentional.injuries = c("Unintentional injuries are the leading cause of death in people ages 1-44 years. Holy freak."),
                    proportion.vs.rate = c("Rate: a ratio representing change over time: 1) units 2) range 3) e.g. velocity","Odds: probability of an event: 1) no units 2) range 3) e.g.","Incidence density and incidence rate are the same thing."),
                    prevalence.vs.incidence = c("Prevalent cases: existing cases (old and new) at a specified point in time or over a period of time. 'Prevalence proportion' is a term that is used in the literature.","Incident cases: new cases identified over a specified period of time."),
                    prevalence.proportion_two.kinds = c("Point prevalence: at a moment in time.","Period prevalence: e.g. throughout the entire year. You gotta use the average total population during that same period for the denominator"),
                    prevalence.proportion_time.of.onset = c("You DON'T need to know time of onset in order to calculate prevalence proportion. Whereas with incidence, you DO need to know time of onset."),
                    measure.of.risk_prevalence.or.incidence_CI.4 = c("Prevalence is not a measure of risk. Incidence is a measure of risk. Use the football watching study as an example. 27 of us total. Only 22 at risk of becoming football watchers, because 5 already do watch. Cumulative incidence (risk) is the proportion of initially susceptible individuals in a population who become new cases of a disease in a specified time period.","Cumulative Incidence (risk) measures the probability that healthy people will develop disease during a specified time.","Cumulative incidence is calculated with specification of numerator that excludes existing cases, only including new cases.","To calculate cumulative incidence you need to specify the population at risk. The denominator thus excludes people with disease already and people who are not susceptible."),
                    incidence.rate_counting.person.time = c("Incidence rate (aka incidence density) is also a measure of risk, like cumulative incidence.","The denominator for incidence rate is person-time. e.g. a rate of .2 cases per person-year is equivalent to 20 cases per 100 person-years.","When do we stop counting person time? 1) When the person experiences the outcome of interest 2) When they die 3) they become lost to follow-up; e.g. by moving 4) study ends."),
                    point.prevalence_period.prevalence = c("Point prevalence = prevalence of the disease at a certain point in time. The term 'prevalence' usually refers to point prevalence.","Period prevalence = how many people have had the disease at any point during a certain time period. Some people may have developed the disease during that period, and others may have had the disease before and died or been cured during that period. The important point is that everyon represented by the numerator had the disease at some time during the period specified."),
                    why.cant.you.use.prevalence.as.measure.of.risk = c("Prevalence does not take into account the direction of the disease. Example in book: Compare high town and low town chest x rays. High town is rich; low town is poor. If the prevalence of TB is high in high town, does that mean they have high risk? Not necessarily. It could be that they survive with TB for longer than poor people in low town. This would mean you'd end up having more prevalence in high town than low town, but the risk could be the same.","PREVALENCE DOESN'T TAKE INTO ACCOUNT THE DIRECTION OF THE DISEASE--THAT'S WHY IT DOESN'T TELL YOU RISK."),
                    what.then.do.we.use.prevalence.for = c("If we cannot use it to measure risk, why bother with it at all? Prevalence is an important and useful measure of the burden of disease in a community. This is necessary in order to determine, for example, how many clinics and health professionals are needed"),
                    why.is.incidence.hard.to.measure = c("Because it's hard to know when the disease started."),
                    prevalence.depends.on.2 = c("It depends on incidence and ...","... duration of disease. Actually, prevalence is incidence times duration: P = I x D."),
                    when.is.mortality.rate.good.surrogate.for.incidence.rate.of.disease = c("When survival is low: When people who get the disease die from it (nearly) invariably and quickly."),
                    what.is.cumulative.incidence = c(),
                    if.we.want.to.look.at.the.cause.of.disease.we.need = c("If we want to determine etiology, we must explore the relationship between an exposure and ...","... the risk of disaease. To do this, we need data on ...","... incidence."),
                    observed.changes.in.mortality_artifactual = c("Error in the NUMERATOR due to ...","Errors in diagnosis","errors in age","changes in coding rules","changes in classification.","Or, and error in the DENOMINATOR due to ...","Errors in counting the population","errors in classifying population characteristics like age, race, or sex, etc.","differences in percentages of populations at risk"),
                    difference.between.case.fatality_vs_mortality.rate = c("Case fatality denominator is just people who have the disease. Mortality rate denominator is people with the disease and people at risk of developing the disaease."),
                    does.incidence.proportion.have.time.in.it = c("No. When you follow EVERYONE for the study period. if we can follow everyone for the entire study period, then we can calculate incidence proportion. But if not, then we can only calulate incidence rate. If we follow everyone for the entire study period, then we can calculate the cumulative incidence, or in other words, the incidence proprotion. They are the same thing. If you can't do that, then you're gonna have to include time in the denominator and calculate incidence density, which is the same thing as incidence rate. Thus, in order to calculate incidence rate, you DON'T need to follow all subjects the same length of time."),
                    secular.trends.are.associated.with = c("The time component (as opposed to personal and place components)."))

Epi.ppts = list(one = "filler",
                lecture_1 = c("Intro and History"),
                lecture_2 = c("Morbidity"),
                week.2_l3 = c("Mortality"),
                week.2_l4 = c("Descriptive Epidemiology"))
Epi.Intro_History = list(one = "filler",
                         public.health.definition_2 = c("Public health is organized community efforts aimed at the prevention of disease and the promotion of health.","Public health is the science and art of protecting and improving the health of communities through education, promotion of healthy lifestyles, and research for disease and injury prevention."),
                         definition.and.6components.of.epidemiology = c("Definition: Epidemiology is the study of the distribution and determinants of health-related states and events in populations, and the application of this study to control health problems.","Components (6): ...","'study' - refers to MEASUREMENT of disease frequency, involving QUANTIFICATION of the occurrence of disease. Examples: surveillance, observation, hypothesis testing, descriptive, analytical and experimental research.","'distribution' - refers to FREQUENCY of disease by person, place, and time. We look for deviations from a uniform distribution (i.e. does the disease cluster?). Example: diabetes prevalence in the US concentrates in the south and midwest.","'determinants' - any FACTOR that brings about a change in a health condition. Types (6): PHYSICAL (safe water, clean air), BIOLOGY AND GENETICS, SOCIAL (culture, beliefs, support), redundantly: cultural, INDIVIDUAL [behavioral] (diet, exercise, smoking), and ECONOMIC (socioeconomic status, access to care).","'health-related states and events' (10) - infectious diseases, chronic diseases, deaths, injuries, disabilities, mental disorders, suicide, substance abuse, behaviors, use of health services.","'populations' - as opposed to individuals; epidemiology is population medicine.","'to control' - the aim is to PROMOTE, PROTECT, and RESTORE health."),
                         easy.key.terms.in.epidemiology_4_a5 = c("Exposure: refers to ...","1) determinants","2) agents","3) interventions","4) conditions","5) policies, or anything that might affect health.","Disease: refers to all health-related states or events","Morbidity: illness","Mortality: death"),
                         really.what.is.epidemiology_2.and.5 = c("Epidemiology is the study of health and disease in populations.","It's the basic science of public health.","WHO gets disease?","What CAUSES disease?","How does disease SPREAD?","What PREVENTS disease?","What works in CONTROLLING disease?"),
                         scope.of.epidemiology_4 = c("Describe (the health status of populations)","Explain (the etiology of disease)","Predict (the occurrence of disease)","Control (the distribution of disease)."),
                         why.consider.the.history.of.epi_2 = c("Scientific understanding of disease and causal factors is a product of historical successes and failures.","Changes in science, technology and disease patterns influence evolution of pubilic health and epidemiology, and so to address changing needs, the science of epidemiology continues to evolve, developing new methodologies to deal with complex problems."),
                         Hippocrates_5_i.e.Discernment.TermsUsed.Humors.Book.9Associations = c("Recognized that disease results from the physical environment rather than supernatural causes.","Used the terms 'epidemic' and 'endemic'.","Believed disease was the result of imbalance in 4 bodily humors: black bile, yellow bile, phlegm, and blood.","Wrote a work called 'On Airs, Waters and Places' in 400 BC.","He associated disease with geography, water conditions, climate, eating habits and housing, and the mode of living including glutony, indolence, or fondness for exercise and labor."),
                         James.Lind_16_i.e.Year.Approach.6Interventions.Recovery.4ApproachCriteria.NavyYear = c("Experiments with scurvy in 1753.","Divided sailors into groups of 2. Same diet in both.","Interventions (6): ...","1) cider","2) vinegar","3) sea water","4) lemon and 2 oranges","5) elixir","6) garlic, mustard seed and horseradish","The men with citrus in their diet were fit for duty in 2-6 days.","Lind's approach included four key criteria: ...","1) he made CLINICAL observations","2) he had an EXPERIMENTAL DESIGN","3) he observed POPULATION changes and their effect on disease","4) he considered SOURCES of disease.","Since 1895, the British navy has required limes/lime juice be included in diet of seamen."),
                         Edward.Jenner = c("In 1796, he did three things I already know about."),
                         John.Snow_4_i.e.Title.WhatUsed.MapYearIllustrates.Recap = c("The father of epidemiology.","He used 'shoe leather epidemiology' to determine that polluted water was the source of cholera.","Snow's map of cholera deaths in Soho in 1854 illustrates the study of DISTRIBUTION of disease.","Snow thought it spread through the water, and used epidemiologic methods to test his hypothesis."),
                         cholera_4 = c("There are 3-5 million cases of cholera a year globally, with ...","... 100,000 deaths globally.","Infection is often mild and may not display any symptoms.","5-10% [more like 5%, right? 5% of 3 million is 150,000] of those infected will develop severe with watery diarrhea, vomiting, and leg cramps with death occurring within hours without treatment (oral rehydration therapy)."),
                         nine.subspecialties.of.epi = c("clinical","reproductive","infectious","environmental","nutritional","pharmaco","social","molecular/genetic","pediatric or geriatric"),
                         ten.examples.of.current.public.health.issues = c("Autoimmune diseases","Chemicals","Climate change","Diabetes","Emerging infectious diseases like Zika and Ebola","Gun violence","Obesity","Tobacco","Unintentional injuries","Vaccines"),
                         do.the.causes.of.many.important.diseases.remain.unexplained = c("Yes."),
                         The.Two.Fundamental.Assumptions.of.Epi = c("Disease does not occur at random.","Disease has causal and preventive factors that can be identifed through systematic investigation."),
                         The.Essence.of.Epi = c("Comparison."),
                         epidemiologic.triangle_with.factors_2Host.4Agent.3Environ = c("For disease to occur there must be an interaction betweeen host, agent, and environment.","Host factors (2): ...","1) biological","2) social","Agent (4): ...","1) biological (bacteria, viruses)","2) chemical (alcohol, toxins)","3) physical (trauma, radiation)","4) nutritional","Environmental factors (3): ...","1) physical (climate, urban/rural, pollution, altitude)","2) social (political, social, economic)","3) herd immunity"),
                         epi.traingle.TB.example = c("Host: poor nutrition; concurrent disease; low immunity","Agent: Mycobacterium tuberculosis","Environment: crowding; poor ventilation; poor sanitation"),
                         three.major.contributors.to.understanding.health.and.disease = c("Basic sciences","Clinical sciences","Population medicine: epidemiology"),
                         clinical.vs.epi_3vs3 = c("Clinical examines the individual.","Clinical describes signs and symptoms in the individual.","Clinical prescribes individual treatment.","Epi does ...","populations; describes groups affected with time and place trends and other variables affecting distribution;...","prescribes and evaluates community-wide interventions."),
                         complementarity.of.clinical.and.epi_2and1 = c("Epi depends on clinical for accurate diagnoses.","Clinical observations may suggest need for further epidemiological investigation.","Epidemiological knowledge helps guide selection of diagnostic procedures: Which tests are best? What's the most likely diagnosis given what is known about the patient AND the disease distribution?"),
                         basic.research.vs.epi_2vs2_and.also.dont.forget.thalidomide = c("Basic research illuminates biological basis of etiology and prevention (establishes biological plausibility).","Basic research provides precise control of covariates like genetics, environment and measurement(???).","Epi quantifies the magnitude of the association in humans.","Epi contributes to public health interventions even before biological mechanisms of disease are understood. - e.g. smoking and cancer; toxic shock syndrome"),
                         what.do.we.know.about.the.public.health.relevance.of.lab.findings_4 = c("Differences between vitro and vivo.","Differences in susceptibility across species.","Difficulties extrapolating across dosages, routes of administration, and lifespan.","Difficulties generalizing results from a controlled setting to free-living populations."),
                         research.strategies.ie.study.designs.in.epi_2_bb4 = c("Experimental: clinical and field trials","Observational: descriptive (person, place and time patterns) and analytic (hypothesis testing)","Analytic study designs: ...","1) ecological","2) cross-sectional","3) CASE-CONTROL","4) COHORT (PROSPECTIVE AND RETROSPECTIVE)"),
                         seven.bullet.points.of.epidemiological.reasoning = c("1) Suspect the possible influence of a factor on the occurrence of disease.","2) Modulate suspicion in response to observations.","3) Formulate hypothesis.","4) Test hypothesis using appropriate comparison groups.","5) Systematically collect and analyze data to determine whether association exists.","6) Assess validity of results - chance, bias, confounding.","7) Assess causality."),
                         applications.of.epi_1_7 = c("The application of epidemiology is to prevent disease/injury and promote health via a scientific approach.","1) by conducting surveillance of disease/injury occurrence in populations","2) by studying past (and future???) trends in health/illness","3) by identifying sections of the population at greatest risk","4) by evaluating the effectiveness of programs","5) by studying natural history of disease","6) by identifying CAUSES","6) by investigating outbreaks"),
                         the.epidemiologic.transition.of.the.last.century_1900 = c("1) pneumonia and inlfuenza - 200/100,000","2) TB - 194","3) diarrhea and enteritis - 142","4) heart disease - 137","5) stroke - 106","6) nephritis - 88","7) accidents - 72","8) cancer - 64","9) senility - 50","10) diphtheria - 40"),
                         transition.part.2_2010_COMPARE.TO.NCHS.2017.REPORT.SCREENSHOT = c("1) heart disease - 178","2) cancer - 172","3) chronic lower respiratory diseases - 42","4) stroke - 39","5) accidents - 37","6) Alzheimers - 25","7) diabetes - 20","8) nephritis - 15","9) pneumonia and influenza - 15","10) suicide - 12"),
                         natural.history.of.disease_def.and.use.and.subdivisions.of.phases = c("Definition: The course of disease from its beginning to its final clinical end points.","Uses (2): ...","1) Provides framework for prevention and control","2) identifies where in the disease's natural history effective intervention can occur.","Phases (6): ...","1) preclinical phase: onset","2) clinical phase (4, followed by outcome): ...","a) symptoms","b) seek care","c) diagnosis","d) treatment","then there's the outcome."),
                         summary.of.ppt_5 = c("What is epidemiology?","It's the study of disease distribution and determinants in populations.","What are the fundamental assumptions?","Disease doesn't occur by chance, and causes can be identifed and predicted through systematic study.","The essence of epidemiology is ...","... comparison.","What are the uses of epi? (4 - BUT REMEMBER HOW THERE WERE 7 BEFORE, IN applications.of.epi_1_7 ???)","1) identification of causes","2) monitoring frequency and distribution","3) measuring disease burden in populations","4) evaluating impact of intervention programs","What are the sub-specialties? - Just remember the 9 from the other flashcard."),
                         five.types.of.determinants = c("physical environment (safe water, clean air)","social environment (culture, beliefs, support)","economic (status, access to care)","individual characteristics (diet, exercise, smoking)","biology and genetics"),
                         COMPARE.4USES.TO.7APPLICATIONS.OF.EPI = c("4: Identifying causes, Monitoring frequency and distribution, Measuring disease burden, and Evaluating intervention programs.","7: Surveillance, study Trends, sections of pop at greater Risk, evaluate Programs, study Natural history of disease, id Causes, and investigate Outbreaks."),
                         COMPARE.6COMPONENTS.TO.4KEYTERMS.OF.EPI = c("6 Components: Study (6 examples, one of which is also one of the 7 applications of epi), Distributions, Determinants (6 types - which one is not included in the list of 5 determinants?), Health-related states, Populations, and Control.","4 Key Terms: Exposure (5, one of which is also one of the 6 components), Disease, Morbidity, and Mortality."))
Epi.Morbidity = list(one = "filler",
                     objectives = c("Define ratio, proportion, rate, and odds as measures of frequency.","Calculatea cumulative incidence, incidence rate and prevalence proportion and understand the difference between these measures.","Identify and calculate commonly used measures of morbidity."),
                     ratio.as.measure.of.freq = c("Division of one quantity by another.","It's the most general term; units and range are variable."),
                     proportion.as.measure.of.freq = c("A ratio in which the numerator is contained in the denominator.","No units.","Range: 0 to 1."),
                     rate.as.measure.of.freq = c("A ratio representing change over time."),
                     odds.as.measure.of.freq = c("Probability of an event DIVIDED BY THE PROABILITY OF A NON-EVENT: P/(1-P).","No units.","Range: 0 to infinity.","Example: 3:1 odds at a horse race. Example 2: If there are 100 smokers and 60 develop a cough, then ... odds of developing a cough are 60:40 or 1.5."),
                     four.things.you.need.to.calculate.a.measure.of.frequency = c("1) count of events","2) population size","3) period of observation","4) constant, K."),
                     cumulative.incidence.CI_4 = c("The cumulative incidence is the risk!","It's the proportion of initially susceptible individuals in a population who become new cases of a disease in a specified period.","The equation is ... (# of new cases of a disease during a given period of time / population AT RISK during that same time)*1000","You could also define CI (risk) as the probability that healthy people will develop disease during a specified time."),
                     incidence.rate.IR_aka = c("Also called incidence density.","It's the number of new cases divded by the person-time of observation."),
                     prevalence.proportion.PP = c("The proportion of the population who have the disease at a given point in time.","The equation's K = 1000. Numerator: # of existing cases of a disease at a given point in time. Denominator: total population."),
                     difference.between_CI.IR.PP = c("???"),
                     commonly.used.measures.of.morbidity = c("???"),
                     the.count_how.to.make.it.descriptive = c("# of cases of a health event","In order to be descriptive, a count must be considered relative to the size of the group."),
                     central.metrological.concern.in.epidemiology = c("Appropriate denominators. (Think in terms of counting and what makes a count descriptive.)"),
                     The.two.categories.of.measurement.for.describing.disease.freq = c("Prevalence.","Incidence."),
                     Prevalent.cases = c("Existing cases (old and new) at a point in time or over a period iof time."),
                     Incident.cases = c("New cases identified over a specified period of time."),
                     Two.kinds.of.prevalence.and.period.prevalence.denominator = c("Point prevalence.","Period prevalence.","The denominator of period prevalence, instead of being total population at a point in time, is the AVERAGE population during a PERIOD of time."),
                     criteria.needed.to.calculate.prevalence_4.plus.1.not.needed = c("1) knowledge of health status of study population.","2) specification of numerator (old and new cases).","3) size of total population.","4) POINT or PERIOD of observation.","We don't need to know time of onset in order to calculate prevalence."),
                     criteria.needed.to.calculate.CI_5.plus.7 = c("1) health status of population","2) time of onset","2) i.e. acute vs chronic (e.g. flu vs cancer)","3) specification of numerator","3) i.e. (A) new cases of disease","3) i.e. (B) excluding existing cases","4) specification of population at risk","4) i.e. (A) denominator should not include people with disease or who are not susceptible--a correction not usually made","4) i.e. (B) over time, the number of people at risk varies; how is this handled? - by using the population size at the mid-point of the time period","5) period of observation","5) i.e. (A) usually 1 year","i.e. (B) if frequency of disease is low, aditional years are included for stability"),
                     IMPORTANT.assumption.for.calculating.risk = c("E.g. - West Nile Virus calculation assumes EVERYONE is at risk of WNV."),
                     when.is.incidence.rate.used_what.does.it.estimate_what.is.the.unit_and.what.is.the.equation = c("It's used when individuals are observed for different lengths of time.","It estimates how quickly people are acquiring the disease.","The unit is 1/time. But isn't it different to say the denominator is person-time rather than just time??","IR or ID = # (of new cases during time period / total person-time at risk during study period)*1000"),
                     more.on.ID_what.it.allows.for.and.denominator.and.example_3 = c("ID allows for individual differentce in time at risk.","The denominator is person-time.","e.g. a rate of .2 cases per 12 person-months = .2 cases per person-year = 20 cases per 100 person-years."),
                     person.time_3 = c("An estimate of the actual time at risk in years, months, or days that all persons contributed to the study.","It accumulates over the duration of the study and for all participants.","A subject is eligible to contribute person-time to the study as long as they remain susceptible (at risk of developing the disease)."),
                     uses.of.prevalence_5 = c("1) NOT a measure of risk","2) used to express 'burden of disease'","3) used by health planners for determining workload","4) used to monitor control programs for chronic conditions","5) if incidence is not available, then prevalence can be used to estimate importance of disease."),
                     uses.of.incidence_3 = c("1) It tells us about change in status from non-diseased to diseased (limited to new cases)","2) etiology - it's a direct indicator of risk of disease","3) for comparing incidence rates in populations."),
                     limitations.of.prevalence.for.etiology_3 =c("1) It favors inclusion of chronic over acute","2) the screening effect","3) it's cross-sectional - the temporality problem."),
                     what.is.the.screening.effect = c("???"),
                     prevalence.depends.on_2 = c("Think P = IxD ...","It depends on incidence, and","duration of disease."),
                     P.equals.IxD = c("Incidence is the water dripping out of the faucet into the tub.","Prevalence is the water in the tub.","Recovery/death is the water draining out of the tub."),
                     QUIZ.LEARNING.EXPERIENCE_if.you.implement.a.CURE.to.a.given.disease.are.you.affecting.the.incidence.rate = c("The answer is no--you're only reducing the prevalence proportion. This is because, by CURING the disease, rather than PREVENTING it, you're still going to get the same frequency of people getting the disease, whom you will then cure. People GET the disease as often as before, but since people don't KEEP the disease for as long as previously (due to the cure) you'll find a lower prevalence when you measure it. So I'm guessing only a PREVENTIVE MEASURE could reduce (both) incidence (and prevalence)."),
                     CI.is.the.risk_Is.it.a.rate.or.a.proportion = c("Darwe said that CI is synonymous with incidence proportion; that would make it a proportion."),
                     risk.equals_not_ = c("Risk is CUMULATIVE incidence!","Apparently risk is NOT incidence RATE! Right? WRONG!!! Risk can be the incidence and incidence can be the risk."))
Epi.Mortality = list(one = "filler",
                     mortality.rate_3 = c("Incidence of death in a population.","Number of deaths occurring in a specified population in a given time period.","You canonly die once so the numerator can only be incident cases (as opposed to both new and old cases in the same individuals?)."),
                     when.is.mortality.rate.a.good.surrogate.for.an.incidence.rate.of.disease + c("When survival is low."),
                     crude.mortality_1 = c("Total death rate in an entire population (generally per 100,000 person-years)."),
                     cause.specific.mortality_2 = c("Rate at which deaths occur for a specific cause.","Number of deaths from specific cause / total population for a given year."),
                     age.specific.mortality = c("Number of deaths for age group / total population in age group for a given year."),
                     two.reasons.for.changes.in.mortality.trends.of.a.disease = c("Artifactual (4 and 3).","Real (4)."),
                     an.artifactual.reason_BOOK.VERSION_2_a4_b3 = c("An error in the numerator due to ...","1) ... errors in diagnosis","2) errors in age","3) changes in coding ruls","4) changes in classification.","An error in the denominator due to ...","... 1) errors in counting population","2) errors in classifying by demographic characteristic (e.g. race, age, etc.)","3) differences in percentages of populations at risk."),
                     a.real.reason_4 = c("1) a change in survivorship without change in incidence","2) change in incidence","change in age composition of the populations","4) a combination of the above factors."),
                     proportionate.mortality.ratio.PMR_eq.and.2Uses = c("# of deaths from given cause in a certain time preiod / total deaths in same time period ... per 100.","It's useful for identifying causes of death.","It gives the relative importance of a specific cause of death in relation to all deaths."),
                     the.magnitude.of.PMR.depends.on_e.g. = c("... deaths from other causes. - e.g. unintentional injuries among children vs among elderly"),
                     case.fatality.CFR_eq.and.2 = c("# of deaths due to disease X / # of cases of disease X ... *100","Refers to the proportion of fatal cases among those who have the disease.","Provides an index of the deadliness of a particular disease within a specific population."),
                     comparing.CFR.and.mortality.rates_2_a2 = c("e.g. rabies: deaths from rabies is rare in the US.","Cause-specific mortality rate would be LOW, because ...","... small numerator (# deaths due to rabies)","and there's a ***total population denominator***","Case fatality, on the other hand, would be HIGH. ...","... because death is almost certain, making the numerator almost the same as the denominator."),
                     infant.mortality_def.eq.1plus2Indicator = c("Defined as deaths under 1 year.","# of deaths among infants < 1 year in given time period / # live births during same period ... *1,000","Infant mortality is traditionally seen as an important indicator of community health. ...","... unmet health needs","unfavorable environmental factors like economic conditions, nutrition, education, sanitation, etc."),
                     maternal.mortality.rate_2Indicators = c("Considered an indicator of ...","adequacy of obstetric care","general level of socioeconomic development."),
                     maternal.mortality_denominator.and.equation = c("Denominator = only live births, though ideally it would be all pregnancies, but the records are more complete for live births than they are for fetal deaths.","# of deaths related to childbirth / # live births ... *100,000"),
                     NEED.TO.KNOW_why.is.crude.birth.rate.not.rate.nor.proportion.and.why.is.it.more.a.ratio = c("It's the number of births per average population."),
                     NEED.TO.KNOW_why.is.attack.rate.not.a.rate.but.instead.a.porportion = c("It is the number of new cases divided by those exposed. e.g. - household cases in outbreaks."),
                     NEED.TO.KNOW_why.is.fertility.rate.not.rate = c(),
                     major.public.health.measures.whose.denominators.are.total.pop = c(),
                     major.public.health.measures.whose.denominators.are.live.births = c())
Epi.Descriptive = list(one = "filler",
                       descriptive.epi = c("The study of the distribution of disease in population groups."),
                       two.things.descriptive.epi.does = c("Summarizes data in a systematic fashion.","Determines non-random variation --> clues to etiology."),
                       five.objectives.of.descriptive.epi = c("1) evaluate trends in health and disease","2) provide for planning, provision, and evaluation","3) identify problems to be studied by analytic methods","4) need for changes in policy or research","5) directions, priorities for public health.","In summary: EVAL/TRENDS PROVIDE/P.P.E. ID/PROBS CHANGES/P.R. DIR/PRIORITIES"),
                       EASY.three.components.of.descriptive.epi = c("Person place time."),
                       person_elements.alphabetical.7.plus.other = c("Age","Family variables","Gender","Marital status","Occupation","Race/Ethnic","Social class","Other personal variables."),
                       person.age_3.things = c("Relationship to mortality and morbidity","J-shaped pattern for age-specific death rates","Related to chronic illness morbidity, less consistently with acute illness."),
                       person.gender_3.things = c("Morbidity higher for females","Mortality higher for males","Gender differences for specific diseases"),
                       male.to.female.ratio.of.age.adjusted.death.rates.for.leading.causes.of.death_10 = c("All causes: 1.8","Homicide and legal intervention: 3.88","Chronic obstructive pulmonary disease: 3.13","Suicide: 3.05","Accidents and adverse effects: 2.96","Chronic liver disease and cirrhosis: 2.19","Heart disease: 2.01","Pneumonia and influenza: 1.86","Congenital anomalies: 1.15","Diabetes mellitus: 1.04"),
                       person.race_2.obvious.things.and.a.Q = c("Tradition","Difference in morbidity and mortality by race/ethnicity","Q: What do differences mean?"),
                       person.social.class_2.things_also.intermediate.variables.and.health.outcomes.of.wealth.education.occupation = c("1) Summarizing variable, linking occupation, education, residence, income, lifestyle","2) Associated with morbidity and mortality","Wealth - Intermediate variables ...","Intermediate variables: Access to health care; access to dietary choices.","Health outcomes: Detection and treatment of illnesses; obesity, blood pressure, cholesterol levels.","Education - Intermediate variable ...","Knowledge, attitude and behavior about diet, smoking, alcohol, exercise, sexual practices, illegal drug use, family planning, prenatal care.","Health outcomes: Heart disease, lung cancer, AIDS, low birth weight.","Occupation - Intermediate variable ...","Exposure to hazards, psychological stresses, physical activity","Health outcomes: Cancer, heart disease, accidents, miscarriages, birth defects, other."),
                       person.occupation_3.things = c("Physical conditions, chemicals, noise, stress","Self-selection bias","Associated with specific diseases"),
                       person.marital.status_1.thing.and.an.explanation = c("Lower mortality among married","Explanation: selection factors, psychological and physical support, sexual exposure or hormonal factors for women, misclassification(???)."),
                       person.family_4.things = c("Family size --> social class","Birth order","Maternal age","Parental deprivation"),
                       person.other.variables_4.things = c("Blood type","Personality traits","Specific immunity","Exposures or behaviors"),
                       place_elements.5 = c("1) Natural boundaries","2) Political subdivisions","3) Urban-rural","4) International","Migrant studies"),
                       place.natural.boundaries_3.things = c("Natural barriers, physical environment","More useful than political boundaries in etiology","Climate, temperature, humidity, rainfall, altitude, and water supply."),
                       place.political.subdivisions_3.things = c("Practical and convenient","Reflect administrative boundaries","Denominator for rates"),
                       place.urban.rural_3.things = c("Differences in morbidity and mortality related to specific factors","Urban factors: pollution, crowding, stress, violence, etc.","Rural factors: specific exposures, accidents."),
                       place.international.comparisons_3.things = c("Relative progress in disease control","Clues to etiology","Differences may be artifactual."),
                       place.migrant.studies_3.things = c("Separate genetic from environmental factors","Compares genetically similar populations under different environmental conditions","Selective factors associated with migration."),
                       time_elements.4 = c("Secular trends","Cyclic or seasonal changes","Short-term fluctuation","Time/place clusters"),
                       time.secular.trends_3.things = c("Long-term trends, years or decades","All diseases, especially cancer","May reflect changes in incidence, survival, or artifacts"),
                       time.cyclic.or.seasonal.trends_2.obvious.things = c("Annual, monthly, weekly","Most helpful in explaining infectious diseases"),
                       time.time.and.place.clusters_2.things = c("Clusters --> etiological agent was introduced into environment at a certain time","Denominator, calculating expected values, complex statistical issues"),
                       why.does.epidemiology.deal.with.populations_1.and.2ab = c("Because population-level data is better for generalization of findings beyond group studied","Because population data is needed for statistical inference: ...","a) Estimating incidence, prevalence","b) Low frequency diseases - need large numbers of at risk individuals to get reliable estimates."),
                       does.epidemiologic.or.public.health.research.need.large.data.sets = c("Yes!"))
Epi.Infectious.Outbreaks = list(one = "filler",
                                
                                how.do.we.know.if.someone.has.an.infection = c("By whether or not they have produced antibodies in response to something."),
                                why.is.bubonic.plague.not.a.problem.anymore = c("Because the bubonic part doesn't lead to the pneumonic part anymore. The pneumonic transmission is what leads to rapid spread within a population, and the septicemic follows it, and causes death. But people can get greated with antibiotics as soon as they start to notice they have a swelling in their regional lymph nodes, thus preventing the bubo from ever rupturing and bacteria spreading to the lungs in the first place."),
                                how.many.cases.of.Ebola.would.it.take.to.call.it.an.epidemic = c("In Oklahoma, it would take only 1 case to call it an epidemic. That's because we're not expecting ANY cases of Ebola. Wow.","Call it a cluster, outbreak, or an epidemic. They're all the same. They're not based on numbers. It just depends on how much you want to get the public's attention."),
                                what.is.the.Merriam.Webster.definition.of.a.pandemic.and.why.can.the.Ebola.outbreak.be.called.a.pandemic = c(),
                                what.is.the.key.term.or.phrase.with.determining.that.we.need.to.do.a.cohort.study.in.an.outbreak = c("Defined population. That's the key term. When we have a defined population, we do a cohort study. But the problem with this definition is that we still need to determine what kinds of criteria can define a population! Duh! What defines the population in the birthday party example? The fact that they were all at the birthday party."),
                                why.9.cases.instead.of.3.cases.on.the.quiz.question.about.what.is.most.suggestive.of.an.outbreak.sharing.a.common.exposure = c("THIS QUESTION IS AWESOME! I GOT IT WRONG BECAUSE I DIDN'T NOTICE THAT IF IT'S ALL WITHIN THE SAME FAMILY, THEN IT'S NOT A FREAKING OUTBREAK DUH! I CAN'T BELIEVE HOW EASY THAT ONE ENDED UP BEING."))
Epi.Exam1.MyQuestions = list(one = "filler",
                           why.are.pneumonia.and.influenza.lumped.together.in.mortality.reports = c(),
                           is.it.useful.or.nonsense.to.the.medical.community.to.say.someone.died.of.senility.or.old.age = c(),
                           what.is.the.screening.effect = c())
                                    

##########################################################################################

## list of lists: a deck with all the decks in it:
deck.of.decks = list(ancient.names.3,
                     BofM.translation.facts.variable,
                     bythenumbers.3,
                     climate.two.per.lecture,
                     #definitions.variable,
                     geography.lists.variable,
                     medical.terminology.3,
                     #name.ten.variable,
                     pathogens.variable,
                     people.important.variable,
                     PharaohList.3,
                     physics.and.technology.variable,
                     political.facts.variable,
                     psalms.in.BofM.4,
                     quotes.variable,
                     random.facts.4,
                     random.lists.variable,
                     science.corroboration.variable,
                     scriptures.variable,
                     TreeLifeList.3)

length(deck.of.decks)
deck.of.decks[1] # gives the entire deck
deck.of.decks[[1]] # gives the exact same thing, minus the [[1]] business before each card
deck.of.decks[[3]][2] # gives every side of just the second card in the third deck
deck.of.decks[[3]][[2]] # gives every side of just the second card in the third deck, minus the front side
deck.of.decks[[3]][[2]][1] # gives the SECOND side (the one right after the front) of the second card
deck.of.decks[[3]][[2]][2] # gives the THIRD side of the second card
names(deck.of.decks[[3]][2]) # HOLY FREAK I FIGURED IT OUT: gives the FIRST side of the second card of the third deck



# Ancient peoples known by two phonetically dissimilar names:
ancient.names.3 = list(one = "filler",
                       Phrygians = c("Mushki","The Mushki were an Iron Age people of Anatolia who appear in sources from Assyria but not from the Hittites. Several authors have connected them with the Moschoi of Greek sources and the Georgian tribe of the Meskhi. Josephus Flavius identified the Moschoi with the Biblical Meshech. ***Assyrian sources identify the Western Mushki with the Phrygians,*** but Greek sources clearly distinguish between the Phrygians and the Moschoi. The Encyclopedia of Indo-European Culture notes that the Armenians according to Diakonoff, are then an amalgam of the Hurrian (and Urartians), Luvians and the Proto-Armenian Mushki (or Armeno-Phrygians) who carried their IE language eastwards across Anatolia.According to Greek mythographers,[9] the first Phrygian Midas had been king of the Moschi (Mushki), also known as Bryges (Brigi) in the western part of archaic Thrace.Assyrian sources from the 8th century BC speak of a king Mita of the Mushki, identified with king Midas of Phrygia. The Phrygians were an ancient Indo-European people, initially dwelling in the southern Balkans – according to Herodotus – under the name of Bryges (Briges), changing it to Phryges after their final migration to Anatolia, via the Hellespont. - Wikipedia 'Mushki' and 'Phrygians'")
                       )


# 27 (26) items in the BMList flashcard deck:
BofM.EME.4 = list(one = "filler",
                   fN6_3 = c("desire","require","And it mattereth not to me that I am particular to give a full account of all the things of my father, for they cannot be written upon these plates, for I desire the room that I may write of the things of God."),
                   fN7_15 = c("choice","judgment","Now behold, I say unto you that if ye will return unto Jerusalem ye shall also perish with them. And now, if ye have choice, go up to the land, and remember the words which I speak unto you, that if ye go ye will also perish"),
                   fN8_12 = c("desirous","desirable","And as I partook of the fruit thereof, it filled my soul with exceeding great joy. Wherefore I began to be desirous that my family should partake of it also, for I knew that it was **desirous** above all other fruit."),
                   fN8_21_A14_27 = c("obtain","reach (a place)","1 Nephi 8:21 - And I saw numberless concourses of people, many of whom were pressing forward, that they might obtain the path which led unto the tree by which I stood. Alma 14:27 - And it came to pass that so great was their fear that they fell to the earth, and did not obtain the outer door of the prison"),
                   fN18_9 = c("to that","until","And after we had been driven forth before the wind for the space of many days, behold, my brethren and the sons of Ishmael and also their wives began to make themselves merry, insomuch that they began to dance, and to sing, and to speak with much rudeness, yea, even **to** that they did forget by what power they had been brought thither"),
                   fN22_13 = c("turn upon","fall upon","And the blood of that great and abominable church, which is the whore of all the earth, shall turn upon their own heads; for they shall war among themselves, and the sword of their own hands shall fall upon their own heads"),
                   sN1_26 = c("manifest","expound, declare","and that which ye call anger was the truth, according to that which is in God, which he could not restrain, manifesting boldly concerning your iniquities"),
                   Ms3_19 = c("but if","unless","For the natural man is an enemy to God, and has been from the fall of Adam, and will be, forever and ever, **but if** he yields to the enticings of the Holy Spirit, and putteth off the natural man and becometh a saint through the atonement of Christ the Lord"),
                   A1_9_A5_53_A8_13 = c("withstand","oppose, deny, contradict","1:9 - Now, because Gideon withstood him with the words of God he was wroth with Gideon, and drew his sword and began to smite him. 5:53 - And now my beloved brethren, I say unto you, can ye withstand these sayings; 8:13 - Now when the people had said this, and withstood all his words, and reviled him, and spit upon him, and caused that he should be cast out of their city, he departed thence"),
                   A7_5_A15_3 = c("by the cause of","on account of, by reason of","nevertheless I do not desire that my joy over you should come by the cause of so much afflictions and sorrow which I have had for the brethren at Zarahemla; 15:3 - Zeezrom lay sick at Sidom, with a burning fever, which was caused by the great tribulations of his mind on account of his wickedness, for he supposed that Alma and Amulek were no more; and he supposed that they had been slain **by the cause of** his iniquity"),
                   A11_2 = c("stripe","whip, beat","Now if a man owed another, and he would not pay that which he did owe, he was complained of to the judge; and the judge executed authority, and sent forth officers that the man should be brought before him; and he judged the man according to the law and the evidences which were brought against him, and thus the man was compelled to pay that which he owed, or be **striped** [not 'stripped'], or be cast out from among the people as a thief and a robber."),
                   A11_25_A24_13_A59_10_tN3_10_Mni7_8 = c("retain","hold back","when thou had it in thy heart to retain them from me. 24:13 - Nay, let us retain our swords that they be not stained with the blood of our brethren. 59:10 - therefore he retained all his force; 3 Nephi 3:10 - which have dissented away from you because of your wickedness in retaining from them their rights of government. Moroni 7:8 - wherefore it is counted unto him the same as if he had retained the gift."),
                   A37_37_A39_10 = c("counsel","to consult, ask counsel of","**Counsel** [not 'counsel with'] the Lord in all thy doings, and he will direct thee for good. 39:10 - And I command you to take it upon you to **counsel** [not 'counsel with'] your elder brothers in your undertakings--for behold, thou art in thy youth and ye stand in need to be nourished by your brothers--and give heed to their counsel."),
                   A44_7 = c("extinct","dead (of an individual)","I will command my men that they shall fall upon you and inflict the wounds of death in your bodies, that ye may become extinct"),
                   A46_17 = c("gave (instead of 'named')","give = describe","And it came to pass that when he poured out his soul to God, he **gave** [not 'named'] all the land which was south of the land Desolation--yea, and in fine all the land, both on the north and on the south--a chosen land and the land of liberty."),
                   A63_5 = c("curious","ingenious","And it came to pass that Hagoth, he being an exceedingly curious man, therefore he went forth and built him an exceedingly large ship"),
                   H1_15 = c("pitch (battle)","set in array","And they came down again that they might pitch battle against the Nephites."),
                   H7_16 = c("hurl","drag","Yea, how could ye have given away to to the enticing of him who art seeking to hurl away your souls down to everlasting misery and endless woe?"),
                   H8_11 = c("depart","divide","Behold, my brethren, have ye not read that God gave power unto one man, even Moses, to smite upon the waters of the Red Sea, and they departed [not 'parted'] hither and thither"),
                   H9_17 = c("detect","expose","And now behold, we will detect this man, and he shall confess his fault and make known unto us the true murderer of this judge."),
                   tN1_29 = c("became","began to act","And there was also a cause of much sorrow among the Lamanites; for behold, they had many children who did grow up and began to wax strong in years, that they became for themselves, and were led away by some who were Zoramites"),
                   Eth6_10a = c("mar","hinder","And thus they were driven forth; and no monster of the sea could break them, neither whale that could mar them"),
                   Eth6_10b = c("break","stop","And thus they were driven forth; and no monster of the sea could break them, neither whale that could mar them"),
                   Eth12_41 = c("commend","recommend (to do a thing)","And now, I would commend you to seek this Jesus of whom the prophets and apostles have written"),
                   Mni10_26 = c("do away","dismiss, reject","And woe unto them which shall do these things away and die, for they die in their sins and they cannot be saved in the kingdom of God."),
                   tpage_scattered = c("scattered","separated (from the main body)","An abridgment taken from the book of Ether also, which is a record of the people of Jared, which were scattered at the time the Lord confounded the language of the people when they were building a tower to get to heaven"))

# y = 5
# x = 2
# z = y
# y = x
# x = z
# y
# x

for (i in 2:27){
  z = BofM.EME.4[[i]][3]
  BofM.EME.4[[i]][3] = BofM.EME.4[[i]][1]
  BofM.EME.4[[i]][1] = z
}
BofM.EME.4 # THIS IS THE ***REAL*** BMLIST!!

# BMList[3] # gives you everything
# BMList[[3]] # gives you just what's "in" scripture reference #3
# BMList[[3]][2] # gives you just the second sub-item in contents of scripture reference #3


BofM.translation.facts.variable = list(one = "filler",
                                       stream.of.dictation_Isaiah.chapters = c("'Milton Blackman, Stan Larson, and Royal Skousen have written on ... textual evidence for a dictated manuscript, citing homophonic miscues, or errors of the ear (e.g. 'no' corrected to read 'know'), scribal anticipation errors (cross-outs that appear before intervening dictation), and the general lack of revisions in the text.[89]'","'Those extensive portions that largely parallel the King James Version of Isaiah 2-14 and 48-54 and Matthew 5-7 seem, even to at least one Mormon scholar, prima facie evidence that Joseph simply used an open copy of the King James Bible, at least for those portions.[90] If so, that would contravene both the testimony of eyewitnesses and textual hints that Joseph was dictating extemporaneously. (For example, as Royal Skousen has shown, Joseph's thematic divisions of Isaiah are inconsistent with the KJV chapter groupings. Also, Cowdery's spellings in these sections mimic the spelling he used in other portions of the Book of Mormon, not King James spelling, showing the relevant passages were the product of dictation rather than scribal copying.[91]'","source: From Darkness Unto Light (?)")
                                       )


## bythenumbers.3 
# topic, number, source
bythenumbers.3 = list(one = "filler",
                      antibody.drug.industry.revenue = c("Antibodies made by the pharmaceutical industry rake in about $100 billion a year.","source: Nature News - 25 May 2018 - Heidi Ledford")
                      )


## climate random facts (variable)
# topic name, facts, sources
climate.random.variable = list(one = "filler",
                               Younger.Dryas = c("The Younger Dryas lasted from 11,000 to 9,000 years BP.","Glaciers began to melt quickly following the maximum glacial advance about 18,000 years BP. Global warming ended the most recent Ice Age beginning around 14,000 years BP.","A brief warm period called the Allerod occurred roughly 11,500 years BP, followed the cooling trend of the Younger Dryas in which glaciers began to readvance.","source: geologycafe.com"),
                               climate.since.Younger.Dryas = c("After the Younger Dryas, the Earth became dramatically warmer. The period between about 8,000 and 7,000 years BP is called the Hypisthermal/Altithermal, or Climatic Optimum. More on the Climatic Optimum to follow ...","The Climatic Optimum was the warmest period within the Holocene. Global temperatures may have been as much as 4 C higher than today. Africa was wetter, and the midwest US was more desert-like.","Since the Climatic Optimum, global temperatures have fallen haltingly: ...","A minor warm period called the Little Climatic Optimum began around 300 BC and lasted for roughly 1500 years. Then a cool period called the Little Ice Age began in the 1300s and ended in the 1840s. Earth has been warming ever since. What would normally be happening next (and right now) ...","... is that the Earth should, based on the patterns of previous interglacial stages and Milankovitch Cycles, be heading towards another ice age after the Climatic Optimum.")
                               )


# 15 (14) cards in climate.two.per.lecture:
climate.two.per.lecture = list(one = "filler",
                               earth.warming = c("Since the mid-20th century, our uncertainty is down to 0.05 Celsius, due to the growing number of monitoring stations around the world.","We don't report an actual temperature, but rather a change from 0, because it's 'easier' to talk about temp changes from a particular station, or from a vast number of stations."),
                               BGH.satellites.strato = c("Microwave emission from oxygen molecules can be measured by satellites in order to get an idea of the temperature for vast volumes of atmosphere.","While the troposphere has warmed, the stratosphere has undergone a cooling trend--an effect scientists would expect in a greenhouse scenario."),
                               iceAges.recently = c("Atmospheric water vapor is O-18-depleted, because O-18 doesn't evaporate as readily. Moreover, the further poleward the condensation goes, the more O-18-depleted it becomes. But more O-18 makes it to the poles the warmer the planet is.","Ten independent studies pointed to the same overall temperature trend during the last thousand years; averaging them yields a very gradual cooling trend up until the 20th century."),
                               greenhouse = c("The distinction between energy balance and equilibrium: When a system reaches the same temperature as its surroundings, that is equilibrium. When the rate at which energy flows into a system is the same as the rate of energy leaving the system, that is energy balance, even if the system's temperature is not the same as its surroundings.","The earth gets 240 watts per m^2 from sunlight. Or, rather, it's about 164 watts per m^2. However, that's an average over 24 hours, over the whole earth. The extraterrestrial solar radiation is actually 1367 watts per square meter at the upper reaches of the atmosphere. At the ground, that amount comes down to 1050 Watts/m^2."),
                               which.molec.planets = c("Which molecule dominates the greenhouse effect? Water vapor! But it precipitates back out of the gas phase within about a week on average.","Earth's orbital eccentricity somehow has 100k- and 413k-year cycles (how, though?). Earth's axial tilt (of 23 deg.) has a 41k-year cycle; in other words, it takes 41k years for the tilt to go from 22.1 to 24.5 degrees and back again, while its rotational axis precesses. It's maximum tilt of 24.5 was reached around 8,700 BC, and the 22.1 minimum will be reached around 11,000 AD."),
                               cycles = c("The water cycle is fast (average time a water molecule spends in the atmosphere is about a week). Guess how long the average carbon-containing molecule stays in the atmosphere? 5 years! WTF?? But the carbon cycle is not just one cycle--there are many widely varying timescales. The 5 year cycle is misleading because if carbon goes into a plant, it will end up cycling right back into the atmosphere really quickly. The carbon we emit will cycle through the atmosphere and transient 'sinks' for a good 300 to 1,000 years.","There are a whopping 39,000 Gt of carbon in the deep ocean. There might be around 10,000 Gt of carbon in fossil fuels, or maybe half that, or maybe twice that. No one knows for sure."),
                               humans = c("Fossil fuel CO2 is C-13-depleted relative to volcanic CO2, because photosynthetic life preferrentially takes up C-12.","Atmospheric CO2 was at 280 ppm before the industrial revolution, and now it's at 400. That's more than a 40% increase. CO2 levels were flat over the last 1,000 years, until the rapid upswing starting with the industrial revolution. Furthermore, while the temperature today might be near the top of the range we see over the last several hundred thousand years, the CO2 is way above."),
                               the.future = c("The equation that calculates the temperature of the earth's surface based solely on its distance from the Sun is a zero-dimensional model. A 1D, two-box climate model involving the atmosphere and the surface, with arrows running between, is more complicated. A 2D model is more complicated still, with a curved surface and heat diffusing from the equator to the poles. Global Circulation Models are 3D models, and are more complicated still, even though the earth is pretty symmetric east to west. Even without adding the fourth dimension--time--computers are a necessity for modeling climate. There are climate model runs that have taken the better part of a year of straight computing.","Validating computer models can be done with the vertical temperature curve of the atmosphere, and the latitudinal variation of precipitation, as well as the Mt. Pinatubo eruption, which proved climate models can be quite accurate."),
                               impacts = c("Wolfson says the sea level rise in the 20th century is 1-2 mm per year. Nerem et al. (PNAS Feb 2018) say that since 1993 it has been rising at 3 mm per year give or take .4 mm. They also say the rate of rise, since 1993, has been accelerating by .084 mm per year per year, give or take .025 mm.","Even with 2 feet of sea level rise, Bangladesh stands to lose 20% of its land area. The economic cost of switching from fossil fuels to renewables should factor in the cost of staying on fossil fuels."),
                               energy = c("The US represents about 4% of the global population, but we consume about a quarter of the total energy budget.","About 87% of global energy consumption is from fossil fuels."),
                               alternatives = c("There will be an energy crisis when peak oil occurs. Peak oil is not when the last rop of oil is used, but rather when either the demand vs supply trend changes from a stable one to a discrepant one.","All the radioactive waste produced by a nuclear power plant in one year could be condensed down into a container the size of the space occupied by a dinner table. Every year, one truck load is all that is needed to restock a nuclear power plant, while a comparable coal-fired plant needs 110 train-cars of coal every week."),
                               sustainable.futures = c("What is a dangerous level of CO2? Double the pre-industrial levels? So 560 ppm?","One wind turbine of the largest size only produces 5 megawatts at its peak. That's 200 times less than the capacity of a coal plant."),
                               Extras = c("Greenhouse: O2 and N2 don't absorb infrared like CO2 and H2O do. These triatomic molecules can vibrate in ways that absorb more radiation, which the diatomics can't do--that's why they trap heat. The greenhouse effect is about 33 Celsius, or 60 Fahrenheit, making the earth's average temperature go up from the 0 F it would be without an atmosphere to about 60 F.","If you look at a graph showing the paleoclimate reconstructions of the last millennium, the recent upswing to today's temperature anomoly is high but only at around .5 deg. Celsius above the next highest temp peak in antiquity. If temperature continues to rise at this rate, by 2100 it will be way off that chart, at 3 deg. Celsius higher. 3 degrees doesn't sound like much until you see that graph."),
                               Questions = c("Why does the precession of the equinoxes have a 26,000-year cycle?","Why exactly, in the real world, do a few degrees matter? Ivar Giaever commented on the significance of the apparent rise in temperature when he stated, 'What does it mean that the temperature has gone up 0.8 degrees Kelvin: probably nothing.'"))


# definitions.variable flashcard deck:
definitions.variable = list(one = "filler",
                     The.World.Bank = c("The World Bank is an international financial institution that provides loans to [not necessarily poorer] countries, with the goal of reducing poverty.","It was created in 1944 at the Bretton Woods Conference along with the IMF (International Monetary Fund). Both are based in Washington DC and work closely with each other.","The President of the bank is traditionally an American, nominated by the US, which is the largest shareholder in the bank (the managing director of the IMF having always been a European).","Joseph Stiglitz was chief economist at the World Bank from '97 to '00.","45 countries pledged $25 billion in aid for the world's poorest countries (in 2007??). The World Bank has received criticism due to wealthier nations funding their own aid projects, like those for diseases.","IDA money is, according to Robert Zoellick, former president of the World Bank, the core funding that the poorest developing countries rely on.","The World Bank has also been criticized, most strongly, for the way it is governed. It is supposed to represent 188 countries, but it is run by a small number of economically powerful ones. Those countries choose the bank's leaders, and their interests dominate its activities. Some have argued that rather than alleviating poverty, its projects have primarily served to expand government bureaucracy."),
                     The.IMF = c(),
                     biomarker = c("Any measurable indicator of some biological state or condition.","Examples: ...","It can also be a substance whose detection indicates a particular disease state, for example, the presence of an antibody may indicate an infection.","Other biomarkers can be based on measures of the electrical activity of the brain (using Electroencephalography (so-called Quantitative electroencephalography (qEEG)) or Magnetoencephalography), volumetric measures of certain brain regions (using Magnetic resonance imaging), saliva testing of natural metabolites, such as saliva nitrite, a surrogate marker for nitric oxide or analysis of urinary exosomes that carry biomarkers of disease.","One example of a commonly used biomarker in medicine is prostate-specific antigen (PSA). This marker can be measured as a proxy of prostate size with rapid changes potentially indicating cancer.","Biomarker type categories in personalized medicine: ...","... Biomarkers used for personalized medicine are typically categorized as either prognostic or predictive. ...","... Prognostic biomarkers indicate outcome regardless of treatment. Predictive biomarkers are used for estimating likelihood of benefitting from a specific therapy.","Biomarkers and precision oncology--cases of typical use: ...","... Biomarkers for precision oncology are typically utilized in the molecular diagnostics of chronic myeloid leukemia, colon, breast, and lung cancer, and in melanoma.","In cell biology: ...","... In cell biology, a biomarker is a molecule that allows the detection and isolation of a particular cell type (for example, the protein Oct-4 is used as a biomarker to identify embryonic stem cells).","In genetics: ...","... In genetics, a biomarker (identified as genetic marker) is a DNA sequence that causes disease or is associated with susceptibility to disease. They can be used to create genetic maps of whatever organism is being studied.","In geology and astrobiology: ...","A biomarker can be any kind of molecule indicating the existence, past or present, of living organisms. In the fields of geology and astrobiology, biomarkers, versus geomarkers, are also known as biosignatures. The term biomarker is also used to describe biological involvement in the generation of petroleum.","History of usage: ...","The widespread use of the term 'biomarker' dates back to as early as 1980. The term 'biological marker' was introduced in 1950s. In 1998 the NIH defined a biomarker as ...","... a characteristic that is objectively measured and evaluated as an indicator of normal biological processes, pathogenic processes, or pharmacologic responses to a therapeutic intervention. To recap, that's 1) normal processes, 2) pathogenic processes, and 3) pharmacologic responses to therapy. THE END."),
                     patriotism = c(),
                     jingoism = c(),
                     nationalism = c(),
                     fascism = c(),
                     totalitarianism = c(),
                     aristocracy = c(),
                     plutocracy = c(),
                     meritocracy = c(),
                     republic = c(),
                     democracy = c(),
                     constitutional.democracy = c(),
                     rule.of.law = c(),
                     capitalism = c(),
                     laissez.faire = c(),
                     state.capitalism = c(),
                     welfare.capitalism = c(),
                     socialism = c(),
                     communism = c(),
                     conservativism = c("","Reagan vs Trump - definitions of conservatism: ...","Reagan: "),
                     liberalism = c(),
                     classical.liberalism = c(),
                     libertarianism = c(),
                     secularism = c(),
                     pluralism_vs_multiculturalism_vs_cultural.relativism = c(),
                     collectivism_vs_inividualism = c(),
                     monophyly_vs_paraphyly_vs_polyphyly = c(),
                     seal_sealing = c(),
                     monotheism_henotheism_monolatry_polytheism_pantheism_panentheism_trinitarianism_unitarianism = c(),
                     deism_atheism_agnosticism_antitheism = c(),
                     empiricism = c(),
                     rationalism_vs_empiricism = c(),
                     The.Enlightenment = c(),
                     The.Rennaissance = c(),
                     political.correctness = c(),
                     propaganda = c(),
                     obscurantism = c(),
                     elitism = c("(i.e. recall James Anderson's claim that his grandmother's Christian faith towered over his own, and that it would be elitist to think that she didn't have much faith after all because she didn't have the sophisticated scholarly knowledge of the Bible that he now does.")
                     )


geography.lists.variable = list(one = "filler",
                                largest.cities.South.America.ten = c("Sao Paulo - 12 M","Lima - 9 M","Bogota - 8 M","Rio de Janeiro - 6 M","Santiago - 5.5 M","Caracas - 3 M","Buenos Aires - 3 M","Salvador - 3 M","Brasilia - 3 M","Fortaleza - 2.5 M","So, it goes Sao Paulo, Lima, Bogota, Rio, Santiago, Caracas, Buenos Aires, Salvador, Brasilia, Fortaleza; which is (country-wise) ...","Brazil, Peru, Colombia, Brazil, Chile, Venezuela, Argentina, Brazil, Brazil, Brazil. So Brazil has 5 of the top 10, 2 of the top 5, and also the biggest city, and also is the only repeat in the top 10.")
                                )


# Try to do 5 of these a day, and get 1000 medical terms in only 200 days! I'll be done in February of 2019!
# link: https://www.health.harvard.edu/a-through-c
medical.terminology.3 = list(one = "filler",
                             five.alpha.reductase = c("Enzyme that converts testosterone into dihydrotestosterone. This hormone can cause the prostate gland to grow abormally.","E that c t into d. This h c c the p g to g a."),
                             ablation = c("A form of treatment that uses electrical energy, heat, cold, alcohol or other modalities to destroy a small section of damaged tissue.","A f of tx that u e e, h, c, a, or other m to d a s s of d t."),
                             accommodation.eyes = c("The eye's ability to focus on objects close up.","E's a t f o o c u."),
                             ACE.inhibitor = c("Angiotensin-converting enzyme inhibitor, a drug used to treat hypertension and congestive heart failure.","A-c e i, a d u to tx h and CHF."),
                             acetaldehyde = c("The main breakdown product of alcohol metabolism; accumulation of it in the bloodstream may produce flushing (feeling of heat in the face or chest) and vomiting.","T m b p of a m; a of it in the b m p f (a f of h in the f or c) and v."),
                             achlorhydria = c("A condition in which the stomach produces little or no acid. This can affect digestion, cause stomach pain, and keep the body from absorbing vitamins and nutrients.","A c in which the s p l or n a. This c a d, c s p, and k the b f a v and n.")
                             )


# name.ten flashcard deck:
name.ten.variable = list(one = "filler",
                         ten.geniuses = c("(This one is good for showing people like Jordan Peterson how stupid it is to think of Trump as a stable genius). I would think it important to pick ten geniuses who best represent either important contributors to current fields of knowledge, or who have unmistakably exuded unusual thinking abilities in some field.","Albert Einstein","Richard Feynman","Alan Turing","Isaac Newton","James Clerk Maxwell","Emmy Noether","William Shakespeare","Leo Tolstoy","Mozart","Socrates"),
                         ten.overpaid.CEOs = c(),
                         ten.historians = c(),
                         ten.philosophers = c(),
                         ten.innovators = c(),
                         ten.inventers = c(),
                         ten.Muslim.majority.countries = c(),
                         ten.Christian.majority.countries = c(),
                         ten.countries.that.have.remained.economically.stagnant.over.the.past.40.years = c(),
                         ten.top.grossing.tech.companies = c(),
                         ten.top.grossing.pharmaceutical.giants = c(),
                         ten.top.grossing.car.companies = c(),
                         ten.biggest.fossil.fuel.companies = c(),
                         ten.most.socialist.countries = c(),
                         ten.most.capitalist.countries = c(),
                         ten.best.evidences.for.evolution = c(),
                         ten.common.creationist.arguments = c("Think Gish-galloping."),
                         ten.global.warming.evidences = c(),
                         ten.global.warming.denial.arguments = c(),
                         ten.capitalist.victories.by.industry = c("For example, has capitalism been a boon to modern medicine?",""),
                         ten.black.intellectuals.scholars.or.scientists = c(),
                         ten.economically.important.minerals = c(),
                         ten.BofM.evidences = c(),
                         ten.examples.of.confounding.factors = c("Duration of treatment for postmenopausal osteoporosis: People who did worse at the end of the study might not have had a poorer outcome because they didn't take alendronate; rather, it might have been because they didn't start out as well in the first place relative to other patients.")
                         )


pathogens.variable = list(one = "filler",
                          survival.on.surfaces = c("MRSA","MRSA: 7 days to 7 months","VRE","VRE: 5 days to 4 months","Acinetobacter","Acinetobacter: 3 days to 5 months","C difficile","C difficile: 5 months","Norovirus","Norovirus: 12 to 28 days","HIV","HIV: minutes to hours","HBV","HBV: 7 days","HCV","HCV: 16 hours to 4 days")
                          )


# important people (INCLUDES 'names_with_ideas' when I need to group people by idea/movement/school of thought/etc.)
people.important.variable = list(one = "filler",
                                 Otto_Warburg = c("1883-1970; son of physicist Emil Warburg (friend of Einstein's), German Jewish (Emil's parents were Orthodox Jews, though Emil had converted to Protestantism) Nobel (1931, for his 'discovery of the nature and mode of action of the respiratory enzyme') laureate","Warburg Hypothesis: To quote Warburg himself, 'Cancer, above all other diseases, has countless secondary causes. But, even for cancer, there is only one prime cause. Summarized in a few words, the prime cause of cancer is the replacement of the respiration of oxygen in normal body cells by a fermentation of sugar.' Warburg thought that cancer is caused by cells generating ATP mainly by anaerobic breakdown of glucose (i.e. fermentation/anaerobic respiration), in contrast to healthy cells, which undergo oxidative breakdown of pyruvate. Because pyruvate is oxidized in the mitochondria, Warburg thought cancer should be interpreted as a mitochondrial dysfunction."),
                                 Walter_Lippmann = c(""),
                                 Benedict_Anderson = c("Died 2015, was a political scientist and historian, best known for his 1983 book Imagined Communities, which explored the orignis of nationalism. A polyglot with an interest in Southeast Asia, Anderson was a distinguished Professor Emeritus of international studies and government and asian studies at Cornell.","Anderson's work on the Cornell Paper that debunked the official story of Indonesia's 30 September Movement and the subsequent anti-Communist purges of 1965-66 led to his expulsion from that country."),
                                 Mark_Hertsgaard = c(""),
                                 Hector_Gramajo = c("(Died 2004, was a general in the Guatemalan Army who served as Defense Minister from 1987 to 1990, during part of the Guatemalan Civil War (1960-1996).","He ran unsuccessfully in 1995 as the presidential candidate for the coalition between the FUN and PID parties.","Some of Gramajo's friends from his graduating class at Escuela Politecnica would become leaders of Guatemala's marxist-inspired guerrilla insurgency.","During his term as Defense Minister, he played a pivotal role in upholding the elected government from at least two coup attempts led by right-wing hardliners in May 1988 and May 1989.","After finishing his term as Defense Minister, Gramajo entered the graduate program at Harvard's JFK School of Government, where he got a degree in public administration in 1991. In 1991, he also served as commencement speaker at the US Army's School of the Americas, in Fort Benning Georgia.","Also in 1991, while getting up to deliver his commencement address (timing/setting not mentioned by Wikipedia) Gramajo was served in a combined suit by the Center for Constitutional Rights, representing 8 Kanjobal Indians who survived the destruction of their village in the eary 1980s, and the American nun Dianna Ortiz, who had been abducted and tortured in Guatemala in 1989. They accused Gramajo of having command responsibility for the abuses that took place. In 1995 Gramajo was judged civilly liable and ordered to pay a total of $47 million in damages. He did not contest the lawsuit or pay the damages. In 1995, the US revoked his entry visa, barring him from the US.","Gramajo died in 2004 at his ranch in Guatemala. He and his son were attacked by a swarm of africanized bees and died from their many stings."),
                                 names_with_ideas_American.decline = c("Walter Russell Mead's 2018 July 4th article mentions three people with specific articles or books predicting imminent/present American decline. Who are they?","Andrew Hacker - political scientist who published 'The End of the American Era' in 1970.","Charles Kupchan - used the same title for a 2002 book.","Stephen Walt used it again for an influential artile in the National Interest.","Mead also adds Paul Kennedy, Fareed Zakaria and Henry Kissinger as three more 'acute foreign-policy thinkers' who have perceived signs of American decline."),
                                 Walter_Williams = c("b. 1936, an African American economist, commentator, and academic. He is the John M. Olin Distinguished Professor of Economics at George Mason University, as well as a syndicated columnist and author known for his classical liberal and libertarian conservative views.","Wrote Liberty versus the Tyranny of Socialism: Controversial Essays; Race and Economics: How Much Can Be Blamed on Discrimination?")
                                 )


# 31 (30) items in the PharaohList.3 flaschard deck:
PharaohList.3 = list(one = "filler", 
                     BC3100a = c("Narmer","Pharaoh of the 1st dynasty in the Early Dynastic Period. First to unite the lands of upper and lower Egypt."),
                     BC3100b = c("Menes","Believed to be the same person as Narmer. Founded the 1st dynasty, according to Herodotus."),
                     BC2670 = c("Djoser","Pharaoh of the 3rd dynasty during the Old Kingdom. Built the step pyramid as part of his funeral complex at Saqqara. Later pharaohs thought his reign to be the beginning of pharaonic history."),
                     BC2613 = c("Sneferu","First pharaoh of the 4th dynasty, Old Kingdom. Built the first true pyramid, changed the orientation of the funerary complex to east-west. Buried in the Red Pyramid."),
                     BC2589 = c("Khufu","aka Greek 'Cheops' / 'Suphis'. 4th dynasty, Old Kingdom. Built the Great Pyramid, with burial chamber in the center rather than at the bottom of the pyramid. Depicted as a cruel tyrant by ancient Greek authors, but as a pious, generous ruler by [contemporary?] Egyptian sources. Main protagonist of the famous Westcar Papyrus. The first imprinted papyri originate from his reign."),
                     BC2558 = c("Khafre","aka Greek 'Cephren'. 4th dynasty, Old Kingdom. Second largest pyramid at Giza. Built the Great Sphinx."),
                     BC2460 = c("Neferefre","5th dynasty, Old Kingdom. His pyramid only reached its lowest courses. Records state he built a sun-temple. A store of papyri was discovered in his pyramid temple providing information about the economic, administrative, and religious practices of his time."),
                     BC2278 = c("Neferkare Pepi II","6th dynasty, Old Kingdom. His rule was Egypt's longest at 94 years. The first half of his reign was prosperous, the second half was a time of economic crisis."),
                     BC2180 = c("Neitiqerty Siptah and Nitocris","Last pharaoh of the 6th dynasty, Old Kingdom. The male king gave rise to the legendary queen mentioned by Herodotus and Manetho. Modern scholars doubt she ever existed."),
                     BC1971 = c("Kheperkare Senusret I","aka Sesostris I. 12th dynasty, Middle Kingdom. Built the White Chapel. Reigned during a time of peace with no records of military campaigns found to date. First pharaoh to begin irrigating the Faiyum. His statues show signs of his actual appearance, marking a new trend in Egyptian art."),
                     BC1878 = c("Sesostris III","12th dynasty, Middle Kingdom. Most powerful pharaoh of the 12th dynasty. The pharaoh of Abraham, according to John Gee. Also Amenemhat III, the next pharaoh, could have been a pharaoh of Abraham's day."),
                     BC1550 = c("Ahmose I","Founder of the 18th dynasty, beginning the New Kingdom. Began the reunification of Egypt after the second intermediate period. Fought battles throughout Egypt, Palestine and Kush as he sought to banish the Hyksos. Goods and artwork show a Minoan influence during this time. He built projects at Memphis and Thebes (his religious capital), especially at Karnak."),
                     BC1541 = c("Amenhotep I","18th dynasty, New Kingdom. Continued his father's building projects and military campaigns. The workmen at Deir el-Medina (the town of the builders of the tombs in the Valley of the Kings) worshipped him and his mother as their patron gods for centuries."),
                     BC1492 = c("Thutmose II","18th dynasty, New Kingdom. His wife, Hatshepsut, attempted to replace his name on monuments with hers. His son later tried to restore his father's name, resulting in conflicting information about this pharaoh's life. His mummy was found in the royal cache at the Temple of Hatshepsut, showing signs of diseases that caused his death."),
                     BC1479 = c("Hatshepsut","18th dynasty, New Kingdom. Second known female ruler of Egypt. Married her uncle. Ruled during the height of Egypt's power. Began her reign merely as regent because her son, the next heir, was still a child. She claimed to be the child of Amun and transformed herself into a king by wearing the symbols of kingship. Known for her building projects. Sent trade missions to the land of Punt and brought back various exotic goods."),
                     BC1458 = c("Thutmose III","18th dynasty, New Kingdom. Conducted military campaigns in the Levant, conquered most of Palestine. His mother's name and monuments were not dishonored until the end of his reign. Built many monuments."),
                     BC1425 = c("Amenhotep II","18th dynasty, New Kingdom. Completed the dishonoring of Hatshepsut's monuments to end any claims by her family to the right to rule. Built various temples, including one to worship Horemakhet, a god associated with the Great Sphinx."),
                     BC1401 = c("Thutmose IV","18th dynasty, New Kingdom. Son of Amenhotep II but not the crown prince. His unexpected rule was legitimized by the Dream Stele, according to which he had a vision of Horemakhet-Khepri-Ra-Atum, while sleeping under the shadow of the Great Sphinx, telling him to restore the Sphinx by removing the sand that had partially buried it, and he would be given the kingship."),
                     BC1390 = c("Amenhotep III","18th dynasty, New Kingdom. Harvests during his time were rich, and he later became a fertility god. Called his palace 'the gleaming Aten' and emphasized the worship of various solar deities. Built a large tomb in the Valley of the Kings and the Colossi of Memnon near his mortuary temple."),
                     BC1352 = c("Amenhotep IV","aka Akhenaten. 18th dynasty, New Kingdom. Came to the throne at a time when the priests of Amun were wealthy and powerful. Built a temple to Aten at Karnak during the first few years of his reign. Built a new capital at Akhetaten (now Amarna), changed his name, and declared Aten the only god in Egypt (described as monolatristic, henotheistic, or even quasi-monotheistic. Started what is known as the Amarna Period."),
                     BC1334 = c("Neferneferuaten","18th dynasty, New Kingdom. Many scholars believe she was Akhenaten's queen, Nefertiti, others that she was Meritaten, the daughter of Akhenaten and Nefertiti."),
                     BC1332 = c("Tutankhamun","18th dynasty, New Kingdom. Son of Akhenaten, became pharaoh at age 9. During the first year of his reign, he abandoned Amarna and restored the cults of the traditional deities. He restored the power of Thebes. Egyptologists found his treasures and body intact when they excavated his tomb in the 1920s."),
                     BC1292 = c("Ramesses I","Founded the 19th dynasty, New Kingdom. Succeeded Horemheb, who was born a commoner, and became a general during the Amarna Period, and who obliterated images of the Amarna pharaohs and destroyed buildings and monuments associated with them. And this pharaoh, who succeeded Horemheb, was also of non-royal birth. This pharaoh's reign marked the transition between the stabilizing reign of Horemheb and the rule of the powerful pharaohs of the 19th dynasty (in particular his son Seti I and grandson Ramesses II)."),
                     BC1290 = c("Seti I","19th dynasty, New Kingdom. Conducted military campaigns to raise money for his building projects. After the enormous social upheavals of Akhenaten's religious reform, Horemheb, Ramesses I and this pharaoh's main priority was to re-establish order in the kingdom and reaffirm Egypt's sovereignty over Canaan and Syria, by confronting the Hittites."),
                     BC1279 = c("Ramesses II (The Great)","19th, NK. Known as Ozymandias in the Greek sources (transliteration of his throne name, Usermaatre Setepenre - 'The justice of Re is powerful--chosen of Re'). One of the most powerful pharaohs of all time. He had at least 95 children. Reigned for 67 years. Built a massive tomb for his children in the Valley of the Kings. He usurped monuments made by older pharaohs by erasing their names and carving his own instead. He declared himself a god before the tenth year of his reign and outlived his 12 oldest sons. He is famous for the Battle of Kadesh, against the Hittites. During his reign, the Egyptian army is estimated to have totaled 100,000 men."),
                     BC1213 = c("Merenptah / Merneptah","19th dynasty, New Kingdom. 13th son of Ramesses II. The Israel Stele is a name commonly given to his victory stele in which he is said to have laid waste to 'ysriar'--the earliest apparent textual reference to Israel known to date, and the first from Egypt."),
                     BC1191 = c("Twosret","19th dynasty, New Kingdom. A female pharaoh, wife of Seti II. Third female pharaoh to rule during the New Kingdom."),
                     BC1186 = c("Ramesses III","20th dynasty, New Kingdom. Not a relative of the previous Ramesses's. After Twosret's death, there was a period of lawlessness that this pharaoh's father ended. This pharaoh had to fight various invaders trying to take advantage of Egypt's internal turmoil. He reorganized temple administrations and land allocations, giving one third of the farm land over to the temples, causing a food shortage which led to one of the first recorded strikes of the workers at Deir el-Medina. It also led to a weakening of the central government."),
                     BC530 = c("Cambyses II","1st Persian dynasty (Achaemenid Empire) = 27th dynasty, in the Late Period. First foreign (absentee) pharaoh, ruling from Persia."),
                     BC332 = c("Alexander the Great","In Egypt, he was portrayed as the son of Nectanebo II, the last pharaoh before the Persian conquest. His defeat of Darius was depicted as Egypt's salvation, 'proving' Egypt was still ruled by an Egyptian. The greatest city he founded was Alexandria, in Egypt. He was regarded as a liberator in Egypt. He was pronounced son of the deity Amun at the Oracle of Siwa Oasis in the Libyan desert. Henceforth, Alexander often referred to Zeus-Ammon as his true father, and after his death, currency depicted him adorned with the horns of a ram as a symbol of his divinity."),
                     BC51_30 = c("Cleopatra VII","Last pharaoh of the Ptolemaic dynasty. Rome was moving to invade Egypt but Mark Antony, her lover, helped her stand against Rome. Then Augustus invaded Egypt and killed Mark Antony. This pharaoh then committed suicide, marking the end of pharaonic Egypt."))


physics.and.technology.variable = list(one = "filler",
                                       ferroelectric.materials = c("Ferroelectric materials have spontaneous electric polarization that can be switched with an external electric field. Applications ...","... Applications: Ferrolectrics thus have applications as capacitors, sensors, and data-storage devices."),
                                       ferroelectric.perovskite.oxides = c("Since the discovery of high-performance BaTiO3 and LiNbO3, ferroelectric perovskite oxides have dominated industrial applications. However ... (cost) ...","... However, these ceramics are expensive to produce as high-quality thin films, and they often contain ...","... they often contain heavy metals like lead 2+, which raises health and environmental concerns. So, what's the alternative? ...","Replacing heavy metals with organic components in the perovskite lattice was considered an alternative, but ...","... but hybrid organic-inorganic perovskites have not shown copmarable ferroelectricity and stability."),
                                       Ye.et.al._organic.ferroelectrics_Science.2018.July.13 = c("Because of the cost, and health and environmental concerns of inorganic ferroelectric materials, organic alternatives have been sought. However, hybrid organic-inorganic perovskites have not shown comparable ferroelectricity and stability. Ye et al. have designed a new class of ferroelectric metal-free perovskites that can compete with ferroelectric oxides (BaTiO3 and LiNbO3). The general chemical formula of perovskites ...","Perovskite general chemical formula: ABX3, where A and B are large and small metal cations, respectively, and X is a bitopic anion, like oxygen 2-, for example. X coordinates to B. About structural topology ...","... The structural topology of the perovskite is important. This topology can be maintained if the A- and X-site ions are replaced with organic molecules within certain size ranges. Finally, recap on the advantages of the new metal-free perovskites designed by Ye et al. ...","These new metal-free perovskite ferroelectrics are simple to make, low cost, and lightweight.")
                                       )


## Political stuff:
# Front side: topic; back sides: some facts with sources
political.facts.variable = list(one = "filler",
                                Israel.2005.disenggmnt.Gaza.part1 = c("aka 'Gaza expulsion', it was the withdrawal of the Israeli army from and dismantling of all 21 Israeli settlements in the Gaza Strip in 2005. Despite the disengagement, the Gaza Strip is sill considered to be under military occuption by Israel by ...","... the UN, international human rights organizations and most legal scholars. - source: Andrew Sanger, 'The Contemporary Law of Blockade and the Gaza Freedom Flotilla', in _Yearbook of International Humanitarian Law 2010_ (2011) ...","... What are the reasons listed by Sanger (2011) for rejecting the Israeli claim of disengagement? ...","Reason 1: Israel's own Disengagement Plan provides for Israel monitoring the external land perimeter of the Gaza Strip. ...","... Reason 2: Israel maintains exclusive authority in Gaza air space and maintains security activity off the Gaza coast. ...","... Reason 3: Israel reserves the right to reenter Gaza at will and maintains a military presence on the Egyptian-Gaza border. ...","Reason 4: Israel continues to control six of Gaza's seven land crossings as well as its maritime borders, and controls the movement of goods and people in and out of the territory. ...","Reason 5: Israeli Defence Force regularly enters parts of the territory, deploys missile attacks, drones and sonic bombs into Gaza. ...","... Reason 6: Israel has declared a no-go buffer zone stretching deep into Gaza: if Gazans enter this zone they can be shot on sight. ...","... Reason 7: Gaza is also dependent upon Israel for water, electricity, telecommunications and other utilities, currency, issuing IDs, and permits to enter and leave the territory. Israel also maintains sole control over the Palestinian Population Registry (through which they classify who is a Palestinian, who is a Gazan or West Banker)."),
                                Israel.2005.disnggmnt.Gaza.part2.aftermath = c("The disengagement/expulsion took place in August and finished in September. Many Israelis resisted and some protesters pelted Israeli military/security with rocks and barricaded themselves. Some told their children to walk with their hands raised in reference to the Holocaust. Right-wingers fiercely opposed Ariel Sharon's action to disengage, including Benjamin Netanyahu, who threated to resign then took it back. But Sharon did win unusual support from the left for the Gaza withdrawal move. So why did Sharon do it? Did he go back on his campaign rhetoric in a sudden inexplicable abandonement of his former professed interests? Apparently it was all about demographics: Israeli democratic majority. Palestinian population growth was outpacing that of Israelis in Gaza.","What happened with the greenhouses? ...","... The following people (7) strongly believed that Israelis left behind all their greenhouses in perfect condition out of an act of selfless charity and good will, only to have them all demolished by rabid Palestinians who wanted to destroy Israeli gifts as much as Israeli people: ...","... Ezra Levant, writing in the Toronto Sun in July 2014. ...","... Charles Krauthammer, writing in the Washington Post in July 2014. ...","... Richard Chesnoff, writing in the Huffington Post in July 2014. ...","J J Goldberg, writing in The Atlantic in July 2014. ...","... Alan Dershowitz, writing in the Jerusalem Post in July 2014. ...","... Lee Smith, writing in Tablet (Israeli magazine), in November 2014. ...","... and Yair Rosenberg, writing in Tablet in July 2014.","What was the reality of the greenhouse debacle? ...","... In reality, the Israeli greenhouse owners were supposed to receive extra compensation for leaving the greenhouses behind. Non-profits like the Economic Cooperation Foundation raised millions in donations. But many Palestinians objected to Israeli settlers being given extra compensation, and the laws of many donor countries and international institutions prevent giving aid to a relatively wealthy country like Israel. About half of the 1,000 acres of greenhouses were destroyed by the owners themselves after giving up waiting for the money they were promised. - source: Steven Erlanger, writing in the New York Times in July 2005 (writing at a time before Palestinians could have destroyed them?? Maybe not?) ...","... The greenhouses that were left were looted by Palestinians. Palestinian Authority security forces attempted to stop them but didn't have the manpower. Then, the Palestinian Economic Development Company invested $20 million and by October the industry was back on its feet. ...","... However, the harvest intended for export via Israel to Europe was lost due to Israeli restrictions on the Karni crossing, which was closed more often than not, leading to losses in excess of $120,000 per day. Economic consultants estimated that the closures cost the whole agricultural sector in Gaza $450,000 a day in lost revenue. 25 truckloads of produce a day through that crossing were needed to render the project viable, but they were lucky to get 3. - sources: ...","... (source for Palestinian looting and Israeli cause of harvest losses: Justin Schwegel, writing in Mondoweiss in August 2014)")
                                )
## Links to sources:
# Andrew Sanger (2011) - Yearbook of International Humanitarian Law 2010 ... https://books.google.com/books?id=hYiIWVlpFzEC&pg=PA429#v=onepage&q&f=false
# Steven Erlanger (July 2005) - New York Times ... https://www.nytimes.com/2005/07/15/world/middleeast/israeli-settlers-demolish-greenhouses-and-gaza-jobs.html
# Justin Schwegel (August 2014) - Mondoweiss ... http://mondoweiss.net/2014/08/propaganda-dehumanize-palestinians/
# Wikipedia source for the Israeli Disengagement of Gaza in 2005: https://en.wikipedia.org/wiki/Israeli_disengagement_from_Gaza#cite_note-occ-2


## Psalms in the Book of Mormon (49 - 1 = 48):
# Phrase (front side), Book of Mormon reference, Psalm reference
psalms.in.BofM.4 = list(zero = "filler",
                      one = c("tender mercies are over all","1 Nephi 1:20","Psalm 145:9"),
                      two = c("to take away my life","1 Nephi 7:16","Psalm 31:13"),
                      three = c("according to the multitude of his tender mercies","1 Nephi 8:8; Ether 6:12","Psalm 51:1 and 69:16"),
                      four = c("rod of iron","1 Nephi 8:19","Psalm 2:9"),
                      five = c("my rock and my salvation","1 Nephi 13:36","Psalm 62:2,6"),
                      six = c("broken heart ... contrite spirit","2 Nephi 2:7; 2 Nephi 4:32; 3 Nephi 9:20; Mormon 2:14; Ether 4:15","Psalm 51:17 and 34:18"),
                      seven = c("great ... goodness ... trust","2 Nephi 4:17,19","Psalm 31:19"),
                      eight = c("cry/cried","2 Nephi 4:23,25","Psalm 30:8"),
                      nine = c("heart ... rejoice","2 Nephi 4:28","Psalm 28:7"),
                      ten = c("because of mine enemies","2 Nephi 4:27,29","Psalm 27:11"),
                      eleven = c("my God and the rock of my salvation","2 Nephi 4:30","Psalm 89:26"),
                      twelve = c("I will praise thee forever","2 Nephi 4:30","Psalm 52:9"),
                      thirteen = c("to take away my life","2 Nephi 5:2","Psalm 31:13"),
                      fourteen = c("clean hands and a pure heart","2 Nephi 25:16; Alma 5:19","Psalm 24:4"),
                      fifteen = c("water my couch/pillow by night with my tears","2 Nephi 33:3","Psalm 6:6"),
                      sixteen = c("as in the provocation ... in the day of temptation in the wilderness","Jacob 1:7","Psalm 95:8"),
                      seventeen = c("pains of hell","Jacob 3:11","Psalm 18:4-5 and 116:3"),
                      eighteen = c("in great mercy ... over all his works","Jacob 4:10","Psalm 145:8-9"),
                      nineteen = c("the stone which the builders refused is become the head stone of the corner","Jacob 4:17","Psalm 118:22"),
                      twenty = c("today if ye will hear his voice, harden not your heart","Jacob 6:6; Alma 12:36","Psalm 95:7-8"),
                      twentyone = c("ye are called ... his sons ... this day he hath begotten you","Mosiah 5:7","Psalm 2:7"),
                      twentytwo = c("at the right hand (of God)","Mosiah 5:9","Psalm 110:1 - this one is a huge stretch"),
                      twentythree = c("break ... bands ... death","Mosiah 15:8; Alma 5:7; Alma 22:14","Psalm 107:14, also 18:4-5 and 116:3"),
                      twentyfour = c("delivered my soul from ... hell","Alma 5:6","Psalm 86:13"),
                      twentyfive = c("mercy ... long-suffering","Alma 5:6","Psalm 86:15"),
                      twentysix = c("chains/sorrows of hell","Alma 5:7,9,10","Psalm 18:5"),
                      twentynine = c("in the paths of righteousness","Alma 7:19","Psalm 23:3"),
                      thirtyone = c("after the order of Melchizedek/his son","Alma 13:1,2,9","Psalm 110:4"),
                      thirtytwo = c("wrath ... enter into ... rest","Alma 12:35,37","Psalm 95:8,11"),
                      thirtythree = c("enter into my rest","Alma 13:6","Psalm 95:11"),
                      thirtyfour = c("pains of hell","Alma 14:6; Alma 26:13","Psalm 18:4-5 and 116:3"),
                      thirtysix = c("darkness ... into marvelous light","Alma 26:3","Psalm 118:23,27"),
                      thirtyseven = c("gather ... give thanks to his holy name ... praise","Alma 26:6,8","Psalm 106:47"),
                      thirtyeight = c("boast of my God","Alma 26:12,35","Psalm 44:8"),
                      forty = c("laugh ... to scorn","Alma 26:23","Psalm 22:7"),
                      fortyone = c("God/the Lord is mindful of","Alma 26:37","Psalm 115:12"),
                      fortytwo = c("bring forth fruit","Alma 32:37","Psalm 1:3"),
                      fortythree = c("in the midst of thy congregations","Alma 33:9","Psalm 74:4"),
                      fortyfour = c("keep ... preserve ... generation","Alma 37:4","Psalm 12:7"),
                      fortyfive = c("as chaff before the wind","Alma 37:15; Mormon 5:16","Psalm 35:5"),
                      fortysix = c("marvelous works","Alma 37:41","Psalm 9:1; also 118:23 and 139:14"),
                      fortyseven = c("because of their transgression ... afflicted","Alma 37:42","Psalm 107:17"),
                      fortynine = c("pure in hear ... shall see God","3 Nephi 12:8","Psalm 24:4,6"),
                      fifty = c("depart from me all ye workers of iniquity","3 Nephi 14:23","Psalm 6:8"),
                      fiftyone = c("the light of thy countenance","3 Nephi 19:25","Psalm 4:6"),
                      fiftytwo = c("out of the mouth of babes","3 Nephi 26:16","Psalm 8:2"),
                      fiftysix = c("counted unto him for righteousness","Moroni 7:7","Psalm 106:31"),
                      fiftyseven = c("none that doeth good, no, not one","Moroni 10:25","Psalm 14:3 (also 53:3--53 and 14 are the same)"))


# quotes (author and topic on front, quote on back, citation info behind that (could be on same side); could break quote up into multipe parts if long):
quotes.variable = c(one = "filler",
                    Boyd.K.Packer.on.inborn.tendencies.gay = c("Original orally communicated talk as delivered over the pulpit in general conference: 'Some suppose that they were preset and cannot overcome what they feel are inborn tendencies toward the impure and unnatural. Not so! Why would our Heavenly Father do that to anyone? Remember he is our Heavenly Father.' - General Conference October 2010","Subsequent clarification: 'Some suppose that they were preset and cannot overcome what they feel are inborn temptation toward the impure and unnatural. Not so! Remember, God is our Heavenly Father.'","The key difference here is that in the clarification, he didn't imply that God would not have created anyone with same-gender attraction, suggesting that he wanted to take back the claim that God would not create people with inborn same-gender attraction (I know same-gender attraction isn't specifically mentioned in the quote, but that's what no one seems to be debating, as the relevance to same-gender attraction seems rather obvious and not worth debating.)","Some puzzling commentary by Gregory Smith: 'President Packer has an extensive publication record on homosexuality--and, as we will now see, the edited version of his conference talk matches precisely what he has always taught. Far from backpedaling, the edited version is a smooth continuation of principles that he has tuaght for over thirty years.' - G. Smith, 'Shattered Glass: The Traditions of Same-Sex Marriage Advocates Encounter Boyd K Packer'","This is not backpedaling, because it's consistent with his earlier material? What makes something backpedaling is not whether or not the clarification matches an earlier, consistent position, but arises from how the clarification compares to the original message being clarified. If it reduces the boldness or brashness of what was originally said, then that is what makes something backpedaling."),
                    Howard.W.Hunter.on.the.Spirit = c("From Preach My Gospel: 'President Howard W Hunter offered this counsel: 'Let me offer a word of caution. ... I think if we are not careful ..., we may begin to try to counterfeit the true influence of the Spirit of the Lord by unworthy and manipulative means. I get concerned when it appears that strong emotion or free-flowing tears are equated with the presence of the Spirit. Certainly the Spirit of the Lord can bring strong emotional feelings, including tears, but that outward manifestation ought not to be confused with the presence of the Spirit itself.' (The Teachings of Howard W Hunter, 184). The Spirit of the Lord always edifies.'"),
                    David.O.McKay.on.evolution = c("Science, dominated by the spirit of religion, is the key to progress and the hope of the future. For example, evolution's beautiful theory of the creation of the world offers many perplexing problems to the inquiring mind. Inevitably, a teacher who denies divine agency in creation, who insists there is no intelligent purpose in it, will infest the student with the thought that all may be chance. I say, that no youth should be so led without a counterbalancing thought ... In the Brigham Young University and every other Church school the teacher can say God is at the helm. - BYU Address, 10 October 1952")
                    )


## Random facts to remember (with 4 components, usually the last one is a source. So, 
# usually a topic name, two facts about it, and a source):
random.facts.4 = list(one = "filler",
                      humans.v.volcanoes_CO2_sources = c("Humans emit ~24 billion tons of CO2 a year","Volcanoes emit ~200 million tons CO2. That means volcanoes emit .83% what humans emit","This is according to the USGS. Also, the federally funded Carbon Dioxide Information Analysis Center says CO2 has gone up year after year, regardless of whether there have been major volcanic eruptions. - from a Scientific American article from probably 2009"),
                      layy.v.lehi_F.M.Cross_plus.sources = c("The connection of the name Lei (classical Arabic Layy, meaning 'bend, twist') with Lehi is based on a linguistic blunder. ... I would not support layy = lehi anymore than I would confuse Lee with Locke.","lyy and lhy cannot be confused in Semitic. The 'h' is a strong laryngeal spirant in Semitic, like the German ch in Buch or Scottish ch in loch.","I read this F M Cross quote from BAR in Jeffrey Chadwick's 2009 Khirbet Beit Lei and the Book of Mormon: An Archaeologist's Evaluation."),
                      Beit.Lei.archaeology_when.was.it.not = c("Was there any ancient Israelite or Jewish settlement at Khirbet Beit Lei during the time of biblical Samson (ca. 1100 BC; Iron Age I) or during the time of the Book of Mormon prophet Lehi (ca. 600 BC; Iron Age II)?","Answer: No. The archaeological survey of Khirbet Beit Lei carried out by Yehuda Dagan in the 1970s revealed no evidence of any Iron Age I or Iron Age II settlement at the site. This was confirmed by the archaeological excavations of Joseph Patrich and Yoram Tsafrir in the 1980s and by the current excavations being carried out by Oren Gutfeld and Yakov Kalman. Not even a single sherd of Iron Age I or II pottery has been found at Khirbet Beit Lei, nor any architectural component from those periods. There was no city, nor town, nor village, nor private estate at Khirbet Beit Lei during the time of Samson or of Lehi.","So could there have been any kind of settlement at Khirbet Beit Lei around 600 BC that could be called a city of Lehi or Beit Lehi, or was there any community at the site around 600 BC in which Lehi might have prophesied? Answer: No city or town; no community at all. No one lived at the site in 600 BC."),
                      Beit.Lei.archaeology_when.was.it.then = c("When was the village at Khirbet Beit Lei an active community?","Answer: All of the archaeologists (mentioned in another flashcard) affirm that the site was first utilized as an oil-pressing complex during the Hellenistic and early Roman periods (ca. 300 BC to AD 70). Underground oil presses and dovecotes have been excavated at the site by Gutfeld and Kalman. Later, during the Byzantine period (fourth to sixth centuries AD), the site was used as a Christian monastic complex with an elaborately decorated chapel, which was excavated by Patrich and Tsafrir. The site seems to have been abandoned thereafter until it was resettled and built up as an Arab village during the Mameluke period (thirteenth to fifteenth centuries AD). The ruins of houses and public buildings from this era have been cleared by Gutfeld and Kalman; pottery remains from the Hellenistic, Roman, Byzantine, and Mameluke periods have been recovered from all over the site.","Was Khirbet Beit Lei ever a Jewish site? Answer: Possibly. According to Gutfeld, the site seems to have been taken over by Jewish forces during the time of John Hyrcanus, around 100 BC. The site may have been used by Jews of Judea from that time until the Roman war against Judea, which culminated in AD 70."),
                      Otto_Warburg = c("1883-1970; son of physicist Emil Warburg (friend of Einstein's), German Jewish (Emil's parents were Orthodox Jews, though Emil had converted to Protestantism) Nobel (1931, for his 'discovery of the nature and mode of action of the respiratory enzyme') laureate","Warburg Hypothesis: To quote Warburg himself, 'Cancer, above all other diseases, has countless secondary causes. But, even for cancer, there is only one prime cause. Summarized in a few words, the prime cause of cancer is the replacement of the respiration of oxygen in normal body cells by a fermentation of sugar.' Warburg thought that cancer is caused by cells generating ATP mainly by anaerobic breakdown of glucose (i.e. fermentation/anaerobic respiration), in contrast to healthy cells, which undergo oxidative breakdown of pyruvate. Because pyruvate is oxidized in the mitochondria, Warburg thought cancer should be interpreted as a mitochondrial dysfunction."),
                      Ediacaran.biota = c("Ediacaran organisms were first discvoered in 1946 in South Australia's Ediacara Hills.","As of 2018, researchers have identified about 200 different types in Ediacaran-age rocks across the world.","Source: Science - 'These half-billion-year-old creatures were animals—but unlike any known today' 8 August 2018")
                      )


## Random lists of variable lengths:
# topic (title?) and date and _Magazine/Journal; author?; facts (however many; with sources if needed); commentaries/interpretations/conclusions (however many; by whom); further reading
# Then develop another deck of flashcards with just facts (first) then you have to guess the source
random.lists.variable = list(one = "filler",
                             intersex.conditions.10 = c("XX CAH (congenital adrenal hyperplasia)","XX progestin-induced virilization","XY AIS (androgen-insensitivity syndrome)","XY 5-ARD (5-alpha reductase deficiency)","XY PMDS (persistent Mullerian duct syndrome)","XY anorchia","XXY (Klinefelter)","XX male (de la Chapelle syndrome)","XY gonadal dysgenesis (Swyer syndrome)","true hermaphroditism"),
                             quantum.mechanics_old.new.schools.17 = c("old - Hendrik Lorentz","old - Max Planck","old - Albert Einstein","new - Niels Bohr","Max Born","Erwin Schrodinger","Arthur Compton","Louis de Broglie","Wolfgang Pauli","Paul Dirac","Werner Heisenberg","Enrico Fermi","Richard Feynman","Julian Schwinger","Sin-Itiro Tomonaga","J Robert Oppenheimer"),
                             pathbreakers.early20th.12 = c("Einstein - 1905 papers on special relativity, Brownian motion, m=E/c^2, and photoelectric effect","Picasso - 1907 Les Demoiselles d'Avignon","Matisse - 1904 Luxe, Calme et Volupte; and 1905 Woman with a Hat","Stravinsky - 1913 Rite of Spring","Schoenberg","Joyce","Eliot","Proust","Diaghilev","Freud","Wittgenstein","and dozens of others"),
                             world.scripture.11 = c("Biyan Lu (Blue Cliff Record) - Zen Classic - 12th century","Wumenguan (Gateless Gate) - Zen Classic - 13th century","Jewish Zohar - 13th century","Rumi's Masnavi - organized around a series of fables - 13th century","Bar-do Thos-grol (Book of the Dead) - Tibetan - 14th century","Mayan Popol Vuh - 16th century","Adi Granth - Sikhs - 17th century","Kitab-i-Iqan (Book of Certitude) - Bahai - 1861","Kitab-i-Aqdas (Most Holy Book) - Bahai - 1873","Hindu Puranas and Epics such as the Mahabharata and Ramayana","Buddhist Jataka tales"),
                             minerals.that.make.helium.5 = c("cleveite","pitchblende","carnotite","monazite","these minerals contain uranium and thorium; the helium is produced as alpha particle emission via radioactive decay"),
                             NT.apocrypha.12 = c("Gospel of Thomas","Gospel of Philip","Apocryphon of James","Pistis Sophia","close candidate for canonization - Epistle of Barnabas","close candidate for canonization - Epistles of Clement","Epistles of Ignatius","Apocalypse of Paul","close candidate for canonization - Shepherd of Hermas","close candidate for canonization - Didache","close candidate for canonization - Apocalypse of Peter (also the only book never accepted as canonical that was commented on by a Church Father","close candidate for canonization - Third Epistle to the Corinthians"),
                             NT.apocrypha.close.candidates.6 = c("Epistle of Barnabas","Epistles of Clement","Shepherd of Hermas","Didache","Apocalypse of Peter","Third Epistle to the Corinthians"),
                             Biblical.Christianity.so.called.10 = c("The following really have no effect on your 'Biblical' Christianity?","Platonism","Aristotelianism","Neoplatonism","Manichaeism","Augustinianism","Averroism","Thomism","Calvinism","modern Fundamentalism"),
                             Rupert.Sheldrake.10dogmasofscience.10 = c("Nature is mechanical; the universe is like a machine; plants and humans and everything can be understood in principle as machines","Matter is unconscious: stars and planets and atoms. Not even humans are conscious.","The laws of nature are fixed: they're the same as they were at the big bang, and they'll stay the same forever into the future","The total amount of matter-energy in the universe is always the same","Nature is purposeless: evolution has no purpose; no other natural process is teleological","Biological heredity is material (he could have just said everything is material according to philosophical materialism","Memories are material modifications in the brain (again, philosophical materialism)","Your mind is in your brain (again, everything is material according to philosophical materialism)","Psychic phenomena are impossible","Mechanistic medicine is the only kind that works--complementary and alternative therapies don't work"),
                             ancientDNA.1Jun18_Science = c("(news article) authors: Achilli, Olivieri, Semino, Torroni",""),
                             Zotero_ice.cores.Roman.lead.pollution.May18_PNAS = c("The paper, 'Lead pollution recorded in Greenland ice indicates European emissions tracked plagues, wars, and imperial expansion during antiquity', is by Joseph McConnell (leading expert in ice core analysis; at the Desert Research Institute (DRI) in Reno), and Andrew Wilson (classical archaeologist at Oxford; Head of School of Archaeology there), and others","Lead pollution in Arctic ice reflects midlatitude emissions from ancient lead-silver mining and smelting.","Though measurements have been extrapolated to infer performance of ancient economies before, including that of the Roman Empire, past studies were based on sparse sampling and inaccurate dating. This study shows that annual European lead emissions closely varied with historical events, like imperial expansion, wars, and major plagues.","Emissions rose coeval with Phoenician expansion, and reached a maximum under the Roman Empire.","Their results indicate sustained economic growth during the first two centuries of the Roman Empire, terminated by the 2nd century Antonine plague.","Materials/Methods (M/M): They analyzed parts of the NGRIP2 ice core--originally collected in 1998--using the DRI's continuous melter system.","M/M cont.: The DRI ice-core analytical system includes two high-resolution inductively coupled plasma mass spectrometers (HR-ICP-MS) operating in parallel. These MSs, along with a lot of other instruments, are capable of simultaneous measurements of ~30 elements, isotopes, and chemical species.","M/M cont.: All measurements were exactly coregistered in depth.","M/M cont.: The part of the NGRIP2 core they examined was between depths 160 m and 580 m. They got 21,000 low (~9 samples / a --meaning 9 samples per year!) and 48,000 medium (~19 samples / a) slit resolution measurements of lead. This afforded them an effective depth resolution for the HR-ICP-MS measurements of .015 m, equivalent to ~12 samples/a during the Roman era.","M/M cont.: Lead concentration detection limits (defined as 3xs the SD of the blank??) were .01 and .18 pg/g--well below the average NGRIP2 lead concentrations of 1.4 and 3 pg/g during background (1100-1000 BC) and Roman periods. Concentrations ranged from .4 pg/g to more than 20 pg/g during these periods.","M/M cont.: It was *assumed* that total lead comprised three components: 1) crustal lead from windblown dust, 2) volcanic lead largely from quiescent emissions, and 3) pollution lead. Deriving the crustal and voclanic lead levels is an interesting, not too lengthy story. See article for full explanation.","Independent ice core chronology for NGRIP2: Because the NGRIP2 measurements did not extend to the surface, they used the distinct sulfur concentration maximum associated with the well-known Samalas volcanic eruption of 1257 as a signal to synchronize the NGRIP2 with the NEEM_2011_S1 volcanic record. The remaining 162 to 582 m NGRIP2 was independently dated through annual layer counting."),
                             Korean.War_Prager = c("The Korean War, starting just 5 years after WWII ended, was the first major clash between democratic and communist forces.","On June 25th, Soviet-backed communist North Korea crossed the 38th parallel and invaded its US-backed, anti-communist South Korean neighbor. [So they started it.] Within weeks the communists had absorbed nearly the entire country.","The US was confused over whether it should or even could respond. (The US had slashed its military budget at the end of WWII.) The Soviet Union sensed America's lack of resolve and encouraged the North's aggression.","Then Truman rushed troops to Pusan to save the last sliver of unconquered territory.","General Douglas MacArthur pushed further north into North Korean territory in order to unite the entire peninsula [so it was a noble aim].","By the 1980s, South Korea had developed into an economic powerhouse.","Was the Korean war worth the cost (35,000 American lives is the only cost mentioned in the video. The loss of Korean lives is never once brought up.)?","Answer: The natural dividend of saving the South was the evolution of today's democratic and prosperous South Korea. It brought 50 million South Koreans undreamed of freedom and affluence, and it brought the world top flight products. South Korea is a model global citizen and a strong ally of the US, standing in stark contrast to [the hole that is] North Korea. Had it not been for the US, the monstruous Pyongyang regime would now rule all of Korea."," The US's intervention was an effort to save South Koreans. It was also a message that the free world under US leadership would no longer tolerate communist military takeovers of free nations. This deterrent prevented the Soviets from trying similar tactics on Japan, Taiwan, and western Europe.","Finally, the Korean War awakened the US to the dangers of disarmament and isolationism. It led to the bipartisan policy of containment of global communism.","Though not a full victory (didn't unite the peninsula), the Korean War was a victory nonetheless, and not just a military victory, but a moral one as well (it kept half the Korean people free). Want further evidence that it was a moral victory? ...","... evidence: Korea did not have a single material resource that would have benefited America."),
                             Korean.War_basic.facts = c("The Korean War started 25 June 1950, ended 27 July 1953."),
                             Mount.St.Helens.eruption.6 = c("table of contents: 1) VEI 2) amount of material ejected 3) size of crater 4) thermal energy 5) death toll 6) homes destroyed.","In 1980, the Mount St. Helens eruption ranked 5 on the Volcanic Explosivity Index with ...","... more than 1 km^3 of ejected material (estimated to have ejected ~1 cubic mile of material).","This left Mount St. Helens 400 meters shorter, with a crater 1-2 miles wide and 640 meters deep.","It released 24 megatons of thermal energy, which is equivalent to ...","... 1,600 Hiroshimas.","57 people were killed.","200 homes were destroyed."),
                             public.health_health.care.policy_important.facts = c("Inspired by remembering that Will VDB was deriding promises made about the potential health benefits stemming from Human Genome Project. What are the benefits made possible or facilitated by the HGP thus far? How do US health care policies get in the way of (or facilitate) realizing benefits from recent research? What do I need to know to sketch a basic outline of the relative performance/success of the approaches of different national governments to health care (a la T. R. Reid)? What do I need to know about public health in various countries of interest to this last question in order to understand the effects of different health care policies?")
                             )


## A list of old or well-known claims or ideas in science that receive important corroboration some time after the initial introduction of the idea and its general acceptance:
# source; however many key points
science.corroboration.variable = list(one = "filler",
                                      planets.are.born.of.disks.of.gas.and.dust.that.coalesce.around.young.stars = c("Science news - 'In a first, astronomers witness the birth of a planet from gas and dust' (2 July 2018)","In the first convincing observation of its kind, astronomers have directly imaged a newborn planet still forming around its star. The planet, hotter than any in our solar system, supports what astronomers have long believed: that such bodies are born of the disks of gas and dust that coalesce around young stars.","Astronomer Kevin Hang at U of Bern says, 'After decades of speculation, it’s nice to actually see one. It’s very comforting.'",""))


# Scripture references
scriptures.variable = list(one = "filler",
                           drunkenness.Bible.4 = c("Isaiah 5:22","I5:22 - Woe unto them that are mighty to drink wine","1 Corinthians 6:10","1C6:10 - nor ... drunkards shall inherit the kingdom of God","Galatians 5:21","G5:21 - drunkenness ... do such things shall not inherit the kingdom","Ephesians 5:18","E5:18 - be not drunk with wine, wherein is excess")
                           )


# 71 (70) dichotomous branchings in this tree of life:
TreeLifeList.3 = list(one = "filler",
                      LUCA = c("Eubacteria","Archaea"),
                      Eubacteria = c("Alphaproteobacteria --> mitochondria","All the other bacteria"),
                      Archaea = c("Lokiarchaeota --> Eukarya","All the other archaea"),
                      Eukarya = c("Bikonta --> plants and chromalveolates","Unikonta --> Amoebazoa, Fungi, Choanozoa + Animalia"),
                      Animalia = c("Porifera, Placozoa, Cnidarians","Eumetazoa"),
                      Eumetazoa = c("Ctenophora","Bilateria"),
                      Bilateria = c("Xenacoelomorpha","Nephrozoa --> Kimberella (possibly a protostome?)"),
                      Nephrozoa = c("Protostomes (~million species alive today)","Deuterostomes (~70,000 species alive today--incliuding 66,000 vertebrates)"),
                      Protostomes = c("Spiralia","Ecdysozoa"),
                      Spiralia = c("Rotifers and Platyhelminthes","Mollusks (largest marine phylum--at 23% of all named marine organisms) and Annelids"),
                      Ecdysozoa = c("Priapulids and Kinorhynchs","Nematodes and Panarthropoda"),
                      Panarthropoda = c("Onychophora","Tardigrades and Arthropods"),
                      Mollusca = c("Gastropods (70k)","Bivalves (20k - clams, oysters, scallops, and mussels) and Cephalopods (900)"),
                      Annelida = c("earthworms","leeches and ragworms"),
                      Gastropods = c("snails","slugs"),
                      Cephalopods = c("squid","octopus (and other things: nautilus, cuttlefish"),
                      Deuterostomes = c("Ambulacraria","Chordates"),
                      Ambulacraria = c("Echinoderms --> sea cucumbers, sea stars, sea lilies, sea urchins, sand dollars,","Hemichordates --> acorn worms"),
                      Chordates = c("Cephalochordates --> Lancelet","Olfactores"),
                      Olfactores = c("Urochordates","Vertebrates"),
                      Urochordates = c("tunicates","Appendicularia"),
                      Vertebrates = c("Cyclostomes","Gnathostomes"),
                      Gnathostomes = c("Chondrychthyes","Osteichthyes"),
                      Osteichthyes = c("Actinopterygii (and Teleostomes)","Sarcopterygii"),
                      Sarcopterygii = c("lobe-finned fish","Tetrapods"),
                      Tetrapods = c("Anamniotes (amphibians)","Amniotes"),
                      Amniotes = c("Sauropsids","Synapsids"),
                      Sauropsids = c("Diapsids","?"),
                      Diapsids = c("Lepidosaurs","Archosaurs"),
                      Lepidosaurs = c("lizards (and don't forget tuataras)","snakes"),
                      Archosaurs = c("Pterosaurs","Dinosaurs"),
                      Dinosaurs = c("Ornithischia","Saurischia"),
                      Ornithischia_ = c("Ceratops","Stegasaurs"),
                      Saurischia = c("Theropods (originated in Middle Triassic, ancestrally carnivorous, but also included other -vores; eventually gave rise to birds)","Sauropods"),
                      Sauropods = c("Brachiosaurus and Brontosaurus","Diplodocus and Apatosaurus"),
                      Synapsids = c("Eupelycaosauria","?"),
                      Therapsids = c("Biarmosuchia","Eutherapsids"),
                      Eutherapsids = c("Dinocephalia","Neotherapsids"),
                      Theriodontia = c("Gorgonopsid","Eutheriodontia"),
                      Cynodontia = c("Cynognathia","Probainognathia"),
                      Mammaliaformes = c("examples include Morganucodon, Docodonta --> Castorocauda, and Hadrocodium","Mammalia"),
                      Mammalia = c("Prototheria --> Monotremes","Theria"),
                      Theria = c("Metatheria --> Marsupials","Eutheria --> Placentals"),
                      Placentals = c("Atlantogenata","Boreoeutheria"),
                      Atlantogenata = c("Xenarthra","Afrotheria"),
                      Xenarthra = c("armadillos","anteaters and sloths"),
                      Afrotheria = c("elephant shrews, tenrecs, and aardvarks","elephants, hyraxes and manatees"),
                      Boreoeutheria = c("Laurasiatheria","Euarchontoglires"),
                      Laurasiatheria = c("Eulipotyphla --> moles, shrews, and hedgehogs","Scrotifera"),
                      Scrotifera = c("Chiroptera","Ferungulata"),
                      Ferungulata = c("Euungulata","Ferae"),
                      Euungulata = c("Cetartiodactyla","Perissodactyla"),
                      Cetartiodactyla = c("Cetaceans","Artiodactyls"),
                      Artiodactyls = c("e.g. pigs, peccaries, hippos, camels, llamas","e.g. giraffes, deer, cattle, buffalos, antelopes"),
                      Perissodactyla = c("horses, donkeys, zebras","rhinos ; and tapirs"),
                      Ferae = c("Pholidota --> pangolins","Carnivora"),
                      Carnivora = c("Feliforms","Caniforms"),
                      Feliforms = c("cats","mongooses; hyenas; civets"),
                      Caniforms = c("bears, wolves, wolverines/red pandas/skunks/etc.","Pinnipeds"),
                      Euarchontaglires = c("Glires ... and also treeshrews","Euarchonta"),
                      Glires = c("rodents","lagomorphs"),
                      Euarchonta = c("Dermoptera (flying lemurs, aka colugos)","Primates"),
                      Primates = c("Strepsirrhines --> lemurs and lorises","Haplorhines"),
                      Haplorhines = c("tarsiers","Simiiformes"),
                      Simiiformes = c("Platyrrhines","Catarrhines"),
                      Catarrhines = c("Cercopithicoidea","Hominoidea"),
                      Hominoidea = c("Hylobates --> gibbons","Hominidae"),
                      Hominidae = c("Ponginae","Homininae"),
                      Homininae = c("Gorillas","Hominini"),
                      Hominini = c("Pan","Homo"))




###################################################################

### USMLE step 1 decks:

## biochem lists:
# chromosomal abnormalities
# vitamins and nutrition
# patient presentations
USMLE.biochem.chromosomal_abnormalities = list(one = "filler",
                     VonHippel.Lindau_disease = c(""),
                     renal_cell_carcinoma = c(""),
                     adult_polycystic_kidney_disease = c(""),
                     Huntington_disease = c(""),
                     Cri.du.chat_syndrom = c(""),
                     familial_adenomatous_polyposis = c(""),
                     Williams_syndrome = c(""),
                     cystic_fibrosis = c(""),
                     Friedreich_ataxia = c(""),
                     Wilms_tumor = c(""),
                     Patau_syndrome = c(""),
                     Wilson_disease = c(""),
                     Prader.Willi_syndrome = c(""),
                     Angelman_syndrome = c(""),
                     neurofibromatosis_type_1 = c(""),
                     neurofibromatosis_type_2 = c(""),
                     Edward_syndrome = c(""),
                     Down_syndrome = c(""),
                     DiGeorge_syndrome = c(""),
                     fragile_X_syndrome = c(""),
                     X.linked_agammaglobulinemia = c(""),
                     Kleinfelter_syndrome = c(""))

USMLE.biochem.vitamins = list(one = "filler",
                              vitamin.B5 = c("aka: pantothenate","description: ","deficiency: "),
                              Wernicke.Korsakoff = c("SS: ","key concept 1: "," key concept 2: "),
                              dry_beriberi = c("SS: ","etiology: "),
                              vitamin.B9 = c("aka: folic acid","description: ","deficiency: ","key concep 1: ","key concept 2: "),
                              vitamin.B3 = c("aka: niacin","description: ","deficiency: "),
                              Hartnup_disease = c("description: ","key concept: ","tx: "),
                              vitamin.B12 = c("aka: cobalamin","description: ","deficiency: ","key concept 1: ","key concept 2: ","key concept 3: ","dx: "),
                              wet_beriberi = c("SS: ","description: "),
                              vitamin.A = c("aka: retinol","description: ","deficiency: ","acute excess: ","chronic excess: ","key concept: "),
                              vitamin.B7 = c("aka: biotin","description: ","deficiency: ","key concept: "),
                              vitamin.B2 = c("aka:riboflavin","description: ","deficiency: "),
                              vitamin.B1 = c("aka: thiamine","description: ","deficiency: ","dx: "),
                              vitamin.B6 = c("aka: pyridoxine"," description: ","deficiency: ","key concept: "))

USMLE.biochem.pt.presentations.3 = list(zero = "filler",
                                        one = c("3-yo male is found to have enlarged testes, large jaw, and a murmur on examination at his well-child visit.","Fragile X syndrome"),
                                        two = c("17-yo male is found to have elevated cholesterol. His father had an MI at 47 and his brother has been found to have normal cholesterol levels.","Familial hypercholesterolemia (hyperlipidemia type IIA)"),
                                        three = c("A 7-yo male is found to fall below an approprate growth percentile. He also had short arms and legs, a large head, and a prominent forehead.","Achondroplasia"),
                                        four = c("A 29-yo male presents to the clinic for blood in his urine and flank pain. His chart shows an elevated blood pressure on multiple occasions.","Autosomal dominant polycistic kidney disease"),
                                        five = c("A 21-yo female presents to the dermatologist for a reddish rash on her face that has been present since childhood. Her doctor notices that she also has lighter patches of skin on her arms.","Tuberous sclerosis"),
                                        six = c("A 3-yo male is found to have red-brown spots on his iris and brown, flat lesions on his trunk.","Neurofibromatosis type 1"),
                                        seven = c("A newborn male is found to have a small head and jaw, clenched hands and rounded bottoms on his feet. A holosystolic murmur is auscultated on examination.","Edwards syndrome"),
                                        eight = c("A 4-yo male is taken to the doctor for frequent falls. His mother reports that he has been having trouble standing and getting up from his falls.","Duchenne muscular dystrophy"),
                                        nine = c("A 38-yo female complains that her arms feel jerky and uncontrollable. She is adopted but was told that her father had difficulty with his mood and was hospitalized many times for aggressive behavior.","Huntington disease"),
                                        ten = c("A newborn boy with an abnormal 1st trimester ultrasound is found to have a cleft palate and rounded soles on the bottom of his feet.","Patau syndrome"),
                                        eleven = c("A 19-yo male complains of blurred vision. His doctor notes that he is exceptionally tall with long and thin limbs, and his sternum appears to protrude on his chest. He has been previously told that he has a benign cardiac murmur.","Marfan syndrome"),
                                        twelve = c("An 11-yo male is hospitalized with pneumonia. This is his third hospitalization in the last two years. He also has trouble gaining weight and is below average in size for his age.","Cystic fibrosis"))
                                      

###################

## microbiology lists:
# pathogens (microbe, disease, pathophysiology / mechanism, dx, tx)

pathogens = list(one = "filler",
                 E.coli.lettuce.2018 = c("The E coli romaine lettuce outbreak in spring and summer of 2018 killed 5 and sickened 210. It's deadliness was due to the fact that it caused hemolytic uremic syndrome.","It's virulence is due to the Shiga toxin, which destroys RBCs. This in turn blocks the kidneys' filtering system, leading to hemolytic uremic syndrome.","dx: ","tx: ")
                 )


length(deck.of.decks)
deck.of.decks[2] # gives the entire deck
deck.of.decks[[2]] # gives the exact same thing, minus the [[1]] business before each card
deck.of.decks[[4]][2] # gives every side of just the second card in the fourth deck
deck.of.decks[[4]][[2]] # gives every side of just the second card in the fourth deck, minus the front side
deck.of.decks[[4]][[2]][1] # gives the SECOND side (the one right after the front) of the second card
deck.of.decks[[4]][[2]][2] # gives the THIRD side of the second card
names(deck.of.decks[[4]][2]) # HOLY FREAK I FIGURED IT OUT: gives the FIRST side of the second card of the fourth deck
length(deck.of.decks[[4]])
names(deck.of.decks[[4]])
length(random.lists.variable[[2]])
length(deck.of.decks[[15]][[2]]) # WORKS! GIVES SAME ANSWER AS LINE ABOVE. DOUBLE BRACKETS FOR BOTH.
deck.of.decks[[15]]["Korean.War_basic.facts"] # No way to use list names to index them. Can only use card names with list numbers as demonstrated here

cardtot = 0
for (i in 1:length(deck.of.decks)){
  Y = length(deck.of.decks[[i]])
  X = sprintf("%i %7.0f",i,length(deck.of.decks[[i]]))
  cardtot = cardtot + Y
  print(X)
}
realcardtotal = cardtot - length(deck.of.decks)
mydf = data.frame(matrix(NA, nrow = realcardtotal, ncol = 2))
mydf[1,] = c(2,2)
mydf[2,] = sample(100,2)
for (i in 3:10){
  mydf[i,] = sample(100,2)
}
mydf[214,] = c(1,2)

##########################################################################
# THIS IS MONEY (FIGURING OUT HOW TO FIND IDENTICAL ROWS FROM TWO DIFFERENT DATA FRAMES):
Amydf = data.frame(matrix(NA, nrow = 10, ncol = 2))
Bmydf = data.frame(matrix(NA, nrow = 10, ncol = 2))
for (i in 1:10){
  Amydf[i,] = sample(10,2)
  Bmydf[i,] = sample(10,2)
}
myanswer = Amydf[10,] %in% Bmydf[3,]
class(myanswer)
sum(myanswer)
# THIS IS THE MONEY PART:
for (i in 1:10){
  for (j in 1:10){
    if (sum(Amydf[i,] %in% Bmydf[j,]) == 2){ # THE SECRET WAS FIGURING OUT TO SET ***THE SUM*** OF THE %in% OPERATION EQUAL TO 2, CUZ THAT'S TWO TRUES!
      print(c(i,j,Amydf[i,] %in% Bmydf[j,])) 
    }else{print("")}
  }
}
##########################################################################

for (i in 1:length(random.lists.variable)){
  X = sprintf("%i %7.0f",i,length(random.lists.variable[[i]]))
  print(X)
}
  
deckofdecks = function(lols){
  cardtotal = 0
  for (i in 1:length(lols)){
    
  }
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  sampdecks = 1:length(lols)
  dnum = 1:length(lols) # Because every deck's first card needs to be in the data frame to begin with
  cnum = 1
  dnum.cnum = data.frame(col1 = c(dnum), col2 = c(cnum))
  while (continue != ""){
    dnum = sample(sampdecks, 1)
    cnum = sample(2:length(lols[[dnum]])) # starting at 2 you avoid the filler card altogether
    print(names(lols[[dnum]][cnum]))
    for (i in 1:length(lols[[dnum]][[cnum]])){
      show = readline(prompt = "Hit 'l' to show next side: ")
      if (show == "l"){
        print(lols[[dnum]][[cnum]][i])
      } else{print("you suck.")}
    }
    
    dnum.cnum = data.frame(col1 = c(dnum), col2 = c(cnum))
  }
}

sampdecks = 1:length(deck.of.decks)
dnum = 1:length(deck.of.decks) # Because every deck's first card needs to be in the data frame to begin with
cnum = 1
dnum.cnum = data.frame(col1 = c(dnum), col2 = c(cnum)) # IT WORKS!


df1 = data.frame(col1 = sample(10,5), col2 = sample(10,5))
df2 = data.frame(col1 = 1, col2 = 3)
library(plyr)
match_df(df1,df2)
df3 = data.frame(col1 = 1, col2 = 2)
answer = match_df(df1,df3)
class(answer)
answer$col1 = as.numeric(answer$col1); answer$col1 > 0
answer$col2
identical(integer(0),answer$col1)
df4 = data.frame(col1 = 8, col2 = 7)
answer2 = match_df(df1,df4)
answer2$col1 = as.numeric(answer2$col1); answer2$col1 > 0
answer2$col2

### flashcards for many-sided cards (best for lists of variable lengths):
deckofdecks = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    if (length(inum.cum) == length(yourlist)-1){
      inum = mysamp[! mysamp %in% inum.cum]
      print(c(length(yourlist)-1, names(yourlist)[inum]))
      inum.cum = c(inum.cum,inum)
      print(inum.cum)
      Sys.sleep(7)
      for (j in 1:length(yourlist[[inum]])){
        print(yourlist[[inum]][j])
        Sys.sleep(10)
      }
      continue = ""
    } else{
      inum = sample(mysamp[-inum.cum],1)
      print(c(i, names(yourlist)[inum]))
      for (j in 1:length(yourlist[[inum]])){
        show = readline(prompt = "Hit 'l' to show next side: ")
        if (show == "l"){
          print(yourlist[[inum]][j])
        } else{print("you suck")}
      }
      inum.cum = c(inum.cum,inum)
      i = i+1
      continue = readline(prompt = "Hit 'y' to continue, enter to quit: ") 
    }
  }
  print("Peace.")
}









