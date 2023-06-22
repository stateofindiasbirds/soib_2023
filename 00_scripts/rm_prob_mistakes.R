# function to remove probable mistakes, during cleaning of raw data (improving data quality)

# probable mistakes can be of different types:
#   1. entire checklists
#   2. species-checklist combinations
#   3. species-observer combinations
#   4. species-admin unit (state or district) combinations
#   5. species-admin unit-date combinations
#   6. latlong-admin unit (state or district) combinations (or regions)

rm_prob_mistakes <- function(data) {
  
  # creating temporary regions to use in filters
  
  data <- data %>% 
    mutate(TEMP.REGION = case_when(
      
      COUNTY %in% c("Aurangabad", "Parbhani", "Beed", "Hingoli", "Jalna", "Buldhana", 
                    "Washim", "Dhule", "Solapur", "Ahmednagar", "Latur", "Osmanabad", 
                    "Yavatmal", "Wardha") ~ "MH_plains",
      (COUNTY == "Jalgaon" & LATITUDE < 21.292) |
        (COUNTY == "Amravati" & LATITUDE < 21.209) |
        (COUNTY == "Akola" & LATITUDE < 21.209) |
        (COUNTY == "Nagpur" & LATITUDE < 21.398 & LONGITUDE < 79.400) ~ "MH_plains",
      
      COUNTY %in% c("Thiruvallur", "Ranipet", "Kancheepuram", "Chennai", "Cuddalore", 
                    "Ariyalur", "Puducherry") |
        COUNTY == "Viluppuram" & LONGITUDE > 79.42 ~ "TN_N-plains"
      
      COUNTY %in% c("Karur", "Thiruvarur", "Thanjavur", "Nagapattinam") |
        COUNTY == "Tiruchirappalli" & LATITUDE < 11.138 ~ "TN_C-plains"
      
      COUNTY %in% c("Sivagangai", "Pudukkottai", "Ramanathapuram", "Thoothukudi") ~ "TN_S-plains",
      (COUNTY == "Virudhunagar" & LONGITUDE > 77.725) |
        (COUNTY == "Tenkasi" & LONGITUDE > 77.351) |
        (COUNTY == "Tirunelveli" & LONGITUDE > 77.572) |
        (COUNTY == "Madurai" & LATITUDE < 10.07 & LONGITUDE > 77.846) ~ "TN_S-plains",
      
      COUNTY %in% c("Vellore", "Tiruvannamalai", "Kallakurichi", "Tirupathur", "Krishnagiri", 
                    "Dharmapuri", "Salem", "Namakkal") ~ "TN_EG-N"
      
    ))

# 1. entire checklists ----------------------------------------------------

  mistake1 <- data %>% 
    filter(SAMPLING.EVENT.IDENTIFIER %in% c("asd")) %>% 
    distinct(SAMPLING.EVENT.IDENTIFIER)
  

# 2. species-checklist combinations ---------------------------------------

  mistake2 <- data %>% 
    filter(
      (COMMON.NAME == "Blyth’s Tragopan" & SAMPLING.EVENT.IDENTIFIER == "S21274851") |
        (COMMON.NAME == "Himalayan Monal" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S36449309","S110202874"
        )) |
        (COMMON.NAME == "Kalij Pheasant" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S63416231","S64856967"
        )) |
        (COMMON.NAME == "Red Spurfowl" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S76110147", "S132265895", "S98984072", "S25573488", "S25762331", "S122211593", 
          "S84162145", "S74797922", "S41925375", "S34004741", "S34201642", "S63349162", 
          "S121904493", "S72249154", "S20388830", "S20388995", "S20840289", "S12275556", 
          "S31592561", "S21501918", "S20437175", "S20399293", "S34358305"
        )) |
        (COMMON.NAME == "Painted Spurfowl" & SAMPLING.EVENT.IDENTIFIER == "S134340928") |
        (COMMON.NAME == "Mountain Bamboo-Partridge" & SAMPLING.EVENT.IDENTIFIER == "S21274851") |
        (COMMON.NAME == "Red Junglefowl" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S108982385","S67570561"
        )) |
        (COMMON.NAME == "Grey Junglefowl" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S98615153", "S127159840", "S34661870", "S23066272", "S41125176", 
          "S132825863", "S26634409", "S65035598", "S63037541", "S41925375"
        )) |
        (COMMON.NAME == "Black Francolin" & SAMPLING.EVENT.IDENTIFIER == "S60933821") |
        (COMMON.NAME == "Painted Francolin" & SAMPLING.EVENT.IDENTIFIER == "S57803190") |
        (COMMON.NAME == "Rain Quail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S48472342", "S28738842", "S42271891", "S42637748"
        )) |
        (COMMON.NAME == "Jungle Bush-Quail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S76962924", "S89878061", "S65125519"
        )) |
        (COMMON.NAME == "Rock Bush-Quail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S24991930", "S32453722", "S103366454"
        )) |
        (COMMON.NAME == "Lesser Flamingo" & SAMPLING.EVENT.IDENTIFIER == "S126888465") |
        (COMMON.NAME == "Ashy Wood-Pigeon" & SAMPLING.EVENT.IDENTIFIER == "S93699607") |
        (COMMON.NAME == "Spotted Dove" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S113162586", "S70796689"
        )) |
        (COMMON.NAME == "Barred Cuckoo-Dove" & SAMPLING.EVENT.IDENTIFIER == "S133909460") |
        (COMMON.NAME == "Asian Emerald Dove" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S126183847", "S126488688"
        )) |
        (COMMON.NAME == "Green Imperial-Pigeon" & SAMPLING.EVENT.IDENTIFIER == "S20639077") |
        (COMMON.NAME == "Malabar Imperial-Pigeon" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S21879421", "S96866144", "S84526237"
        )) |
        (COMMON.NAME == "Mountain Imperial-Pigeon" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S55422829", "S137655719"
        )) |
        (COMMON.NAME == "Chestnut-bellied Sandgrouse" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S137616888", "S64845421"
        )) |
        (COMMON.NAME == "Greater Coucal" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S42608784", "S43386229"
        )) |
        (COMMON.NAME == "Green-billed Malkoha" & SAMPLING.EVENT.IDENTIFIER == "S125158253") |
        (COMMON.NAME == "Plaintive Cuckoo" & SAMPLING.EVENT.IDENTIFIER == "S90844755") |
        (COMMON.NAME == "Fork-tailed Drongo-Cuckoo" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S96359379", "S24640058"
        )) |
        (COMMON.NAME == "Square-tailed Drongo-Cuckoo" & SAMPLING.EVENT.IDENTIFIER == "S64379861") |
        (COMMON.NAME == "Common Hawk-Cuckoo" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S139777248", "S28823684", "S20984093", "S55301209", "S46323372", "S67672676",
          "S28600623", "S37961563", "S31420463", "S68225437", "S66660738", "S137374061"
        )) |
        (COMMON.NAME == "Hodgson’s Hawk-Cuckoo" & SAMPLING.EVENT.IDENTIFIER == "S55246155") |
        (COMMON.NAME == "Indian Cuckoo" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S97378065", "S107590529", "S49846553", "S61708615", "S131745447"
        )) |
        (COMMON.NAME == "Himalayan Cuckoo" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S32601523", "S28511840", "S25364525", "S28511850", "S25364524", 
          "S21535948", "S21504333", "S21503610"
        )) |
        (COMMON.NAME == "White-rumped Needletail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S17625842", "S81990142", "S88790248", "S24265685", "S24265700", 
          "S24868786", "S59681703"
        )) |
        (COMMON.NAME == "Silver-backed Needletail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S57135779", "S68120528", "S55463823", "S55463822", "S55463821"
        )) |
        (COMMON.NAME == "Indian Swiftlet" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S21877776", "S22141232", "S23966644", "S23511550", "S22149290", "S82135536"
        )) |
        (COMMON.NAME == "Alpine Swift" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S58044659", "S40781715", "S22508834", "S63166306", "S50709313"
        )) |
        (COMMON.NAME == "Dark-rumped Swift" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S136596908", "S133953587"
        )) |
        (COMMON.NAME == "Crested Treeswift" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S35120686", "S39356909", "S54124046", "S42608562", "S53287943", "S79257174", 
          "S103353216", "S103348496", "S86148123", "S46655701", "S32263134", "S48844071",
          "S91979068"
        )) |
        (COMMON.NAME == "Slaty-breasted Rail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S70765821", "S54062383"
        )) |
        (COMMON.NAME == "Brown Crake" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S64000111", "S83604984", "S122810982", "S41982536", "S96928236", "S49771153", 
          "S55009673", "S52121764", "S80442160", "S96270389", "S63661563", "S64807222", 
          "S63667366"
        )) |
        (COMMON.NAME == "Sarus Crane" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S83259843", "S118940543", "S99553961"
        )) |
        (COMMON.NAME == "Great Thick-knee" & SAMPLING.EVENT.IDENTIFIER == "S28771713") |
        (COMMON.NAME == "Eurasian Oystercatcher" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S67718859", "S132754026", "S64305472"
        )) |
        (COMMON.NAME == "Black-bellied Plover" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S123385002", "S34218448", "S27921471", "S99277147", "S34492614", "S44886609", 
          "S70115622", "S81855275", "S81815220", "S82728052"
        )) |
        (COMMON.NAME == "River Lapwing" & SAMPLING.EVENT.IDENTIFIER == "S37500970") |
        (COMMON.NAME == "Lesser Sand-Plover" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S122169372", "S41043890", "S41021293", "S41021217", "S33010135", "S32999682", 
          "S60810032", "S52467678", "S62946445", "S56307563", "S33712996", "S33712788", 
          "S81515840", "S33982984", "S27014963", "S26186397", "S127063577", "S77074722", 
          "S127143144", "S127142452", "S127142209", "S51075394", "S78637842", "S81568637", 
          "S126692010", "S41864652", "S68908985", "S101336692", "S26976203", "S72989252", 
          "S99684662", "S78454227", "S79001575", "S87147483", "S102669179", "S77475777", 
          "S61442808"
        )) |
        (COMMON.NAME == "Greater Sand-Plover" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S125365793", "S88961247", "S128664706", "S128643701", "S128638685", "S32109548", 
          "S88438661", "S96353226", "S45814939", "S101406933", "S101196659", "S126033935", 
          "S126033807", "S125832796", "S94207453", "S136916725", "S131157478", "S124752456", 
          "S124747800", " S124747060", "S129764356", "S98181626", "S97336646", "S28688880", 
          "S28737013", "S50224949", "S47638332", "S49847497", " S60673251", "S41787524", 
          "S41788174", "S128149132", "S125677365", "S125677447", "S125689359", "S125677365", 
          "S125677447", "S125689359", "S128144960", "S129085199", "S96326239", "S57280565", 
          "S57725730", "S68117264", "S66478342"
        )) |
        (COMMON.NAME == "Bronze-winged Jacana" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S77327626", "S47898177"
        )) |
        (COMMON.NAME == "Whimbrel" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S94207453", "S57280565", "S120580604", "S88961247", "S68954928", "S68803520", 
          "S44758620", "S19732637", "S138631110", "S24292330", "S89528530", "S103331869",
          "S87228508", "S32040641", "S24620534", "S125598879", "S124981679", "S124981422", 
          "S82682321", "S64075567", "S64047969", "S63793146", "S63898127", "S65388753", 
          "S65351309", "S65351251", " S65346803", "S125671676", "S107698669", "S47207126", 
          "S82225746", "S31659725", "S21008563", "S83444263", "S42760427", "S42196928", 
          "S103396838", "S101328387", "S101255522", " S101254510", "S101254421", "S51076043", 
          "S103048651", "S102720263", "S102468635", "S65314482", "S64305472", "S64380910", 
          "S64353757", "S21548228", "S21547843", "S41669336", "S41668652", "S41666492", 
          "S58556628", "S41791079", "S41490557", "S41486253", "S41424753", "S32790587"
        )) |
        (COMMON.NAME == "Bar-tailed Godwit" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S129764356", "S35631022", "S127566274", "S127020270", "S126907180", "S125011091", 
          "S99310300", "S115052000", "S98061800", "S123772783", "S131060622", "S125927262", 
          "S121977813"
        )) |
        (COMMON.NAME == "Ruddy Turnstone" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S125365793", "S94207453", "S95259238", "S35271945", "S34754628", "S34754627", 
          "S33762033", "S78041623", "S83505365", "S14038496", "S71040766"
        )) |
        (COMMON.NAME == "Great Knot" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S96924803", "S126555184"
        )) |
        (COMMON.NAME == "Long-toed Stint" & SAMPLING.EVENT.IDENTIFIER == "S33777908") |
        (COMMON.NAME == "Sanderling" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S127159051", "S99502396", "S127736849", "S125691173", "S127861562", "S136914849", 
          "S99901327", "S124509991", "S26193571", "S97652926"
        )) |
        (COMMON.NAME == "Long-toed Stint" & SAMPLING.EVENT.IDENTIFIER == "S33777908") |
        (COMMON.NAME == "Terek Sandpiper" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S36033025", "S35630666", "S33422535", "S50766480", "S31619331", "S51245796", 
          "S51245795", "S51245793", "S34549167", "S33653238", "S130519567", "S54772905", 
          "S64140254", "S64139483", "S64080154"
        )) |
        (COMMON.NAME == "Small Buttonquail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S61522072", "S89775462"
        )) |
        (COMMON.NAME == "Yellow-legged Buttonquail" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S118430336", "S117435760", "S117307799"
        )) |
        (COMMON.NAME == "Crab Plover" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S36033025", "S35630666", "S33422535", "S134096062", "S96370806"
        )) |
        (COMMON.NAME == "Indian Courser" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S97784644", "S127306334", "S83003400"
        )) |
        (COMMON.NAME == "Little Tern" & SAMPLING.EVENT.IDENTIFIER == "S140952496") |
        (COMMON.NAME == "Gull-billed Tern" & SAMPLING.EVENT.IDENTIFIER == "S42719659") |
        (COMMON.NAME == "White-winged Tern" & SAMPLING.EVENT.IDENTIFIER == "S52672155") |
        (COMMON.NAME == "Common Tern" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S63540655", "S54990627", "S50437242", "S54772905", "S95411037", "S102831394", 
          "S123771523", "S129895197", "S120874060", "S120873941", "S120873899", "S103203226", 
          "S103156027", "S41319785", "S123993212", "S24618541", "S51012577", "S33956972", "S12339162"
        )) |
        (COMMON.NAME == "Great Crested Tern" & SAMPLING.EVENT.IDENTIFIER == "S24618541") |
        (COMMON.NAME == "Sandwich Tern" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S62520349", "S94637178", "S67017146", "S86698583", "S75473160", "S74922890"
        )) |
        (COMMON.NAME == "Lesser Crested Tern" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S57280565", "S35630802", "S34910005", "S34835793", "S34761087", "S34758889", 
          "S62520349", "S36033025", "S35630666", "S33422535", "S62357295", "S99221178", 
          "S99220121", "S99624848", "S95659171", "S95244418", "S51891676"
        )) |
        (COMMON.NAME == "Asian Woolly-necked Stork" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S97227016", "S97194628", "S97186140", "S97185402", "S96872172", "S111302557", "S110664590"
        )) |
        (COMMON.NAME == "White Stork" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S43131285", "S100036334", "S99923227", "S61442808", "S38778061", "S38778087", 
          "S42966179", "S43019630", "S55899926", "S55905151", "S43023811", "S54439640", 
          "S88947144", "S127557849", "S67387400", "S97695089", "S98864749"
        )) |
        (COMMON.NAME == "Black-necked Stork" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S70264669", "S100076599", "S42512082"
        )) |
        (COMMON.NAME == "Indian Cormorant" & SAMPLING.EVENT.IDENTIFIER == "S105410896") |
        (COMMON.NAME == "Great White Pelican" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S122642372", "S98271096"
        )) |
        (COMMON.NAME == "Red-naped Ibis" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S37639532", "S75847942"
        )) |
        (COMMON.NAME == "Jerdon’s Baza" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S60606407", "S60606339", "S46292979", "S67613913"
        )) |
        (COMMON.NAME == "Red-headed Vulture" & SAMPLING.EVENT.IDENTIFIER == "S122331698") |
        (COMMON.NAME == "Slender-billed Vulture" & SAMPLING.EVENT.IDENTIFIER == "S43496484") |
        (COMMON.NAME == "Crested Serpent-Eagle" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S72637664", "S57621212", "S70945094", "S70912816", "S49816263", "S136113707", 
          "S85741691", "S42965305", "S42919344", "S102965818", "S75244233", "S35784656", 
          "S63893280", "S62261189", "S54608475", "S29668389", "S133737611", "S91478425", 
          "S79623482", "S68917585", "S53078026", "S56297908", "S43974086", "S70196805", 
          "S21930328", "S126123586", "S96938135", "S104264106", "S35105786"
        )) |
        (COMMON.NAME == "Changeable Hawk-Eagle" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S32263134", "S26605014", "S127071945", "S127072092", "S40389585", "S117494023", 
          "S65809077", "S102965818"
        )) |
        (COMMON.NAME == "Rufous-bellied Eagle" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S119816069", "S36134519", "S32915515", "S127982412", "S99308861", "S40436853", 
          "S90210375", "S97691821", "S83785523", "S83785027"
        )) |
        (COMMON.NAME == "Black Eagle" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S70196805", "S81052731", "S123237920", "S88725142"
        )) |
        (COMMON.NAME == "Tawny Eagle" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S82783539", "S82782788", "S17179271", "S34181478", "S29112916", "S49247113", 
          "S53923521", "S57314082", "S35891258"
        )) |
        (COMMON.NAME == "Pallid Harrier" & SAMPLING.EVENT.IDENTIFIER == "S51111996") |
        (COMMON.NAME == "Crested Goshawk" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S18505248", "S103201363", "S133586111", "S47903089", "S16725097", "S27133439"
        )) |
        (COMMON.NAME == "Gray-headed Fish-Eagle" & SAMPLING.EVENT.IDENTIFIER %in% c(
          "S37879951", "S58025780", "S31343531", "S53087003", "S53694039", "S53695627", 
          "S133774117", "S103686730", "S61596589", "S61596585", "S11088142"
        )) 
    ) %>% 
    distinct(COMMON.NAME, SAMPLING.EVENT.IDENTIFIER)
  

# 3. species-observer combinations ----------------------------------------

  mistake3 <- data %>% 
    filter(
      OBSERVER.ID == "obsr1158032" & COMMON.NAME %in% c(
        "Fulvous Whistling-Duck", "Common Pochard", "Tufted Duck", "Red Spurfowl", 
        "Painted Spurfowl", "Grey Junglefowl", "Rain Quail", "Jungle Bush-Quail", 
        "Lesser Flamingo", "White-rumped Needletail", "Alpine Swift", "Slaty-breasted Rail", 
        "Watercock", "Great Thick-knee", "Black-bellied Plover", "Lesser Sand-Plover", 
        "Greater Sand-Plover", "Ruddy Turnstone", "Common Redshank", "Indian Courser",
        "Little Tern", "Common Tern", "Sandwich Tern", "Lesser Crested Tern", "White Stork",
        "Crested Serpent-Eagle", "Short-toed Snake-Eagle", "Changeable Hawk-Eagle",
        "Black Eagle", "Tawny Eagle", "Eurasian Sparrowhawk"
      ) |
        OBSERVER.ID == "obsr701947" & COMMON.NAME == "Common Tern"
    ) %>% 
    distinct(OBSERVER.ID, COMMON.NAME)
  

# 4. species-admin unit (state or district) combinations ------------------

  mistake4 <- data %>% 
    filter(
      (COMMON.NAME == "Knob-billed Duck" & COUNTY == "Tiruppur") | 
        (COMMON.NAME == "Chestnut-breasted Partridge" & COUNTY == "Darjeeling") |
        (COMMON.NAME == "Snow Partridge" & (
          (STATE == "Jammu and Kashmir" & COUNTY == "Baramulla") |
            STATE == "Ladakh"
        )) |
        (COMMON.NAME == "Himalayan Monal" & COUNTY == "Nainital") |
        (COMMON.NAME == "Red Spurfowl" & COUNTY == "Dharmapuri") |
        (COMMON.NAME == "Red Junglefowl" & COUNTY %in% c(
          "North 24 Parganas", "Purba Bardhaman"
        )) |
        (COMMON.NAME == "Grey Junglefowl" & COUNTY %in% c(
          "Sri Potti Sriramulu Nellore", "East Godavari"
        )) |
        (COMMON.NAME == "Rain Quail" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Jungle Bush-Quail" & (
          COUNTY %in% c("Sri Potti Sriramulu Nellore", "Kurnool", "YSR District (Kadapa)") |
            (STATE == "Uttar Pradesh" & COUNTY %in% c(
              "Rampur", "Lakhimpur Kheri", "Shrawasti", "Lucknow", "Ambedkar Nagar"
            ))
        )) |
        (COMMON.NAME == "Rock Bush-Quail" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Hill Pigeon" & (
          COUNTY %in% c("Bageshwar", "Nainital", "Tehri Garhwal", "Pithoragarh") |
            (STATE == "Jammu and Kashmir" & COUNTY %in% c(
              "Anantnag", "Baramulla", "Budgam", "Doda", "Ganderbal", "Kulgam", "Pulwama", 
              "Shopian", "Srinagar"
            ))
        )) |
        (COMMON.NAME == "Oriental Turtle-Dove" & COUNTY %in% c(
          "Jaisalmer", "Jodhpur", "Barmer", "Bikaner"
        )) |
        (COMMON.NAME == "Spotted Dove" & COUNTY %in% c(
          "Beed", "Hingoli", "Jalna", "Washim", "Solapur", "Latur", "Osmanabad"
        )) |
        (COMMON.NAME == "Barred Cuckoo-Dove" & COUNTY %in% c("Darrang", "Golaghat", "Nagaon")) |
        (COMMON.NAME == "Mountain Imperial-Pigeon" & (
          STATE %in% c("Kerala", "Karnataka", "Tamil Nadu") |
            COUNTY == "Cooch Behar"
        )) |
        (COMMON.NAME == "Greater Coucal" & COUNTY == "Kullu") |
        (COMMON.NAME == "Sirkeer Malkoha" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Grey-bellied Cuckoo" & COUNTY == "Sonitpur") |
        (COMMON.NAME == "Common Hawk-Cuckoo" & COUNTY == "Bageshwar") |
        (COMMON.NAME == "Indian Cuckoo" & COUNTY %in% c(
          "Darbhanga", "Madhubani", "Patna", "Bhagalpur"
        )) |
        (COMMON.NAME == "White-rumped Needletail" & COUNTY == "Chittoor") |
        (COMMON.NAME == "Indian Swiftlet" & COUNTY %in% c("Satara", "Thane", "Ratnagiri")) |
        (COMMON.NAME == "Crested Treeswift" & COUNTY %in% c("Kolhapur", "Nashik")) |
        (COMMON.NAME == "Slaty-breasted Rail" & COUNTY == "Madurai") |
        (COMMON.NAME == "Watercock" & COUNTY == "Karur") |
        (COMMON.NAME == "Slaty-legged Crake" & COUNTY == "Kalimpong") |
        (COMMON.NAME == "Brown Crake" & STATE %in% c("Bihar", "Jharkhand")) |
        (COMMON.NAME == "Sarus Crane" & COUNTY %in% c("Jaisalmer", "Churu")) |
        (COMMON.NAME == "Great Thick-knee" & COUNTY == "Churu") |
        (COMMON.NAME == "Lesser Sand-Plover" & COUNTY %in% c("Jalpaiguri", "Nashik")) |
        (COMMON.NAME == "Bronze-winged Jacana" & COUNTY %in% c(
          "Tiruchirappalli", "Ariyalur", "Karur", "Thiruvarur", "Thanjavur", "Nagapattinam", 
          "Sivagangai", "Pudukkottai", "Madurai", "Ramanathapuram", "Virudhunagar", 
          "Solapur", "Osmanabad", "Nanded", "Aurangabad", "Ahmednagar", "Bikaner", "Churu", 
          "Sikar", "Jodhpur", "Jaisalmer", "Sri Ganganagar", "Gurdaspur", "Kapurthala", 
          "Amritsar", "Ferozepur"
        )) |
        (COMMON.NAME == "Barred Buttonquail" & COUNTY %in% c("Jaisalmer", "Churu")) |
        (COMMON.NAME == "Oriental Pratincole" & COUNTY %in% c("Sonitpur", "Golaghat", "Tinsukia")) |
        (COMMON.NAME == "Common Tern" & COUNTY %in% c("Jalpaiguri", "Madurai", "Tirunelveli")) |
        (COMMON.NAME == "Asian Openbill" & COUNTY == "Jaisalmer") |
        (COMMON.NAME == "Indian Cormorant" & (
          COUNTY %in% c("Jaisalmer", "Barmer", "Bikaner") |
            STATE %in% c("Himachal Pradesh", "Jammu and Kashmir")
        )) |
        (COMMON.NAME == "Red-naped Ibis" & COUNTY %in% c(
          "Hooghly", "Howrah", "Nadia", "Purba Bardhaman", "North 24 Parganas", 
          "South 24 Parganas", "Coimbatore"
        )) |
        (COMMON.NAME == "Crested Serpent-Eagle" & COUNTY %in% c(
          "Barmer", "Bikaner", "Churu", "Jaisalmer", "Solapur", "Parbhani", "Nanded", 
          "Osmanabad", "Beed", "Bhagalpur", "Darbhanga", "Gaya", "Gopalganj", "Katihar", 
          "Nalanda", "Munger", "Saharsa"
        )) |
        (COMMON.NAME == "Changeable Hawk-Eagle" & COUNTY %in% c(
          "Parbhani", "Jaisalmer", "Bikaner", "Jaipur", "Alwar", "Darjeeling", "Kalimpong", "Sikkim"
        )) |
        (COMMON.NAME == "Rufous-bellied Eagle" & COUNTY %in% c(
          "Pithoragarh", "Chamoli", "Rudraprayag", "Uttarkashi"
        )) |
        (COMMON.NAME == "Black Eagle" & COUNTY == "Washim") |
        (COMMON.NAME == "Tawny Eagle" & (
          COUNTY %in% c(
            "Ramanathapuram", "Kurnool", "Parbhani", "Aurangabad", "Jalgaon", "Nashik", 
            "Akola", "Chandrapur", "Nagpur", "East Godavari", "Visakhapatnam", "Bhagalpur"
          ) |
            STATE %in% c("Goa", "Odisha")
        )) |
        (COMMON.NAME == "Montagu’s Harrier" & STATE == "Bihar") |
        (COMMON.NAME == "Crested Goshawk" & (
          COUNTY == "Chittoor" |
            STATE %in% c("Uttarakhand", "Himachal Pradesh")
        )) |
        (COMMON.NAME == "Gray-headed Fish-Eagle" & (
          COUNTY %in% c(
            "Thiruvananthapuram", "Muzaffarnagar", "Tinsukia", "Jalpaiguri", "Alipurduar", 
            "Kalimpong", "Varanasi", "Unnao", "Kanpur Nagar", "Sawai Madhopur"
          ) |
            STATE == "Uttarakhand"
        )) 
    ) %>% 
    distinct(COMMON.NAME, STATE, COUNTY)


# 5. species-admin unit-date combinations ---------------------------------

  mistake5 <- data %>% 
    filter(
      (COMMON.NAME == "Lesser Sand-Plover" & COUNTY == "Malda" & !(month %in% c(3, 4, 10, 11))) 
    ) %>% 
    distinct(COMMON.NAME, STATE, COUNTY, month) 
  

# 6. species-latlong-admin unit (state or district) combinations (or regions) --------

  mistake6 <- data %>% 
    filter(
      (COMMON.NAME == "Red Spurfowl" & (
        TEMP.REGION == "MH_plains" |
          (COUNTY == "Ballari" & LATITUDE > 15.216)
      )) |
        (COMMON.NAME == "Painted Spurfowl" & TEMP.REGION == "MH_plains") |
        (COMMON.NAME == "Grey Junglefowl" & (
          TEMP.REGION == "MH_plains" |
            (COUNTY == "Madurai" & LATITUDE > 10.07 & LONGITUDE > 77.846) |
            (COUNTY == "Tiruchirappalli" & LATITUDE > 11.138)
        )) |
        (COMMON.NAME == "Jungle Bush-Quail" & (
          (TEMP.REGION == "MH_plains" & COUNTY != "Yavatmal") |
            TEMP.REGION %in% c("TN_N-plains", "TN_C-plains", "TN_S-plains")
        )) |
        (COMMON.NAME == "Rock Bush-Quail" & (
          (COUNTY == "Pune" & LONGITUDE < 73.60) |
            (COUNTY == "Satara" & LONGITUDE < 73.85)
        )) |
        (COMMON.NAME == "Red-naped Ibis" & TEMP.REGION %in% c(
          "TN_N-plains", "TN_C-plains", "TN_EG-N"
        )) |
        (COMMON.NAME == "Black Eagle" & COUNTY == "Akola" & LATITUDE < 21.209) 
    ) %>% 
    distinct(COMMON.NAME, STATE, COUNTY, LONGITUDE, LATITUDE, TEMP.REGION)
  

# removing probable mistakes from data ------------------------------------

  filtered <- data %>% 
    anti_join(mistake1) %>% 
    anti_join(mistake2) %>% 
    anti_join(mistake3) %>% 
    anti_join(mistake4) %>% 
    anti_join(mistake5) %>% 
    anti_join(mistake6)
  
  return(filtered)
  
}
