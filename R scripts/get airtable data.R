library(rairtable)
library(data.table)

# Define a helper function: take a vector of record IDs and return a comma-separated string
#Lookup dt must have columns "airtable_record_id", "name"
resolve_linked <- function(record_ids, lookup_dt) {
  if (is.null(record_ids) || length(record_ids) == 0) return(NA_character_)
  state_names <- lookup_dt[airtable_record_id %in% record_ids, name]
  if (length(state_names) == 0) return(NA_character_)
  return(paste(unique(state_names), collapse = ","))
}

#Airtable reading
token <- 'pat7JpuJa2gvOmUJJ.d8770b99cf65993fb14e0481c286d49d160c94f4646e410e9459a9965f9082c1'
set_airtable_api_key(token)
baseid <- 'appmZrocL69psc6h8'

#Relevant airtable tables
statet <- airtable("States",baseid)
hubt <- airtable("Hubs",baseid)
campt <- airtable("Campaigns",baseid)
c4t <- airtable("C4 Organizations",baseid)
orgt <- airtable("Organizations",baseid)

#Lookup table for state record ids
states <- read_airtable(statet)
setDT(states)
states<-states[,c('airtable_record_id','State')]
names(states)[2]<-'name'

#Lookup table for org record ids
orgs <- read_airtable(orgt, fields = 'Organization')
setDT(orgs)
names(orgs)[2]<-c('name')