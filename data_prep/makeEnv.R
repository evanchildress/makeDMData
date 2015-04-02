env<-data.table(dbGetQuery(link$conn,"SELECT * FROM data_environmental_with_zst_zsd;"))
setkey(env,date_ct)
plot(zsd~date_ct,data=env,type='l')
plot(typical_temperature~date_ct,data=env)
