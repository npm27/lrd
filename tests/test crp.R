 data(free_data)
 data(answer_key_free2)

 free_data <- subset(free_data,
  List_Type == "Cat_Recall_L1")

 DF_long <- arrange_data(data = free_data,
  responses = "Response",
  sep = " ",
  id = "Username")

 scored_output <- prop_correct_free( data = DF_long,
  responses = "response",
  key = answer_key_free2$Answer_Key,
  id = "Sub.ID",
  cutoff = 1,
  flag = TRUE,
  group.by = "Version")

 crp_output <- crp(data = scored_output$DF_Scored,
  position = "position",
  answer = "Answer",
  id = "Sub.ID",
  key = answer_key_free2$Answer_Key,
  scored = "Scored")

  head(crp_output)

