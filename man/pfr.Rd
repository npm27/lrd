% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pfr.R
\name{pfr}
\alias{pfr}
\title{Probability of First Recall}
\usage{
pfr(data, position, answer, id, key, scored, group.by = NULL)
}
\arguments{
\item{data}{a dataframe of the scored free recall that you would
like to calculate - use prop_correct_free() for best formatting.}

\item{position}{a column name in the dataframe that contains
answered position of each response in quotes (i.e., "column")}

\item{answer}{a column name of the answer given for that position
in the original dataframe.}

\item{id}{a column name of the participant id in the original
dataframe.}

\item{key}{a vector containing the scoring key or data column name.
This column does not have to be included in the original dataframe.
We assume your answer key is in the tested position order. You should
not include duplicates in your answer key.}

\item{scored}{a column in the original dataframe indicating if the
participant got the answer correct (1) or incorrect (0).}

\item{group.by}{an optional argument that can be used to group the
output by condition columns. These columns should be in the original
dataframe and concatenated c() if there are multiple columns}
}
\value{
\item{DF_PFR}{A dataframe of the probability of first response
for each position including group by variables if indicated.}
}
\description{
This function calculates the probability of first recall
for each serial position. The total number of times an
item was recalled first is divided by the total number of
first recalls (i.e., the number of participants who wrote
anything down!).
}
\details{
This output can then be used to create a PFR visualizations,
and an example can be found in our manuscript/vignettes.

Important: The code is written assuming the data provided are for
a single recall list. If repeated measures are used (i.e., there are
multiple lists completed by each participant or multiple list versions),
you should use this function several times, once on each list/answer key.
}
\examples{

data(free_data)
data(answer_key_free2)

free_data <- subset(free_data,
 List_Type == "Cat_Recall_L1")

DF_long <- arrange_data(data = free_data,
 responses = "Response",
 sep = " ",
 id = "Username")

scored_output <- prop_correct_free(data = DF_long,
 responses = "response",
 key = answer_key_free2$Answer_Key,
 id = "Sub.ID",
 cutoff = 1,
 flag = TRUE,
 group.by = "Version")

pfr_output <- pfr(data = scored_output$DF_Scored,
 position = "position",
 answer = "Answer",
 id = "Sub.ID",
 key = answer_key_free2$Answer_Key,
 scored = "Scored",
 group.by = "Version")

 head(pfr_output)

}
\keyword{correct}
\keyword{free}
\keyword{position}
\keyword{proportion}
\keyword{recall}
\keyword{scoring}
\keyword{serial}
