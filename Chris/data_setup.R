# data_setup.R partitions the data into k-index_folds which can be used
# with any of the various models we might want to try out with the
# data.  The goal of this is to be able to run this, and to initiate
# the data into the R-workspace so models can be trained and tested
# using the data.

# Load Data
mushrooms=read.csv("../Data/agaricus-lepiota.data", header=TRUE, sep=",")


# Specify Some Variables
n_folds <- 10
index_folds <- list()
folds <- list()

n_entries_per_fold <- floor(nrow(mushrooms)/(n_folds))

# Generate the indices we will use to segment the data.
all_indices <- seq_len(nrow(mushrooms))
while( length(all_indices) > n_entries_per_fold)
{
  temp <- sample(all_indices, size = n_entries_per_fold)
  all_indices <- setdiff(all_indices, temp)
  index_folds <- c(index_folds, list(temp))
}

	## At this point there are a few indices that were not used.  These
	## indices are added to the index_folds vectors starting at vector 1.
i = 1
while( length(all_indices) > 0 )
{
  index_folds[[i]] <- append( index_folds[[i]], all_indices[1])
  all_indices <- setdiff(all_indices, all_indices[1])
  i = i+1
  if( i > 10 ) i = 1
}

	## Now we have a list of vectors such that all the vectors contain
	## all of indices of the data set, all the vectors are
	## mutually disjoint (no repeates among them), and all of them are
	## randomly selected.  Now, using these sets of indices, we will
	## subset the data into ten subsets.

  folds <- list(data.frame(mushrooms[index_folds[[1]], ]), data.frame(mushrooms[index_folds[[1]], ]),
								data.frame(mushrooms[index_folds[[1]], ]), data.frame(mushrooms[index_folds[[1]], ]),
								data.frame(mushrooms[index_folds[[1]], ]), data.frame(mushrooms[index_folds[[1]], ]),
								data.frame(mushrooms[index_folds[[1]], ]), data.frame(mushrooms[index_folds[[1]], ]),
								data.frame(mushrooms[index_folds[[1]], ]))


# Now the i'th fold can be accessed as a list item by: folds[[i]]
# categories can be accessed by: folds[[i]]$category_name
