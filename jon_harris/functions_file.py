# Determine Outliers of Continuous Data
def outlier_idx(data, thresh):
	
	avg = data.mean()  # calc average
	stdev = data.std()  # calc standard deviation
	z_score = (data - avg) / stdev  # calc z_score
	ol = z_score > thresh  # boolean (True=outlier)
	outlier_idx = data.index[ol]  # indexes of outliers

	return list(outlier_idx)

#Categorize Features By DataType
def categorize(df):
	# Input: dataframe
	# Return: column names of categorical, continuous numerical, and discreet numerical features

	num_cont_features = []
	num_disc_features = []

	# Separate predictors by numerical and categorical features
	num_features = df.select_dtypes(include=["int64", "float64"]).columns
	cat_features = df.select_dtypes(include=["object"]).columns

	# Remove SalePrice and SalePrice_log from feature list
	num_features = num_features[0:-2]

	# If more than 15 unique values, consider continuous
	for colm in num_features:
		if df[colm].nunique() > 15:
			num_cont_features.append(colm)
		else:
			num_disc_features.append(colm)

	return cat_features, num_cont_features, num_disc_features



# Function to Quickly Summarize NA, Zero, and Correlation statistics
def dirty_summary(features):
    store = [["Predictor", "n_NA", "perc_NA", "n_zero", "perc_zero", "corr"]]

    for idx, feature in enumerate(features):
        data = house_data[[feature, "SalePrice_log"]]  # create df subset
        n_missing = data[feature].isna().sum()  # calc miss values
        perc_missing = n_missing / data[feature].shape[0] * 100  # calc %miss values

        new_data = data.dropna()  # remove missing data
        n_zero = sum(new_data[feature] == 0)  # calc num of zeros
        perc_zero = n_zero / new_data.shape[0] * 100  # calc %zeros
        corr_dropna = stats.pearsonr(
            new_data[feature].values, new_data["SalePrice_log"].values
        )[0]

        store.append(
            [
                feature,
                n_missing,
                round(perc_missing, 1),
                n_zero,
                round(perc_zero, 1),
                round(corr_dropna, 2),
            ]
        )

    analyses = pd.DataFrame(store[1:], columns=store[0])
    return analyses