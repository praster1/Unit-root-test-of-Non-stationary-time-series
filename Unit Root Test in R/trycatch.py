### type 인자 예외처리
def trycatch_type(type):
	type_allowed = ["none", "drift", "trend"]
	try:
		if type not in type_allowed:
			raise ValueError
	except ValueError:
		 print('type 인자에 잘못된 값을 넣었습니다!')

		
### select 인자 예외처리
def trycatch_selectlags(selectlags):
	selectlags_allowed = ["Fixed", "AIC", "BIC"]
	try:
		if selectlags not in selectlags_allowed:
			raise ValueError
	except ValueError:
		 print('selectlags 인자에 잘못된 값을 넣었습니다!')
			
		
### y is not a vector or univariate time series.
def trycatch_univariate(y):
	y = np.array(y)
	try:
		if (np.ndim(y) > 1):
			raise ValueError
	except ValueError:
		 print('y is not a vector or univariate time series.')

		
### NAs in y.
def trycatch_NA(y):
	try:
		if (np.any(np.equal(y, None))):
			raise ValueError
	except ValueError:
		 print('NAs in y.')
		
		
### Lags must be set to an non negative integer value.
def trycatch_lag(lag):
	try:
		if (lag < 0):
			raise ValueError
	except ValueError:
		 print('Lags must be set to an non negative integer value.')
