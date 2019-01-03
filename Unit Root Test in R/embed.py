def embed(x, dimension = 1):
    x = np.array(x)
    n = np.size(x)
    trycatch_embed1(dimension)   # if dimension < 1
    trycatch_embed2(dimension, n)   # if dimension > n

    m = n - dimension + 1
    nrows = n - dimension + 1
    ncols = dimension
    res = np.zeros((nrows, ncols))
    print(res)
    
    for i in range(0, m):
        rangeList = 1
        if dimension > 1:
            rangeList = list(range(i,(i+dimension)))
            rangeList = sorted(rangeList, reverse=True)
        res[i,:] = x[rangeList]
    
    return res
