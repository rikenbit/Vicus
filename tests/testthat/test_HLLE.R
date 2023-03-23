X <- matrix(runif(30*20), nrow=30, ncol=20)

# 2D
objHLLE_2D <- graphMatrix(X, algorithm="HLLE", ndim=2)

expect_equal(length(objHLLE_2D), 3)
expect_true("dgCMatrix" %in% is(objHLLE_2D$M))
expect_true(is.character(objHLLE_2D$algorithm))
expect_true(is.numeric(objHLLE_2D$ndim))

outHLLE_2D <- embedding(objHLLE_2D)

expect_true(is.matrix(outHLLE_2D))
expect_equal(nrow(outHLLE_2D), nrow(X))
expect_equal(ncol(outHLLE_2D), 2)

# 3D
objHLLE_3D <- graphMatrix(X, algorithm="HLLE", ndim=3)

expect_equal(length(objHLLE_3D), 3)
expect_true("dgCMatrix" %in% is(objHLLE_3D$M))
expect_true(is.character(objHLLE_3D$algorithm))
expect_true(is.numeric(objHLLE_3D$ndim))

outHLLE_3D <- embedding(objHLLE_3D)

expect_true(is.matrix(outHLLE_3D))
expect_equal(nrow(outHLLE_3D), nrow(X))
expect_equal(ncol(outHLLE_3D), 3)
