X <- matrix(runif(30*20), nrow=30, ncol=20)

# 2D
objLEM_2D <- graphMatrix(X, algorithm="LEM", ndim=2)

expect_equal(length(objLEM_2D), 3)
expect_true("dgCMatrix" %in% is(objLEM_2D$M))
expect_true(is.character(objLEM_2D$algorithm))
expect_true(is.numeric(objLEM_2D$ndim))

outLEM_2D <- embedding(objLEM_2D)

expect_true(is.matrix(outLEM_2D))
expect_equal(nrow(outLEM_2D), nrow(X))
expect_equal(ncol(outLEM_2D), 2)

# 3D
objLEM_3D <- graphMatrix(X, algorithm="LEM", ndim=3)

expect_equal(length(objLEM_3D), 3)
expect_true("dgCMatrix" %in% is(objLEM_3D$M))
expect_true(is.character(objLEM_3D$algorithm))
expect_true(is.numeric(objLEM_3D$ndim))

outLEM_3D <- embedding(objLEM_3D)

expect_true(is.matrix(outLEM_3D))
expect_equal(nrow(outLEM_3D), nrow(X))
expect_equal(ncol(outLEM_3D), 3)
