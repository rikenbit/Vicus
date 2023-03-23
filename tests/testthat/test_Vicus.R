X <- matrix(runif(30*20), nrow=30, ncol=20)

# 2D
objVicus_2D <- graphMatrix(X, algorithm="Vicus", ndim=2)

expect_equal(length(objVicus_2D), 3)
expect_true("dgCMatrix" %in% is(objVicus_2D$M))
expect_true(is.character(objVicus_2D$algorithm))
expect_true(is.numeric(objVicus_2D$ndim))

outVicus_2D <- embedding(objVicus_2D)

expect_true(is.matrix(outVicus_2D))
expect_equal(nrow(outVicus_2D), nrow(X))
expect_equal(ncol(outVicus_2D), 2)

# 3D
objVicus_3D <- graphMatrix(X, algorithm="Vicus", ndim=3)

expect_equal(length(objVicus_3D), 3)
expect_true("dgCMatrix" %in% is(objVicus_3D$M))
expect_true(is.character(objVicus_3D$algorithm))
expect_true(is.numeric(objVicus_3D$ndim))

outVicus_3D <- embedding(objVicus_3D)

expect_true(is.matrix(outVicus_3D))
expect_equal(nrow(outVicus_3D), nrow(X))
expect_equal(ncol(outVicus_3D), 3)
