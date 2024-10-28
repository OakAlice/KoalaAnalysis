# Scripts relevant for ensuring orientation invarianc
# The SVD-based Orientation-Invariant Transformation (OIT) 

accel_data <- matrix(rnorm(100), nrow = 10, ncol = 10)  # 10x10 matrix as an example

# Apply SVD
svd_result <- svd(accel_data)

# Extract the U, D, and V matrices
U <- svd_result$u  # Left singular vectors
D <- diag(svd_result$d)  # Singular values (as a diagonal matrix)
V <- svd_result$v  # Right singular vectors

# Reconstruct the original matrix (for validation)
reconstructed_accel_data <- U %*% D %*% t(V)

# Print the original and reconstructed matrices (for validation)
print("Original Matrix:")
print(accel_data)
print("Reconstructed Matrix:")
print(reconstructed_accel_data)