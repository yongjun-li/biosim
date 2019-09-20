#' @title amatrix Function
#'
#' @description This function allows you to calculate A matrix, the numerical relationship matrix.
#' @param ped a data frame of pedigree with three columns: ID, SIRE, DAM
#' @keywords amatrix
#' @export
#' @examples
#' amatrix()

amatrix <- function(ped) {

    N = nrow(ped)
    A <- as.data.frame(matrix(0,N,N))
    for (i in 1:N){
        for (j in i:N){
            s = ped[j,2]
            d = ped[j,3]
            if (s==0 & d==0) {
                if (i==j) {
                    A[i,j] = 1.0
                } else {
                    A[i,j] = 0.0
                    A[j,i] = 0.0
                }
            } else if (s==0 & d!=0) {
                if (i==j) {
                    A[i,j] = 1.0
                } else {
                    A[i,j] = 0.5*A[i,d]
                    A[j,i] = 0.5*A[i,d]
                }
            } else if (s!=0 & d==0) {
                if (i==j) {
                    A[i,j] = 1.0
                } else {
                    A[i,j] = 0.5*A[i,s]
                    A[j,i] = 0.5*A[i,s]
                }
            } else {
                if (i==j) {
                    A[i,j]=1.0+0.5*A[s,d]
                } else {
                    A[i,j]=0.5*(A[i,s]+A[i,d])
                    A[j,i]=A[i,j]
                }
            }
        }
    }

    return (A)

}
