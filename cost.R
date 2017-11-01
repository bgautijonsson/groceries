cost <- function(pred, test) {
    
    test[test$unit_sales < 0,]$unit_sales <- 0
    cost = (log(pred+1) - log(test$unit_sales+1)) 
    cost = cost^2 * test$perishable
    cost = sum(cost)
    cost = cost/sum(test$perishable)
    cost = sqrt(cost)
    cost
}