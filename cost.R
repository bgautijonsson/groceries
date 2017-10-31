cost <- function(pred, test) {
    
    cost = (log(pred$unit_sales+1) - log(test$unit_sales+1)) * test$perishable
    cost = sum(cost)
    cost = cost/sum(test$perishable)
    cost = sqrt(cost)
    
}