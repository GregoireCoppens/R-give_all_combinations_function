give_all_combinations <- function(x){  
  def_list <- list()
  final_list <- list()
  def_Counter <- 1
  for(i in 1:length(x)){
    def_list[[length(def_list)+1]] <- list(x[i],i)
    final_list[[length(final_list)+1]] <- def_list[[length(def_list)]][[1]]
    while(length(def_list[[length(def_list)]][[1]])<length(x[i:length(x)])){
      for(j in i+1:(length(x[i:length(x)])-1)){
        if(def_list[[def_Counter]][[2]]<j){
          def_list[[length(def_list)+1]] <- list(c(def_list[[def_Counter]][[1]],x[j]),j)
          final_list[[length(final_list)+1]] <- def_list[[length(def_list)]][[1]]
        }
      }
      def_Counter <- def_Counter+1
    }
  }
  return(final_list)
}

y <- give_all_combinations(c("a","b","c","d"))
print.table(y)
