
split_pills_with_gaps <- function(df, cols_per_pill = 5, sep_cols = 2) {
  total_cols <- ncol(df)
  block_size <- cols_per_pill + sep_cols
  n_pills <- floor((total_cols + sep_cols) / block_size)
  
  pills <- vector("list", n_pills)
  standard_names <- c("Sample", "Date", "Time", "Temperature", "Status")
  
  for (i in seq_len(n_pills)) {
    start_col <- (i - 1) * block_size + 1
    end_col <- start_col + cols_per_pill - 1
    pill_df <- df[, start_col:end_col, drop = FALSE]
    colnames(pill_df) <- standard_names
    pill_df$Pill <- paste0("Pill ", i)
    pills[[i]] <- pill_df
  }
  
  names(pills) <- paste0("Pill ", seq_len(n_pills))
  return(pills)
}