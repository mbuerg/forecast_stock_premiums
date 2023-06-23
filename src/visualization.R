# visualization
# plotte die ersten 10


permutation_df_sorted <- permutation_df[rev(order(permutation_df$vi_values)), ]


ggplot(data = permutation_df_sorted[1:10, ], aes(x =reorder(vi_rows, vi_values), 
                                                 y=vi_values)) +
  geom_col() +
  coord_flip() 
#rotate_y_text(45)