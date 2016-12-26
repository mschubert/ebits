sample_rows = function(df, n) {
    if (n < 1)
        n = round(nrow(df) * n)

    df[sample(seq_len(nrow(df)), n),]
}
