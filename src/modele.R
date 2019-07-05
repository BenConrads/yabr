dropout_model <- function(X,y, drop_in, drop_out, lay1, lay2, lay3, decay, momentum, epochs=30, verb=0) {
  colnum <- ncol(X)
  k_clear_session()
  model <- keras_model_sequential()
  model %>%
    layer_dense(units=lay1, input_shape = c(colnum)) %>%
    layer_dropout(rate=drop_in) %>%
    layer_dense(units=lay2, activation='relu') %>%
    layer_dropout(rate=drop_in) %>%
    layer_dense(units=lay3, activation='relu') %>%
    layer_dropout(rate=drop_out) %>%
    layer_dense(units=1, activation = 'sigmoid')
  opt <- optimizer_sgd(decay = decay, momentum = momentum)
  model %>%
    compile(
      loss='binary_crossentropy',
      optimizer=opt,
      metrics=c('accuracy')
    )
  model %>%
    fit(X, y,
        batch_size=16,
        validation_split=0.2,
        verbose=verb,
        epochs=epochs
    )
  return(model)
}

lstm_model <- function(X,y, drop_in, drop_out, lay1, lay2, lay3, decay, momentum, epochs=30, verb=0) {
  colnum <- ncol(X)
  k_clear_session()
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units=lay1, input_shape = c(colnum, 1), return_sequences = T) %>%
    layer_dropout(rate=drop_in) %>%
    layer_lstm(units=lay2, activation='tanh', return_sequences = T) %>%
    layer_dropout(rate=drop_in) %>%
    layer_lstm(units=lay3, activation='tanh', return_sequences = T) %>%
    layer_dropout(rate=drop_out) %>%
    layer_dense(units=1, activation = 'sigmoid')
  opt <- optimizer_sgd(decay = decay, momentum = momentum)
  model %>%
    compile(
      loss='binary_crossentropy',
      optimizer=opt,
      metrics=c('accuracy')
    )
  model %>%
    fit(X, y,
        batch_size=16,
        validation_split=0.2,
        verbose=verb,
        epochs=epochs
    )
  return(model)
}
