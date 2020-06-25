# lgrExtra 0.0.1.9010

* `AppenderDbi`: The default setting for buffer_size has been changed to `0`. 
  This means every log event is written directly to the target database. If you
  want performance improvements, set the buffer 
