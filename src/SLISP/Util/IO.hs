module SLISP.Util.IO(safeLine) where
  
safeLine    ::  String -> IO (String)
safeLine s  =   catch getLine (\_ -> return s)