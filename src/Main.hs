import qualified SimpleCLP          as SP

validOpts :: SP.ValidOptions
-- ^Arbitrary collection of the four types of options for testing.
validOpts = so ++ soa ++ lo ++ loa
    where so  = map SP.Short    [ 'a', 'b', 'c' ]
          soa = map SP.ShortArg [ 'x', 'y', 'z' ]
          lo  = map SP.Long     [ "rock", "paper", "scissors" ]
          loa = map SP.LongArg  [ "animal", "vegetable", "mineral" ]

main :: IO ()
-- ^All this does is take the specified command line options and
-- arguments and prints them back in parsed form along with the
-- evaluations of the SP.chkOpt and SP.optUsed convenience functions
-- for all the valid options defined above.
main = do
    etParsedCmds <- SP.getParsedArgs validOpts
    case etParsedCmds of
         Left  e -> putStrLn . show $ e
         Right x -> doController x

doController :: SP.OptsArgs -> IO ()
doController (opts, args) = do
    mapM_ ( prettyOpt opts ) validOpts
    mapM_ ( \ x -> putStrLn $ "argument: " ++ x ) args

prettyOpt :: SP.Options -> SP.Option -> IO ()
prettyOpt opts opt = do
    let check = show $ SP.chkOpt opts opt
        used  = show $ SP.optUsed opts opt
        disp  = show opt
    putStrLn $ "option: " ++ disp ++ " | " ++ check ++ " | " ++ used
