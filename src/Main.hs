import qualified SimpleCLP          as S
import qualified System.Environment as SE   ( getArgs )

main :: IO ()
main = do
    cmds <- SE.getArgs
    let etOpts = S.parseCmdLine validOpts cmds
    case S.parseCmdLine validOpts cmds of
         Left  e -> putStrLn . show $ e
         Right x -> doController x

doController :: S.OptsArgs -> IO ()
doController (opts, args) = do
    mapM_ ( prettyOpt opts ) validOpts
    mapM_ ( \ x -> putStrLn $ "argument: " ++ x ) args
    putStrLn . show $ opts

prettyOpt :: S.Options -> S.Option -> IO ()
prettyOpt opts opt = do
    let check = show $ S.chkOpt opts opt
        used  = show $ S.optUsed opts opt
        disp  = show opt
    putStrLn $ "option: " ++ disp ++ " | " ++ check ++ " | " ++ used

validOpts :: S.ValidOptions
validOpts = so ++ soa ++ lo ++ loa
    where so  = map S.Short    [ 'a', 'b', 'c' ]
          soa = map S.ShortArg [ 'x', 'y', 'z' ]
          lo  = map S.Long     [ "runs", "flies", "swims" ]
          loa = map S.LongArg  [ "mammal", "fish", "bird" ]
