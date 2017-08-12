import qualified SimpleCLP          as S
import qualified System.Environment as SE   ( getArgs )

main :: IO ()
main = do
    cmds <- SE.getArgs
    case S.parseCmdLine validOpts cmds of
         Left  e -> putStrLn . show $ e
         Right x -> prettyOptsArgs x

prettyOptsArgs :: S.OptsArgs -> IO ()
prettyOptsArgs (opts, args) = do
    mapM_ ( \ (o, a) -> putStrLn $ show o ++ " : " ++ show a ) opts
    mapM_ ( putStrLn . show ) args

validOpts :: S.ValidOptions
validOpts = so ++ soa ++ lo ++ loa
    where so  = map S.Short    [ 'a', 'b', 'c' ]
          soa = map S.ShortArg [ 'd', 'e', 'f' ]
          lo  = map S.Long     [ "runs", "flies", "swims" ]
          loa = map S.LongArg  [ "mammal", "fish", "bird" ]
