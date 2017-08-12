import qualified SimpleCLP          as S
import qualified System.Environment as SE   ( getArgs )

main :: IO ()
main = do
    cmds <- SE.getArgs
    putStrLn . show $ S.parseCmdLine validOpts cmds

validOpts :: S.ValidOptions
validOpts = so ++ soa ++ lo ++ loa
    where so  = map S.Short    [ 'a', 'b', 'c' ]
          soa = map S.ShortArg [ 'd', 'e', 'f' ]
          lo  = map S.Long     [ "runs", "flies", "swims" ]
          loa = map S.LongArg  [ "mammal", "fish", "bird" ]
