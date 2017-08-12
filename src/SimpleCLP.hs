module SimpleCLP
    ( parseCmdLine
    , Option (..)
    , ValidOptions (..)
    , OptsArgs (..)
    , ParseError (..)
    ) where

import Control.Monad.Trans.State ( StateT, StateT (..), execStateT, get )
import Data.List ( foldl' )

---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------

---------------------------------------
-- Exported types

data Option =
      Short    Char
    | ShortArg Char
    | Long     String
    | LongArg  String
    deriving ( Show, Eq )

data ParseError =
      NoParseError
    | RepeatedOptions
    | MissingShortOption
    | MissingShortArgument
    | MissingLongOption
    | MissingLongArgument
    | ShortArgInCluster
    deriving ( Show, Eq )

type ValidOptions = [ Option ]

type Options = [ (Option, String) ]

type Arguments = [ String ]

type OptsArgs = (Options, Arguments)

---------------------------------------
-- Private types

type ShortDict = [ ( Char, Option ) ]

type LongDict = [ ( String, Option ) ]

data ParserSt = ParserSt { validShort :: ShortDict
                         , validLong  :: LongDict
                         , unparsed   :: [String]
                         , pOpts      :: Options
                         , pArgs      :: Arguments
                         } deriving ( Show )

---------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------

parseCmdLine :: ValidOptions -> [String] -> Either ParseError OptsArgs
parseCmdLine vopts cmds =
    case execStateT parseM $ initParser vopts cmds of
         Left pError -> Left pError
         Right pSt   -> Right ( pOpts pSt, pArgs pSt )

initParser :: ValidOptions -> [String] -> ParserSt
initParser vopts cmds = let valid = foldl' getValid ( [], [] ) vopts
                        in  ParserSt { validShort = fst valid
                                     , validLong  = snd valid
                                     , unparsed   = cmds
                                     , pOpts      = []
                                     , pArgs      = [] }

getValid :: ( ShortDict, LongDict ) -> Option -> ( ShortDict, LongDict )
getValid (s,l) ( Short x )    = let s' = s ++ [ ( x, Short x ) ]    in ( s', l )
getValid (s,l) ( ShortArg x ) = let s' = s ++ [ ( x, ShortArg x ) ] in ( s', l )
getValid (s,l) ( Long x )     = let l' = l ++ [ ( x, Long x ) ]     in ( s, l' )
getValid (s,l) ( LongArg x )  = let l' = l ++ [ ( x, LongArg x) ]   in ( s, l' )

parseM :: StateT ParserSt ( Either ParseError ) ()
parseM = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing           -> return ()
         Just ('-':'-':cs) -> parseLong cs >> parseM
         Just ('-':cs)     -> parseCluster cs >> parseM
         Just cs           -> addArg cs >> parseM

parseCluster :: String -> StateT ParserSt ( Either ParseError ) ()
parseCluster []     = return ()
parseCluster (c:[]) = parseShort c
parseCluster (c:cs) = do
    vOpts <- fmap validShort get
    case lookup c vOpts of
         Just ( ShortArg _ ) -> cantParse ShortArgInCluster
         otherwise           -> parseShort c
    parseCluster cs

parseShort :: Char -> StateT ParserSt ( Either ParseError ) ()
parseShort c = do
    vOpts <- fmap validShort get
    case lookup c vOpts of
         Nothing                 -> cantParse MissingShortOption
         Just opt@( Short _ )    -> addOpt ( opt, "" )
         Just opt@( ShortArg _ ) -> addShortOptArg opt

addShortOptArg :: Option -> StateT ParserSt ( Either ParseError ) ()
addShortOptArg opt = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing       -> cantParse MissingShortArgument
         Just ('-':cs) -> cantParse MissingShortArgument
         Just cs       -> addOpt ( opt, cs )

addArg :: String -> StateT ParserSt ( Either ParseError ) ()
addArg cmd = StateT $ \ pSt ->
    let args = pArgs pSt
    in  Right $ ( () , pSt { pArgs = args ++ [ cmd ] } )

parseLong :: String -> StateT ParserSt ( Either ParseError ) ()
parseLong cmd = do
    vOpts <- fmap validLong get
    case lookup ( fst . splitLong $ cmd ) vOpts of
         Nothing                -> cantParse MissingLongOption
         Just opt@( Long _ )    -> addOpt ( opt, "" )
         Just opt@( LongArg _ ) -> do let arg = snd . splitLong $ cmd
                                      if null arg
                                         then cantParse MissingLongArgument
                                         else addOpt ( opt, arg )

addOpt :: ( Option, String ) -> StateT ParserSt ( Either ParseError ) ()
addOpt x = StateT $ \ pSt -> let xs = pOpts pSt
                             in  Right $ ( (), pSt { pOpts = xs ++ [x] } )

cantParse :: ParseError -> StateT ParserSt ( Either ParseError ) ()
cantParse e = StateT $ \ pSt -> Left e

splitLong :: String -> (String, String)
splitLong s = (opt, arg)
    where opt = takeWhile ( /= '=' ) s
          arg = case dropWhile ( /= '=' ) s of
                     '=':ss    -> ss
                     otherwise -> []

popCmd :: StateT ParserSt ( Either ParseError ) ( Maybe String )
popCmd = StateT $ \ pSt -> case unparsed pSt of
                                []     -> Right ( Nothing, pSt )
                                (x:xs) -> Right ( Just x
                                                , pSt { unparsed = xs } )
