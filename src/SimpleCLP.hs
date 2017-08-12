module SimpleCLP
    ( parseCmdLine
    , chkOpt
    , optUsed
    , Option (..)
    , Options (..)
    , ValidOptions (..)
    , OptsArgs (..)
    , ParseError (..)
    ) where

import Control.Monad.Trans.State ( StateT, StateT (..), execStateT, get )
import Data.List ( foldl', delete )

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
      MissingShortOption Char
    | MissingShortArgument Option
    | MissingLongOption String
    | MissingLongArgument Option
    deriving ( Show, Eq )

data OptResult =
      NotUsed
    | Used
    | UsedArg String
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

---------------------------------------------------------------------
-- Exported functions

parseCmdLine :: ValidOptions -> [String] -> Either ParseError OptsArgs
parseCmdLine vopts cmds =
    case execStateT parseM $ initParser vopts cmds of
         Left pError -> Left pError
         Right pSt   -> Right ( collect . pOpts $ pSt, pArgs pSt )

chkOpt :: Options -> Option -> OptResult
chkOpt opts opt = case lookup opt opts of
                       Nothing  -> NotUsed
                       Just ""  -> Used
                       Just arg -> UsedArg arg

optUsed :: Options -> Option -> Bool
optUsed opts opt
    | chkOpt opts opt == NotUsed = False
    | otherwise                  = True

---------------------------------------------------------------------
-- Private functions

--------------------------------------
-- Initialization and finalization

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

collect :: Options -> Options
collect [] = []
collect ((o,a):oas) = case chkOpt oas o of
                           NotUsed   -> ( o, a ):( collect oas )
                           Used      -> collect oas
                           UsedArg b -> let reduced = delete (o, b) oas
                                            augOpt  = (o, a ++ " " ++ b)
                                        in collect $ augOpt:reduced

---------------------------------------
-- Main parsing monad: General functions

parseM :: StateT ParserSt ( Either ParseError ) ()
parseM = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing           -> return ()
         Just "--"         -> makeAllArgs
         Just "-"          -> addArg "-"      >> parseM
         Just ('-':'-':cs) -> parseLong cs    >> parseM
         Just ('-':cs)     -> parseCluster cs >> parseM
         Just cs           -> addArg cs       >> parseM

popCmd :: StateT ParserSt ( Either ParseError ) ( Maybe String )
popCmd = StateT $ \ pSt -> case unparsed pSt of
                                []     -> Right ( Nothing, pSt )
                                (x:xs) -> Right ( Just x
                                                , pSt { unparsed = xs } )

addOpt :: ( Option, String ) -> StateT ParserSt ( Either ParseError ) ()
addOpt x = StateT $ \ pSt -> let xs = pOpts pSt
                             in  Right $ ( (), pSt { pOpts = xs ++ [x] } )

cantParse :: ParseError -> StateT ParserSt ( Either ParseError ) ()
cantParse e = StateT $ \ pSt -> Left e

---------------------------------------
-- Parsing arguments

makeAllArgs :: StateT ParserSt ( Either ParseError ) ()
makeAllArgs = do
    nxtArg <- popCmd
    case nxtArg of
         Nothing -> return ()
         Just x  -> addArg x >> makeAllArgs

addArg :: String -> StateT ParserSt ( Either ParseError ) ()
addArg cmd = StateT $ \ pSt ->
    let args = pArgs pSt
    in  Right $ ( () , pSt { pArgs = args ++ [ cmd ] } )

---------------------------------------
-- Parsing short options

parseCluster :: String -> StateT ParserSt ( Either ParseError ) ()
parseCluster []     = return ()
parseCluster (c:[]) = parseShort c
parseCluster (c:cs) = do
    vOpts <- fmap validShort get
    case lookup c vOpts of
         Just a@( ShortArg _ ) -> addOpt ( a, cs )
         otherwise             -> do parseShort c
                                     parseCluster cs

parseShort :: Char -> StateT ParserSt ( Either ParseError ) ()
parseShort c = do
    vOpts <- fmap validShort get
    case lookup c vOpts of
         Nothing                 -> cantParse $ MissingShortOption c
         Just opt@( Short _ )    -> addOpt ( opt, "" )
         Just opt@( ShortArg _ ) -> addShortOptArg opt

addShortOptArg :: Option -> StateT ParserSt ( Either ParseError ) ()
addShortOptArg opt = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing       -> cantParse $ MissingShortArgument opt
         Just ('-':cs) -> cantParse $ MissingShortArgument opt
         Just cs       -> addOpt ( opt, cs )

---------------------------------------
-- Parsing long options

parseLong :: String -> StateT ParserSt ( Either ParseError ) ()
parseLong cmd = do
    vOpts <- fmap validLong get
    let s = fst . splitLong $ cmd
    case lookup s vOpts of
         Nothing                -> cantParse $ MissingLongOption s
         Just opt@( Long _ )    -> addOpt ( opt, "" )
         Just opt@( LongArg _ ) -> do let arg = snd . splitLong $ cmd
                                      if null arg
                                         then cantParse $ MissingLongArgument opt
                                         else addOpt ( opt, arg )

splitLong :: String -> (String, String)
splitLong s = (opt, arg)
    where opt = takeWhile ( /= '=' ) s
          arg = case dropWhile ( /= '=' ) s of
                     '=':ss    -> ss
                     otherwise -> []
