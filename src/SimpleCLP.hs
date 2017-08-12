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
      MissingShortOption Char
    | MissingShortArgument Option
    | ShortArgInCluster Option
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
         Right pSt   -> Right ( pOpts pSt, pArgs pSt )

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
-- Initialization

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

---------------------------------------
-- Parsing monad

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
         Just a@( ShortArg _ ) -> cantParse $ ShortArgInCluster a
         otherwise             -> parseShort c
    parseCluster cs

parseShort :: Char -> StateT ParserSt ( Either ParseError ) ()
parseShort c = do
    vOpts <- fmap validShort get
    case lookup c vOpts of
         Nothing                 -> cantParse $ MissingShortOption c
         Just opt@( Short _ )    -> addOpt ( opt, "" )
         Just opt@( ShortArg _ ) -> addShortOptArg opt

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

popCmd :: StateT ParserSt ( Either ParseError ) ( Maybe String )
popCmd = StateT $ \ pSt -> case unparsed pSt of
                                []     -> Right ( Nothing, pSt )
                                (x:xs) -> Right ( Just x
                                                , pSt { unparsed = xs } )

addArg :: String -> StateT ParserSt ( Either ParseError ) ()
addArg cmd = StateT $ \ pSt ->
    let args = pArgs pSt
    in  Right $ ( () , pSt { pArgs = args ++ [ cmd ] } )

addShortOptArg :: Option -> StateT ParserSt ( Either ParseError ) ()
addShortOptArg opt = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing       -> cantParse $ MissingShortArgument opt
         Just ('-':cs) -> cantParse $ MissingShortArgument opt
         Just cs       -> addOpt ( opt, cs )

addOpt :: ( Option, String ) -> StateT ParserSt ( Either ParseError ) ()
addOpt x = StateT $ \ pSt -> let xs = pOpts pSt
                             in  Right $ ( (), pSt { pOpts = xs ++ [x] } )

---------------------------------------
-- Error handling

cantParse :: ParseError -> StateT ParserSt ( Either ParseError ) ()
cantParse e = StateT $ \ pSt -> Left e

---------------------------------------
-- Helper functions

splitLong :: String -> (String, String)
splitLong s = (opt, arg)
    where opt = takeWhile ( /= '=' ) s
          arg = case dropWhile ( /= '=' ) s of
                     '=':ss    -> ss
                     otherwise -> []
