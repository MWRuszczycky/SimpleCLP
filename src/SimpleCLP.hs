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

data Argument =
      Arg String
    | NoArg
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

type Options = [ (Option, Argument) ]

type Arguments = [ Argument ]

type OptsArgs = (Options, Arguments)

---------------------------------------
-- Private types

data ParserSt = ParserSt { validShort :: [ (Char, Option) ]
                         , validLong  :: [ (String, Option) ]
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
initParser vopts cmds = ParserSt { validShort = foldl getShort [] vopts
                                 , validLong  = foldl getLong [] vopts
                                 , unparsed   = cmds
                                 , pOpts      = []
                                 , pArgs      = [] }

getShort :: [ ( Char, Option ) ] -> Option -> [ ( Char, Option )]
getShort acc ( Short c )    = acc ++ [ ( c, Short c ) ]
getShort acc ( ShortArg c ) = acc ++ [ ( c, ShortArg c ) ]
getShort acc _              = acc

getLong :: [ ( String, Option ) ] -> Option -> [ ( String, Option ) ]
getLong acc ( Long s )    = acc ++ [ ( s, Long s ) ]
getLong acc ( LongArg s ) = acc ++ [ ( s, LongArg s ) ]
getLong acc _             = acc

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
         Just opt@( Short _ )    -> addOpt ( opt, NoArg )
         Just opt@( ShortArg _ ) -> addShortOptArg opt

addShortOptArg :: Option -> StateT ParserSt ( Either ParseError ) ()
addShortOptArg opt = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing       -> cantParse MissingShortArgument
         Just ('-':cs) -> cantParse MissingShortArgument
         Just cs       -> addOpt ( opt, Arg cs )

addArg :: String -> StateT ParserSt ( Either ParseError ) ()
addArg cmd = StateT $ \ pSt ->
    let args = pArgs pSt
    in  Right $ ( () , pSt { pArgs = args ++ [ Arg cmd ] } )

parseLong :: String -> StateT ParserSt ( Either ParseError ) ()
parseLong cmd = do
    vOpts <- fmap validLong get
    case lookup ( fst . splitLong $ cmd ) vOpts of
         Nothing                -> cantParse MissingLongOption
         Just opt@( Long _ )    -> addOpt ( opt, NoArg )
         Just opt@( LongArg _ ) -> do let arg = snd . splitLong $ cmd
                                      if null arg
                                         then cantParse MissingLongArgument
                                         else addOpt ( opt, Arg arg )

addOpt :: ( Option, Argument ) -> StateT ParserSt ( Either ParseError ) ()
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
