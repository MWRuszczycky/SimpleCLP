module SimpleCLP
    ( parseCmdLine
    , Option (..)
    , ValidOptions (..)
    , OptArgs (..)
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
    | MissingLongOption
    | MissingLongArgument
    deriving ( Show, Eq )

type ValidOptions = [ Option ]

type Options = [ (Option, Argument) ]

type Arguments = [ Argument ]

type OptArgs = (Options, Arguments)

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

parseCmdLine :: ValidOptions -> [String] -> Either ParseError OptArgs
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
         Just ('-':'-':cs) -> parseLongOpt cs >> parseM
         -- Just ('-':cs)     -> parseShortOpt cs >> parseM
         Just cs           -> parseArg cs >> parseM

parseArg :: String -> StateT ParserSt ( Either ParseError ) ()
parseArg cmd = StateT $ \ pSt ->
    let args = pArgs pSt
    in  Right $ ( () , pSt { pArgs = ( Arg cmd ):args } )

parseLongOpt :: String -> StateT ParserSt ( Either ParseError ) ()
parseLongOpt cmd = do
    vOpts <- fmap validLong get
    case lookup ( fst . splitLong $ cmd ) vOpts of
         Nothing                -> cantParse MissingLongOption
         Just opt@( Long s )    -> addOpt ( opt, NoArg )
         Just opt@( LongArg s ) -> do let arg = snd . splitLong $ cmd
                                      if null arg
                                         then cantParse MissingLongArgument
                                         else addOpt ( opt, Arg arg )

addOpt :: ( Option, Argument ) -> StateT ParserSt ( Either ParseError ) ()
addOpt x = StateT $ \ pSt -> let xs = pOpts pSt
                             in  Right $ ( (), pSt { pOpts = x:xs } )

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
