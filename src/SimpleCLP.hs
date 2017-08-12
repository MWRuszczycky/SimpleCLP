module SimpleCLP
    ( parseCmdLine
    , Option (..)
    , ValidOptions (..)
    , OptArgs (..)
    , ParseError (..)
    ) where

import Control.Monad.Trans.State ( StateT, StateT (..), execStateT )
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

data ParserSt = ParserSt { valid    :: ValidOptions
                         , unparsed :: [String]
                         , pOpts    :: Options
                         , pArgs    :: Arguments
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
initParser vopts cmds = ParserSt { valid    = vopts
                                 , unparsed = cmds
                                 , pOpts    = []
                                 , pArgs    = [] }

parseM :: StateT ParserSt ( Either ParseError ) ()
parseM = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing           -> return ()
         Just ('-':'-':cs) -> parseLongOpt cs >> parseM
         --Just ('-':cs)     -> parseShortOpt cs >> parseM
         Just cs           -> parseArg cs >> parseM

parseArg :: String -> StateT ParserSt ( Either ParseError ) ()
parseArg cmd = StateT $ \ pSt ->
    let args = pArgs pSt
    in  Right $ ( () , pSt { pArgs = ( Arg cmd ):args } )

parseLongOpt :: String -> StateT ParserSt ( Either ParseError ) ()
parseLongOpt cmd = StateT $ \ pSt ->
    let mbOpt = getLongOpt ( valid pSt ) cmd
    in  case mbOpt of
             Nothing                     -> Left MissingLongOption
             Just ( LongArg _ , Arg [] ) -> Left MissingLongArgument
             Just opt                    -> let opts = pOpts pSt
                                            in Right $
                                               ( () , pSt { pOpts = opt:opts } )

getLongOpt :: ValidOptions -> String -> Maybe ( Option, Argument )
getLongOpt vOpts cmd
    | elem ( Long cmd ) vOpts = Just ( Long cmd, NoArg )
    | elem ( LongArg splitCmd ) vOpts = Just ( LongArg splitCmd, Arg splitArg )
    | otherwise =  Nothing
    where ( splitCmd, splitArg ) = splitLong cmd

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
