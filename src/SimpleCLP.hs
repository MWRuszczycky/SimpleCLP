module SimpleCLP
    ( parseCmdLine
    , getParsedArgs
    , chkOpt
    , optUsed
    , Option       (..)
    , Options      (..)
    , Arguments    (..)
    , ValidOptions (..)
    , OptsArgs     (..)
    , ParseError   (..)
    ) where

import Data.List                 ( foldl', delete )
import System.Environment        ( getArgs )
import Control.Monad.Trans.State ( StateT, StateT (..), execStateT, get )

---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------

---------------------------------------
-- Exported types

-- |Type of command line options.
data Option =
      Short    Char         -- Single character, does not take argument.
    | ShortArg Char         -- Single character, takes an argument.
    | Long     String       -- String, does not take argument.
    | LongArg  String       -- String, takes an argument.
    deriving ( Show, Eq )

-- |Result type for option lookup.
data OptResult =
      NotUsed               -- Option was not used.
    | Used                  -- Option was used but does not take an argument.
    | UsedArg String        -- Option was used with the supplied argument.
    deriving ( Show, Eq )

-- |Errors that may be raised while parsing the command line.
data ParseError =
      MissingShortOption Char       -- Short option specified but not valid.
    | MissingShortArgument Option   -- Argument not provided for a short option.
    | MissingLongOption String      -- Long option specified but not valid.
    | MissingLongArgument Option    -- Argiment not provided for a long option.
    deriving ( Show, Eq )

-- |List of options that the application will accept as valid.
type ValidOptions = [ Option ]

-- |Association list of parsed along with their arguments. If the
-- option does not take an argument, then the String is empty.
type Options = [ (Option, String) ]

-- |List of parsed non-option arguments.
type Arguments = [ String ]

-- |Pair of the parsed options and non-option arguments.
type OptsArgs = (Options, Arguments)

---------------------------------------
-- Private types

-- |Association list for raw characters and valid options.
type ShortDict = [ (Char, Option) ]

-- |Association list for raw strings and valid options.
type LongDict = [ (String, Option) ]

-- |Type of parsing state that is updated as the command line is read.
data ParserSt = ParserSt { validShort :: ShortDict  -- Valid short options.
                         , validLong  :: LongDict   -- Valid long options
                         , unparsed   :: [String]   -- Unparsed command line.
                         , pOpts      :: Options    -- Parsed options.
                         , pArgs      :: Arguments  -- Parsed non-option arguments.
                         } deriving ( Show )

---------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------

---------------------------------------------------------------------
-- Exported functions

parseCmdLine :: ValidOptions -> [String] -> Either ParseError OptsArgs
-- ^Pure version of the command line parser that takes a list of
-- valid options and the command line as a list of strings and parses
-- it to a Right pair of lists if it succeeds. The first list is an
-- association list of options paired with option arguments. Options
-- that do not take an argument are paired with an empty string. The
-- Second list is a simple list of non-option arguments as strings.
-- If an option taking an argument is specified multiple times, then
-- all of the option arguments are concatenated into single string
-- separated by spaces. Evaluates to a Left ParseError if parsing fails.
parseCmdLine vopts cmds =
    case execStateT parseM $ initParser vopts cmds of
         Left pError -> Left pError
         Right pSt   -> Right ( collect . pOpts $ pSt, pArgs pSt )

getParsedArgs :: ValidOptions -> IO ( Either ParseError OptsArgs )
getParsedArgs vopts = do
    cmds <- getArgs
    return . parseCmdLine vopts $ cmds

chkOpt :: Options -> Option -> OptResult
-- ^More expressive convenience function for looking up an option.
chkOpt opts opt = case lookup opt opts of
                       Nothing  -> NotUsed      -- Option was not specified.
                       Just ""  -> Used         -- Option was specified
                                                -- but takes no argument.
                       Just arg -> UsedArg arg  -- Option was specified with
                                                -- the given argument.

optUsed :: Options -> Option -> Bool
-- ^More expressive convenience function for checking if an option
-- was specified.
optUsed opts opt
    | chkOpt opts opt == NotUsed = False
    | otherwise                  = True

---------------------------------------------------------------------
-- Private functions

--------------------------------------
-- Initialization and finalization

initParser :: ValidOptions -> [String] -> ParserSt
-- ^Initializes a new parsing state with the supplied valid options.
initParser vopts cmds = let valid = foldl' getValid ( [], [] ) vopts
                        in  ParserSt { validShort = fst valid
                                     , validLong  = snd valid
                                     , unparsed   = cmds
                                     , pOpts      = []
                                     , pArgs      = [] }

getValid :: ( ShortDict, LongDict ) -> Option -> ( ShortDict, LongDict )
-- ^Accumulating function for partitioning valid long and short
-- options into association lists with the keyed versus the raw
-- strings and characters that specify them on the command line.
getValid (s,l) ( Short x )    = let s' = s ++ [ ( x, Short x ) ]    in ( s', l )
getValid (s,l) ( ShortArg x ) = let s' = s ++ [ ( x, ShortArg x ) ] in ( s', l )
getValid (s,l) ( Long x )     = let l' = l ++ [ ( x, Long x ) ]     in ( s, l' )
getValid (s,l) ( LongArg x )  = let l' = l ++ [ ( x, LongArg x) ]   in ( s, l' )

collect :: Options -> Options
-- ^Removes repeated specifications of an option. If a repeated
-- option takes an argument, then all of the arguments are
-- concatenated into a single string separated by spaces.
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
-- ^Main parsing monad and routing hub.
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
-- ^Pop the next command from the remaining  unparsed command line.
-- If there are no more commands left to parse, then return Nothing.
popCmd = StateT $ \ pSt -> case unparsed pSt of
                                []     -> Right ( Nothing, pSt )
                                (x:xs) -> Right ( Just x
                                                , pSt { unparsed = xs } )

addOpt :: ( Option, String ) -> StateT ParserSt ( Either ParseError ) ()
-- ^Add an option and its argument (or an empty string) to the
-- association list of parsed options.
addOpt x = StateT $ \ pSt -> let xs = pOpts pSt
                             in  Right $ ( (), pSt { pOpts = xs ++ [x] } )

cantParse :: ParseError -> StateT ParserSt ( Either ParseError ) ()
-- ^Raise a parsing error in the Either ParseError monad.
cantParse e = StateT $ \ pSt -> Left e

---------------------------------------
-- Parsing arguments

makeAllArgs :: StateT ParserSt ( Either ParseError ) ()
-- ^Continues parsing the command line treating all commands as
-- arguments. This is used after encountering "--" as a command.
makeAllArgs = do
    nxtArg <- popCmd
    case nxtArg of
         Nothing -> return ()
         Just x  -> addArg x >> makeAllArgs

addArg :: String -> StateT ParserSt ( Either ParseError ) ()
-- ^Add an argument to the list of parsed arguments.
addArg cmd = StateT $ \ pSt ->
    let args = pArgs pSt
    in  Right $ ( () , pSt { pArgs = args ++ [ cmd ] } )

---------------------------------------
-- Parsing short options

parseCluster :: String -> StateT ParserSt ( Either ParseError ) ()
-- ^Parses a cluster of short options represented as a String. This
-- also handles short options that take arguments not separated from
-- the option character by spaces.
parseCluster []     = return ()
parseCluster (c:[]) = parseShort c
parseCluster (c:cs) = do
    vOpts <- fmap validShort get
    case lookup c vOpts of
         Just a@( ShortArg _ ) -> addOpt ( a, cs )
         otherwise             -> do parseShort c
                                     parseCluster cs

parseShort :: Char -> StateT ParserSt ( Either ParseError ) ()
-- ^Parses a single character as an option.
parseShort c = do
    vOpts <- fmap validShort get
    case lookup c vOpts of
         Nothing                 -> cantParse $ MissingShortOption c
         Just opt@( Short _ )    -> addOpt ( opt, "" )
         Just opt@( ShortArg _ ) -> addShortOptArg opt

addShortOptArg :: Option -> StateT ParserSt ( Either ParseError ) ()
-- ^Handles a single character as an option that takes an argument
-- that is separated from the character by spaces.
addShortOptArg opt = do
    nxtCmd <- popCmd
    case nxtCmd of
         Nothing       -> cantParse $ MissingShortArgument opt
         Just ('-':cs) -> cantParse $ MissingShortArgument opt
         Just cs       -> addOpt ( opt, cs )

---------------------------------------
-- Parsing long options

parseLong :: String -> StateT ParserSt ( Either ParseError ) ()
-- ^Parses a string as a long argument with or without an argument.
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
-- ^Splits a string representing a long argument with an option
-- argument specified by an '=' character. The function evaluates to
-- a pair of strings, where the first is the long option and the
-- second is the argument of the option. If no argument is specified,
-- then the second element in the pair is an empty string.
splitLong s = (opt, arg)
    where opt = takeWhile ( /= '=' ) s
          arg = case dropWhile ( /= '=' ) s of
                     '=':ss    -> ss
                     otherwise -> []
