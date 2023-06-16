-- trifecta
type Parser a =
  forall r.
    -- behavior continuations:
    -- epsilon success: the parser has consumed no input and has a result as well as a possible Err; the position and chunk are unchanged (see pure)
    (a -> Err -> It Rope r) ->
    -- epsilon failure: the parser has consumed no input and is failing with the given Err; the position and chunk are unchanged (see empty)
    (Err -> It Rope r) ->
    -- committed success: the parser has consumed input and is yielding the result, set of expected strings that would have permitted this parse to continue, new position, and residual chunk to the continuation.
    (a -> Set String -> Delta -> ByteString -> It Rope r) ->
    -- committed failure: the parser has consumed input and is failing with a given ErrInfo (user-facing error message)
    (ErrInfo -> It Rope r) ->
    -- the current position
    Delta ->
    --- the chunk of input currently under analysis
    ByteString ->
    It Rope r

data Err = Err { _reason :: Maybe (Doc AnsiStyle), _footnotes :: [Doc AnsiStyle], _expected :: Set String, _finalDeltas :: [Delta] }
-- user-facing error message
data ErrInfo = ErrInfo { _errDoc :: Doc AnsiStyle, _errDeltas :: [Delta] }

data Delta = Delta { number_of_bytes :: Int64 }
{- and more structured location info:
number of characters before/after tab
number of bytes/characters since last newline
number of newlines seen since file start / last LINE directive
current file name / line directive
-}

data It r a = Pure a | It a (r -> It r a)
data Rope = Rope !Delta !(FingerTree Delta Strand)
data Strand = Strand !ByteString !Delta | Skipping !Delta
-- | package fingertree: A representation of a sequence of values of type @a@ and measure type @v@, allowing
-- access to the ends in constant time, and append and split in time logarithmic in the size of the smaller piece.
data FingerTree v a
    = Empty
    | Single a
    | Deep !v !(Digit a) (FingerTree v (Node v a)) !(Digit a)
data Digit a = One a | Two a a | Three a a a | Four a a a a
data Node v a = Node2 !v a a | Node3 !v a a a

-- attoparsec
type Parser i a = forall r.
  State i -> Pos -> More ->
  Failure i (State i) r ->
  Success i (State i) a r ->
  IResult i r

type Failure i t r = t -> Pos -> More -> [String] -> String -> IResult i r
type Success i t a r = t -> Pos -> More -> a -> IResult i r
data More = Complete | Incomplete -- Have we read all available input?

type instance State ByteString = Buffer
data Buffer = Buffer { s :: ByteString, cap :: Int, gen :: Int } -- An "immutable" buffer that makes one-shot appends less expensive by overallocating and putting the append in the extra space. Not thread-safe.
type Pos = Int

data IResult i r
  = Fail { unconsumedInput :: i, contexts :: [String], errorMessage :: String }
  | Partial (i -> IResult i r) -- Needs more input
  | Done { unconsumedInput :: i, successful_result :: r }

-- attoparsec Zepto
type S = ByteString
data Result a = Fail String | OK !a S
type ZeptoT m a = S -> m (Result a)

-- parsec
type ParsecT s u m a = forall b .
  State s u ->
  (a -> State s u -> ParseError -> m b) -> -- consumed ok
  (ParseError -> m b)                   -> -- consumed err
  (a -> State s u -> ParseError -> m b) -> -- empty ok
  (ParseError -> m b)                   -> -- empty err
  m b

data Consumed a  = Consumed a | Empty !a
data Reply s u a = Ok a !(State s u) ParseError | Error ParseError
data ParseError = ParseError !SourcePos [Message]
data Message = SysUnExpect !String -- @ library generated unexpect
             | UnExpect    !String -- @ unexpected something
             | Expect      !String -- @ expecting something
             | Message     !String -- @ raw message
data State s u = State { stateInput :: s, statePos   :: !SourcePos, stateUser  :: !u }
data SourcePos  = SourcePos SourceName !Line !Column

-- megaparsec
type ParsecT e s m a = -- custom error component type e, stream type s, underlying monad m and return type a.
  forall b. State s e ->
    (a -> State s e -> Hints (Token s) -> m b) ->
    (ParseError s e -> State s e -> m b) ->
    (a -> State s e -> Hints (Token s) -> m b) ->
    (ParseError s e -> State s e -> m b) ->
    m b


-- | All information available after parsing. This includes consumption of
-- input, success (with the returned value) or failure (with the parse
-- error), and parser state at the end of parsing.
data Reply e s a = Reply (State s e) Consumption (Result s e a)

data Consumption
  = -- | Some part of input stream was consumed
    Consumed
  | -- | No input was consumed
    Virgin

data Result s e a
  = -- | Parser succeeded
    OK a
  | -- | Parser failed
    Error (ParseError s e)


data State s e = State
  { -- | The rest of input to process
    stateInput :: s,
    -- | Number of processed tokens so far
    stateOffset :: {-# UNPACK #-} !Int,
    -- | State that is used for line\/column calculation
    statePosState :: PosState s,
    -- | Collection of “delayed” 'ParseError's in reverse order. This means
    -- that the last registered error is the first element of the list.
    stateParseErrors :: [ParseError s e]
  }

-- | Cached SourcePos, used for error messages.
--   It can be computed as a function InitState -> State -> SourcePos by scanning the input for newlines, but
--   megaparsec does it as on-demand incremental updates (stateOffset : Int) -> PosState -> (SourcePos, PosState)
--   using Text.Megaparsec.Stream.reachOffset
data PosState s = PosState
  { -- | The rest of input to process
    pstateInput :: s,
    -- | Offset corresponding to beginning of 'pstateInput'
    pstateOffset :: !Int,
    -- | Source position corresponding to beginning of 'pstateInput'
    pstateSourcePos :: !SourcePos,
    -- | Tab width to use for column calculation
    pstateTabWidth :: Pos,
    -- | Prefix to prepend to offending line
    pstateLinePrefix :: String
  }

-- | 'Hints' represent a collection of 'ErrorItem's to be included into
-- 'ParseError' (when it's a 'TrivialError') as “expected” message items
-- when a parser fails without consuming input right after successful parser
-- that produced the hints.
newtype Hints t = Hints (Set (ErrorItem t))
data ErrorItem t
  = -- | Non-empty stream of tokens
    Tokens (NonEmpty t)
  | -- | Label (cannot be empty)
    Label (NonEmpty Char)
  | -- | End of input
    EndOfInput

instance Stream (ShareInput B.ByteString) where
  type Token (ShareInput B.ByteString) = Word8
  type Tokens (ShareInput B.ByteString) = B.ByteString

data ParseError s e
  = -- | Trivial errors, generated by Megaparsec's machinery.
    TrivialError { errorOffset :: Int, unexpectedToken :: (Maybe (ErrorItem (Token s))), expectedTokens :: (Set (ErrorItem (Token s))) }
  | -- | Fancy, custom errors.
    FancyError { errorOffset :: Int, customErrors :: (Set (ErrorFancy e)) }


-- | Additional error data, extendable by user. When no custom data is
-- necessary, the type is typically indexed by 'Void' to “cancel” the
-- 'ErrorCustom' constructor.
--
-- @since 6.0.0
data ErrorFancy e
  = -- | 'fail' has been used in parser monad
    ErrorFail String
  | -- | Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
    ErrorIndentation Ordering Pos Pos
  | -- | Custom error data
    ErrorCustom e
