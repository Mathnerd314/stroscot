{-# LANGUAGE RebindableSyntax #-}
import Prelude hiding ((>>=),(>>),IO)

(>>) e stmts = \c -> e (stmts c)
(>>=) e pstmts = \c -> e (\x -> pstmts x c)

data TaskE = Print String TaskE | ReadFile String (String -> TaskE) | Die | Error String | Continue

type IO a = (a -> Task) -> Task


-- | Catch an exception.
catch :: IO a -> (String -> IO a) -> IO a
catch m f c =
  case m Continue of
    Error l -> f (replace l Continue) c
    Continue a -> c a
    r -> fmap r

isDesiredError e = e == "ArithmeticException"

walkTask (Error e)
  | isDesiredError e = shift (const e)
  | otherwise = e
walkTask (Print s t) = Print s (walkTask t)
walkTask (ReadFile f t) = ReadFile f (fmap walkTask t)
walkTask Die = Die

-- try-catch-else-finally
-- we handle it with continuations
-- and the error-redefining trick

tryCatch foo isDesiredError handle = do
  case reset (Left (walkTask (foo Continue))) of
    e | isDesiredError e -> handle e
    f -> f

-- bubbling

case x of
  e | isError e && isDesiredError (firstError e) -> ...

-- finally

case x of
  e -> e { continuation = cleanup (continuation e) }

-- else clause from Python:
-- executed if control flows normally off the end of the try clause and is not protected by the catch clauses of the try.

try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))



-- IO handlers. Since they discard the result of the "after" computation there is no pure equivalent

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
  -- mask shields the cleanup action from being attacked by asynchronous exceptions, allowing exceptions inside restore.
  mask $ \restore -> do
    x <- before
    -- try catches exceptions and allows cleanup to occur.
    res1 <- try $ restore $ thing x
    case res1 of
      Left (e1 :: SomeException) -> do
        -- ``uninterruptibleMask_`` blocks interrupts from interrupting the after handler
        _ :: Either SomeException b <- try $ uninterruptibleMask_ $ after x -- try swallows exception from after
        -- throw rethrows the exception
        throw e1
      Right y -> do
        _ <- uninterruptibleMask_ $ after x -- mask needed?
        return y


finally :: IO a -> IO b -> IO a
a `finally` sequel =
  mask $ \restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r


onException :: IO a -> IO b -> IO a
onException io what = io `catch` \e -> do _ <- what
                                          throwIO (e :: SomeException)

