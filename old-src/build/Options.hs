import Command

data CotOptions = Options
  { commandOptions :: [CmdOption] -- ^ Defaults to ``[]``. Additional options to be passed to all command invocations.
  }

defaultCotOptions = Options []

