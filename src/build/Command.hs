module Command where

-- String = null-terminated C string
type ByteString = String -- array of bytes with length, may contain nulls

data Stdin
  = String ByteString -- ^ Given as the ``stdin`` of the spawned process.
  | FileIn FilePath -- ^ Take the ``stdin`` from a file.
  | Inherit{-^ Cause the stdin from Cot's process to be inherited. Might also require NoProcessGroup on Linux. Ignored if you explicitly pass a stdin.
Two processes cannot both take input from the same device at the same time. To make sure that only one process tries to take input from the terminal at once, Cot will invalidate the standard input streams of all but one running process. If another process attempts to read from standard input it will usually incur a ‘Broken pipe’ signal.

The first process run will always get it first, and the first process started after that one finishes will get it next, and so on. If processes run in parallel it is unpredictable which process will have a valid standard input stream (which will come from the terminal, or wherever you redirect the standard input).
-}

data CommandOption
  = Cwd FilePath -- ^ Change the current directory of the spawned process. By default uses the parent process's current directory. If multiple options are specified, each is interpreted relative to the previous one: ``[Cwd "/", Cwd "etc"]`` is equivalent to ``[Cwd "/etc"]``.
  | Env [(String,String)] -- ^ Replace the environment block in the spawned process. By default uses the Cot process's environment.
  | AddEnv String String -- ^ Add an environment variable in the child process.
  | RemEnv String -- ^ Remove an environment variable from the child process.
  | AddPath [String] [String] -- ^ Add some items to the prefix and suffix of the ``$PATH`` variable.
  | Shell { escape :: Bool } -- ^ Instead of executing the command directly, pass the command to the shell. If escaping is false any arguments will be joined with spaces. With true arguments are escaped properly for bash.
  | Traced String -- ^ Name to use with 'traced', or ``\"\"`` for no tracing. By default traces using the name of the executable.
  | Timeout Double -- ^ Abort the computation after N seconds, will raise a failure exit code. Calls 'interruptProcessGroupOf' and 'terminateProcess', but may sometimes fail to abort the process and not timeout.
  | AutoDeps -- ^ Compute dependencies automatically. Only works if 'shakeLintInside' has been set to the files where autodeps might live.
  | UserCommand String -- ^ The command shown to the user. Defaults to the actual command.
  | FSAOptions String -- ^ Options to ``fsatrace``, a list of strings with characters such as ``\"r\"`` (reads) ``\"w\"`` (writes). Defaults to ``\"rwmdqt\"`` if the output of ``fsatrace`` is required.
  | NoProcessGroup -- ^ Don't run the process in its own group. Required when running ``docker``. Will mean that process timeouts and asynchronous exceptions may not properly clean up child processes.
  | EchoCommand Bool -- ^ Print each command to stdout before it is executed. We call this echoing because it gives the appearance that you are typing the lines yourself.
  | CollectExitCode
  -- ^ Collect the 'ExitCode' of the process.
  | PrintExitStatus -- ^ print exit status if non-zero
  | IgnoreExitStatus -- ^ continue execution regardless of value.
  | ErrorExitStatus -- ^ if the exit status is nonzero, throw an error and stop executing the task (default)
  | Run Bool -- ^ When false, treat the command as an operation that does nothing, produces no output, and returns a 0 exit code.
  | Stdin Stdin -- ^ Given as the ``stdin`` of the spawned process. By default the ``stdin`` is inherited.
  | CloseFileHandles -- ^ Before starting the command in the child process, close all file handles except stdin, stdout, stderr in the child process. Uses ``close_fds`` from package process and comes with the same caveats, i.e. runtime is linear with the maximum number of open file handles (``RLIMIT_NOFILE``, see ``man 2 getrlimit`` on Linux).
  | CollectStdout { trim :: Bool }
      {-^ Collect the stdout of the process.
   Note that most programs end their output with a trailing newline, so calling
   ghc --numeric-version will result in "6.8.3\n". If you want to automatically
   trim the resulting string, set trim to True.
   -}
  | FileStdout FilePath -- ^ Should I put the ``stdout`` to a file.
  | EchoStdout Bool -- ^ Should I echo the stream to the parent? Defaults to 'True' unless 'CollectStdout' or 'FileStdout' is used.
  | WithStdout Bool -- ^ Should I include the ``stdout`` in the exception if the command fails? Defaults to 'False'.
  | CollectStderr { trim :: Bool }
    {-^ Collect the stderr of the process. -}
  | CollectStdoutStderr { trim :: Bool }
    {-^ Collect stdout and stderr of the process combined as a single stream. -}
  | WithStderr Bool -- ^ Should I include the ``stderr`` in the exception if the command fails? Defaults to 'True'.
  | EchoStderr Bool -- ^ Should I echo the ``stderr``? Defaults to 'True' unless a 'Stderr' result is required or you use 'FileStderr'.
  | FileStderr FilePath -- ^ Should I put the ``stderr`` to a file.
  | Handle
    -- ^ Return the 'ProcessHandle' of the process.
    --   If you do use the process handle, the command will run asynchronously and the call
    --   will return as soon as the process is spawned. Any 'Stdout' or 'Stderr' captures will return empty strings.
  | Time
    -- ^ Return the time taken to execute the process.
  | UserCmdLine
    -- ^ Return the user command line used for the process. This command line will be approximate -
    --   suitable for user diagnostics, but it may not match the actual execution.
  | RealCmdLine
    -- ^ Return the exact command line used for the process.
