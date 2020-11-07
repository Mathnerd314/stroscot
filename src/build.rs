
{ __ignoreNulls = true; all = [ ... ]; args = [ ... ]; buildInputs = [ ... ]; builder = "/nix/store/2jysm3dfsgby5sw5jgj43qjrb5v79ms9-bash-4.4-p23/bin/bash"; configureFlags = [ ... ]; depsBuildBuild = [ ... ]; depsBuildBuildPropagated = [ ... ]; depsBuildTarget = [ ... ]; depsBuildTargetPropagated = [ ... ]; depsHostHost = [ ... ]; depsHostHostPropagated = [ ... ]; depsTargetTarget = [ ... ]; depsTargetTargetPropagated = [ ... ]; dev = «derivation /nix/store/fkpvvydc54wbvxpyznqwylym7yl0gv7f-bluez-5.54.drv»; doCheck = true; doInstallCheck = false; drvAttrs = { ... }; drvPath = "/nix/store/fkpvvydc54wbvxpyznqwylym7yl0gv7f-bluez-5.54.drv"; enableParallelBuilding = true; enableParallelChecking = true; inputDerivation = «derivation /nix/store/ia8cxvdv4igqknb3panj405zb9c6v5bp-bluez-5.54.drv»; installFlags = [ ... ]; makeFlags = [ ... ]; meta = { ... }; name = "bluez-5.54"; nativeBuildInputs = [ ... ]; out = «derivation /nix/store/fkpvvydc54wbvxpyznqwylym7yl0gv7f-bluez-5.54.drv»; outPath = "/nix/store/ik15233hki3hq4gvib0jykabppzwhiv9-bluez-5.54"; outputName = "out"; outputUnspecified = true; outputs = [ ... ]; override = { ... }; overrideAttrs = «lambda @ /home/aerg/projects/nixpkgs-channels/lib/customisation.nix:85:73»; overrideDerivation = «lambda @ /home/aerg/projects/nixpkgs-channels/lib/customisation.nix:84:32»; passthru = { ... }; patches = [ ... ]; pname = "bluez"; postInstall = "mkdir -p $test/{bin,test}\ncp -a test $test\npushd $test/test\nfor a in \\\n        simple-agent \\\n        test-adapter \\\n        test-device \\\n        test-thermometer \\\n        list-devices \\\n        monitor-bluetooth \\\n        ; do\n  ln -s ../test/$a $test/bin/bluez-$a\ndone\npopd\nwrapPythonProgramsIn $test/test \"$test/test /nix/store/mzkzl2cpqqmz57i2b582hdfi2y98p7zq-python3.8-dbus-python-1.2.16 /nix/store/c9nh0sylvm738vh83pqwpn9i22lwywl1-python3.8-pygobject-3.36.1 /nix/store/rpd6py1n50rdrrg2hmxdnyhscx2c2yj4-python-recursive-pth-loader-1.0\"\n# for bluez4 compatibility for NixOS\nmkdir $out/sbin\nln -s ../libexec/bluetooth/bluetoothd $out/sbin/bluetoothd\nln -s ../libexec/bluetooth/obexd $out/sbin/obexd\n\n# Add extra configuration\nmkdir $out/etc/bluetooth\nln -s /etc/bluetooth/main.conf $out/etc/bluetooth/main.conf\n\n# Add missing tools, ref https://git.archlinux.org/svntogit/packages.git/tree/trunk/PKGBUILD?h=packages/bluez\nfor files in `find tools/ -type f -perm -755`; do\n  filename=$(basename $files)\n  install -Dm755 tools/$filename $out/bin/$filename\ndone\n"; postPatch = "substituteInPlace tools/hid2hci.rules \\\n  --replace /sbin/udevadm /nix/store/f6r3yar1zx1zrll52wmf300h8k8gibk8-systemd-246/bin/udevadm \\\n  --replace \"hid2hci \" \"$out/lib/udev/hid2hci \"\n"; propagatedBuildInputs = [ ... ]; propagatedNativeBuildInputs = [ ... ]; src = «derivation /nix/store/i8p4dxir14alcfnxg9p2i9bhsmf6kwd5-bluez-5.54.tar.xz.drv»; stdenv = «derivation /nix/store/m15naxf285zafnsnlzfaxy0r10dzlanx-stdenv-linux.drv»; strictDeps = false; system = "x86_64-linux"; test = «derivation /nix/store/fkpvvydc54wbvxpyznqwylym7yl0gv7f-bluez-5.54.drv»; type = "notD"; userHook = null; version = "5.54"; }

all = [ ... ];
meta = { ... };
out = «derivation /nix/store/qx2i3r7wbk2znqw1z7p26fqz2dlw8ki1-texlive-memoir-3.7j.drv»;
outPath = "/nix/store/qp5cz64vlnfgvlr253l6b9gi94fvyq34-texlive-memoir-3.7j";

outputName = "out";
outputUnspecified = true;
outputs = [ ... ];

passthru = { ... };
tlType = "run";
}



/// As build commands run they can output extra dependency information
/// (e.g. header dependencies for C source) dynamically.  DepsLog collects
/// that information at build time and uses it for subsequent builds.
///
/// The on-disk format is based on two primary design constraints:
/// - it must be written to as a stream (during the build, which may be
///   interrupted);
/// - it can be read all at once on startup.  (Alternative designs, where
///   it contains indexing information, were considered and discarded as
///   too complicated to implement; if the file is small than reading it
///   fully on startup is acceptable.)
/// Here are some stats from the Windows Chrome dependency files, to
/// help guide the design space.  The total text in the files sums to
/// 90mb so some compression is warranted to keep load-time fast.
/// There's about 10k files worth of dependencies that reference about
/// 40k total paths totalling 2mb of unique strings.
///
/// Based on these stats, here's the current design.
/// The file is structured as version header followed by a sequence of records.
/// Each record is either a path string or a dependency list.
/// Numbering the path strings in file order gives them dense integer ids.
/// A dependency list maps an output id to a list of input ids.
///
/// Concretely, a record is:
///    four bytes record length, high bit indicates record type
///      (but max record sizes are capped at 512kB)
///    path records contain the string name of the path, followed by up to 3
///      padding bytes to align on 4 byte boundaries, followed by the
///      one's complement of the expected index of the record (to detect
///      concurrent writes of multiple ninja processes to the log).
///    dependency records are an array of 4-byte integers
///      [output path id,
///       output path mtime (lower 4 bytes), output path mtime (upper 4 bytes),
///       input path id, input path id...]
///      (The mtime is compared against the on-disk output path mtime
///      to verify the stored data is up-to-date.)
/// If two records reference the same output the latter one in the file
/// wins, allowing updates to just be appended to the file.  A separate
/// compaction/repacking step can run occasionally to remove dead records.



/**
 * Returns a function, that, as long as it continues to be invoked, will not
 * be triggered. The function will be called after it stops being called for
 * N milliseconds. If `immediate` is passed, trigger the function on the
 * leading edge, instead of the trailing. The function also has a property 'clear'
 * that is a function which will clear the timer to prevent previously scheduled executions.
 *
 * @source underscore.js
 * @see http://unscriptable.com/2009/03/20/debouncing-javascript-methods/
 * @param {Function} function to wrap
 * @param {Number} timeout in ms (`100`)
 * @param {Boolean} whether to execute at the beginning (`false`)
 * @api public
 */
function debounce(func, wait, immediate){
  var timeout, args, context, timestamp, result;
  if (null == wait) wait = 100;

  function later() {
    var last = Date.now() - timestamp;

    if (last < wait && last >= 0) {
      timeout = setTimeout(later, wait - last);
    } else {
      timeout = null;
      if (!immediate) {
        result = func.apply(context, args);
        context = args = null;
      }
    }
  };

  var debounced = function(){
    context = this;
    args = arguments;
    timestamp = Date.now();
    var callNow = immediate && !timeout;
    if (!timeout) timeout = setTimeout(later, wait);
    if (callNow) {
      result = func.apply(context, args);
      context = args = null;
    }

    return result;
  };

  debounced.clear = function() {
    if (timeout) {
      clearTimeout(timeout);
      timeout = null;
    }
  };

  debounced.flush = function() {
    if (timeout) {
      result = func.apply(context, args);
      context = args = null;

      clearTimeout(timeout);
      timeout = null;
    }
  };

  return debounced;
};

static int full_scan_dir(struct tent_list_head *head, int dfd, tupid_t dt)
{
	struct tent_list *tl;

	/* This is kinda tricky. We start with a dfd (for "/"), and its tupid. Then we add the
	 * tup entries for the current dt to the front of the tup_entry list. We only use one
	 * list for the whole scan, and when we hit a dt that isn't ours that means we're done
	 * a single level of the directory. We keep our dfd open until the whole subtree is
	 * checked. If at any point we stop finding directories, dfd goes to -1 and we don't
	 * stat anymore. All missing files, or those with dfd==-1 have mtime set to -1. If the
	 * mtime differs from what we have saved, we flag that as a modification. Directories get
	 * an mtime of 0, so we can distinguish between a real directory and a ghost node. If a
	 * command tries to read from '/tmp/foo/bar', but the directory 'foo' doesn't exist yet
	 * then we get a dependency on /tmp/foo. If the directory is later created we need to know
	 * to re-execute since it may now have a 'bar' file.
	 */
	if(tup_db_select_node_dir(full_scan_cb, head, dt) < 0)
		return -1;
	tent_list_foreach(tl, head) {
		struct tup_entry *tent = tl->tent;
		int new_dfd = -1;
		time_t mtime = -1;
		int scan_subdir = 0;

		if(tent->dt != dt)
			return 0;

		if(dfd != -1) {
			struct stat buf;

			if(is_full_path(tent->name.s)) {
				/* This is for Windows, since we store the C:
				 * or D: as the first path element. If it's one
				 * of these we can't fstatat it or openat it,
				 * so just open the new one and be on our way.
				 */
				new_dfd = open(tent->name.s, O_RDONLY);
				mtime = EXTERNAL_DIRECTORY_MTIME;
				scan_subdir = 1;
			} else {
				if(fstatat(dfd, tent->name.s, &buf, AT_SYMLINK_NOFOLLOW) == 0) {
					int link_to_dir = 0;

					if(S_ISDIR(buf.st_mode)) {
						mtime = EXTERNAL_DIRECTORY_MTIME;
					} else {
						if(S_ISLNK(buf.st_mode)) {
							/* If we have an external
							 * symlink, we want to keep
							 * going down the tree as if it
							 * were a directory. Otherwise,
							 * we use the mtime of the
							 * symlink itself as if it were
							 * a file.
							 */
							struct stat lnkbuf;
							if(fstatat(dfd, tent->name.s, &lnkbuf, 0) == 0) {
								if(S_ISDIR(lnkbuf.st_mode)) {
									link_to_dir = 1;
								}
							}
						}
						mtime = MTIME(buf);
					}

					if(S_ISDIR(buf.st_mode) || link_to_dir) {
						/* If we fail to open, new_dfd is -1 which means any
						 * future nodes are assumed to be un-openable as well.
						 */
						new_dfd = openat(dfd, tent->name.s, O_RDONLY);
						scan_subdir = 1;
					}
				}
			}
		}

		if(mtime != tent->mtime) {
			log_debug_tent("Update external", tent, ", oldmtime=%li, newmtime=%li\n", tent->mtime, mtime);

			scan_subdir = 1;
			/* Mark the commands as modify rather than the ghost node, since we don't
			 * expect a ghost to have flags set.
			 */
			if(tup_db_modify_cmds_by_input(tent->tnode.tupid) < 0)
				return -1;
			if(tup_db_set_mtime(tent, mtime) < 0)
				return -1;
		}

		if(scan_subdir) {
			if(full_scan_dir(head, new_dfd, tent->tnode.tupid) < 0)
				return -1;
		}
		if(new_dfd != -1) {
			if(close(new_dfd) < 0) {
				perror("close(new_dfd)");
				return -1;
			}
		}
	}
	return 0;
}

type Key
type PendingKeys

takeKey :: State PendingKeys (Maybe Key)

scanQueue = do
    key <- takeKey
    case key of
        Nothing -> return ()
        Just key -> do
            r <- scanKey key
            case r of
                CleanKey -> scanQueue
                DirtyKey -> markTasks Dirty key

data KeyCheckResult = DirtyKey | CleanKey

data Key = File | Network | Database | InMemory | Custom

scanKey :: Key -> KeyCheckResult

data TaskState = Dirty | Running | Loaded | Error

markDirty :: Key -> ()
markDirty key = do
    tasks <- findReverseDependencies key
    forM_ tasks $ \t -> do
        setState t Dirty
        markDirty t

setState t state = do
    currState <- getCurrentState t
    case (currState,state) of
        (a,b) | a == b -> return ()
        (Running,b) -> abortTask t >> reallySetState t state

runTask :: Task -> ()
runTask t


mainloop = do
    if (files) scanFiles
    wait settle_time
    if files then mainloop else runTask mainTask

After scanning all the keys, we go down starting from the top-level task.
We want a suspending build system [MMPJ20]. So there must be some way to suspend the current task when it calls a sub-task, probably just continuations like how Shake does it.

    When a task is called, we first check its state to determine whether it needs to be re-run. Dirty tasks are run immediately. Loaded tasks can be skipped immediately, as can tasks stored in the database that have not yet been marked. Otherwise, for rechecks, we run through the serialized dependency list and re-check the keys / subtasks in order (and in parallel if the subtasks are parallel). When the task is finished its state is marked as loaded / error.

    Before running a task, we clean up old build results, if any, i.e. delete all generated keys (outputs) that are still present. After running a task we store its (keyed) outputs with either verifying or constructive traces.

    To prune the store (which is a bad idea if there are multiple configurations that build different subsets), we can do as above and also load all the subtasks of present tasks. Then anything not loaded is not needed and its files etc. can be deleted.




  type Task f k v =
    { fetch :: k -> f v
    , key :: k } -> f v

-- | This scheduler builds keys recursively: to build a key it executes the
-- associated task, discovering its dependencies on the fly, and if one of the
-- dependencies is dirty, the task is suspended until the dependency is rebuilt.
-- It stores the set of keys that have already been built as part of the state
-- to avoid executing the same task twice.

-- | This rebuilder uses modification time to decide whether a key is dirty and
-- needs to be rebuilt. Used by Make.

-- | This rebuilder relies on constructive traces.

suspending :: forall i k v. Ord k => Scheduler Monad i i k v
suspending rebuilder tasks target store = fst $ execState (fetch target) (store, Set.empty)
  where
    fetch :: k -> State (Store i k v, Set k) v
    fetch key = do
        done <- gets snd
        case tasks key of
            Just task | key `Set.notMember` done -> do
                value <- gets (getValue key . fst)
                let newTask :: Task (MonadState i) k v
                    newTask = rebuilder key value task
                newValue <- liftRun newTask fetch
                modify $ \(s, d) -> (updateValue key value newValue s, Set.insert key d)
                return newValue
            _ -> gets (getValue key . fst) -- fetch the existing value

modTimeRebuilder :: Ord k => Rebuilder Applicative (MakeInfo k) k v
modTimeRebuilder key value task = Task $ \fetch -> do
    (now, modTimes) <- get
    let dirty = case Map.lookup key modTimes of
            Nothing -> True
            time -> any (\d -> Map.lookup d modTimes > time) (dependencies task)
    if not dirty
    then return value
    else do
        put (now + 1, Map.insert key now modTimes)
        run task fetch

ctRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (CT k v) k v
ctRebuilder key value task = Task $ \fetch -> do
    cachedValues <- constructCT key (fmap hash . fetch) =<< get
    if value `elem` cachedValues
    then return value -- The current value has been verified, let's keep it
    else case cachedValues of
        (cachedValue:_) -> return cachedValue -- Any cached value will do
        _ -> do -- No cached values, need to run the task
            (newValue, deps) <- track task fetch
            modify $ recordCT key newValue [ (k, hash v) | (k, v) <- deps ]
            return newValue

-- | A model of Cloud Shake: a monadic build system that uses constructive
-- traces to check if a key is up to date as well as for caching build results.
cloudShake = suspending ctRebuilder
