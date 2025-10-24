requireInitiallyTimestamps =  HashMap<>();
path = "shared"
genBy = XodusDatabase.createFileDatabase(path)
super(report,  DynamicAnalysis(report, genBy))
executingStack =  ExecutingStack();

knownConsistentUnits = new HashSet<>();
knownInconsistentUnits = new HashMap<>();
assumedUnits = new HashSet<>();
executeStack = new CycleDetectingStack<>();
requireStack = new CycleDetectingStack<>();

BuildUnit<Out> requireInitially(BuildRequest<In, Out, B, F> buildReq) throws Throwable {
  try {
    currentThread = Thread.currentThread();
    currentTime = System.currentTimeMillis();
    requireInitiallyTimestamps.put(currentThread, currentTime);
    report.messageFromSystem("Incrementally rebuild inconsistent units", false, 0);
    result = require(buildReq, true);
    return result.getUnit();
  } finally {
    Exec.shutdown();
  }
}

BuildRequirement<Out> require(final BuildRequest<In, Out, B, F> buildReq, boolean needBuildResult) throws IOException {
  builder = buildReq.createBuilder();
  report.buildRequirement(buildReq);
  dep = builder.persistentPath();
  depResult = BuildUnit.read(dep);

    // Don't execute require because it is cyclic, callStack keeps track of this

    // Need to check that before putting dep on the requires Stack because
    // otherwise dep has always been required
    alreadyRequired = requireStack.push(buildReq);
    executed = false;
    try {
      if (alreadyRequired) {
        report.messageFromSystem("Already required " + buildReq, false, 7);
        assumptionIncomplete = {
          for (BuildRequest<?,?,?,?> req : sccs.getSetMembers(buildReq))
            if (knownInconsistentUnits.containsKey(req))
              return true;
          return false;
        }
        report.messageFromSystem("Assumptions inconsistent " + assumptionIncomplete, false, 10);
        if (!assumptionIncomplete) {
          return yield(buildReq, builder, depResult);
        } else {
          report.messageFromSystem("Detected Require cycle for " + buildReq, false, 7);
          List<BuildRequest<?, ?, ?, ?>> deps = sccs.getSetMembers(sccs.getSet(buildReq));
          cycle = BuildCycle(buildReq, deps);
          cycleCause = executingStack.topMostEntry(cycle.getCycleComponents());

          cycle =  BuildCycle(cycleCause, cycle.getCycleComponents());
          ex =  BuildCycleException("Require build cycle " + cycle.getCycleComponents().size() + " on " + dep, cycleCause, cycle);
          throw ex;
        }
      }

      if (depResult != null && !depResult.isExpired() && knownConsistentUnits.contains(buildReq))
        return yield(buildReq, builder, depResult);

      reasons =  TreeSet<IReporting.BuildReason>();

      AbsoluteComparedFile aDep = AbsoluteComparedFile.absolute(dep);
      knownInconsistent = knownInconsistentUnits.get(aDep);
      if (knownInconsistent != null)
        reasons.addAll(knownInconsistent);

      if (depResult == null) {
        reasons.add(BuildReason.NoBuildSummary);
      } else {
        if (!depResult.getGeneratedBy().deepEquals(buildReq)) {
          reasons.add(BuildReason.ChangedBuilderInput);
        }

        localInconsistencyReason = depResult.isConsistentNonrequirementsReason();
        if (localInconsistencyReason != InconsistenyReason.NO_REASON) {
          reasons.add(BuildReason.from(localInconsistencyReason));
        }

        noOut = !(depResult.getBuildResult() instanceof build.pluto.output.Out<?>);
        expiredOutput = noOut ? false : ((build.pluto.output.Out<?>) depResult.getBuildResult()).expired();
        if (needBuildResult && expiredOutput) {
          reasons.add(BuildReason.ExpiredOutput);
        }
      }

      if (!reasons.isEmpty()) {
        // Local inconsistency should execute the builder regardless whether it
        // has been required to detect the cycle
        // TODO should inconsistent file requirements trigger the same, they
        // should i think
        executed = true;
        return executeBuilder(builder, dep, buildReq, reasons);
      }

      for (Requirement req : depResult.getRequirements()) {
        if (!req.tryMakeConsistent(this)) {
          executed = true;
          // Could get consistent because it was part of a cycle which is
          // compiled now
          if (knownConsistentUnits.contains(buildReq))
            return yield(buildReq, builder, depResult);

          reasons.add(BuildReason.InconsistentRequirement);
          return executeBuilder(builder, dep, buildReq, reasons);
        }
      }

      // Note: This handles the non cyclic case too, then all other cyclic
      // requests are empty, thus all assumed

      // Checks whether all other cyclic requests are assumed to be consistent,
      // which means, that all of them reached this point
      // already, thus checked all assumptions
      if ({
    for (BuildRequest<?,?,?,?> req : sccs.getSetMembers(buildReq))
      if (req != buildReq && !assumedUnits.contains(req))
        return false;
    return true;
    }) {
        // If yes, together with this unit, all cyclic requests are checked and
        // the cycle is consistent

    knownConsistentUnits.addAll(sccs.getSetMembers(buildReq));
      } else {
        // No, then only mark this as assummed
        this.assumedUnits.add(buildReq);
      }

      report.skippedBuilder(buildReq, depResult);
    } finally {
      C poppedEntry = callStack.remove(callStack.size()-1);
      assert poppedEntry.equals(buildReq) : "Got the wrong build stack entry from the stack";
      report.finishedBuildRequirement(buildReq);
    }

    return yield(buildReq, builder, depResult);
  }


executeBuilder : Builder<In, Out> -> File -> BuildRequest<In, Out, B, F> -> Set<BuildReason> -> BuildRequirement<Out> throws IOException
executeBuilder builder dep buildReq reasons
    knownInconsistentUnits.put(buildReq, reasons);

    depResult = BuildUnit.read(dep);
    previousDepResult = depResult == null ? null : depResult.clone();

    dynamicAnalysis.reset(depResult);
    report.startedBuilder(buildReq, builder, depResult, reasons);

    depResult = BuildUnit.create(dep, buildReq);

    setUpMetaDependency(builder, depResult);

    // First step: cycle detection
    executingStack.push(buildReq);

    inputHash = DeepEquals.deepHashCode(builder.getInput());

    depResult.setState(BuildUnit.State.IN_PROGESS);
    regularFinish = false;

    try {
      // call the actual builder
      out = builder.triggerBuild(depResult, this, previousDepResult);
      depResult.setBuildResult(out);
      if (!depResult.isFinished())
        depResult.setState(BuildUnit.State.SUCCESS);
      regularFinish = true;
    } finally {
      if (!depResult.isFinished()) {
        depResult.setState(BuildUnit.State.FAILURE);
        depResult.requireOther(Requirement.FALSE);
      }

      executingStack.pop(buildReq);
      knownConsistentUnits.add(buildReq);
      knownInconsistentUnits.remove(buildReq);

      try {
        dynamicAnalysis.check(depResult, inputHash);
        assert depResult.isConsistentShallowReason() == InconsistenyReason.NO_REASON :
          "Build manager does not guarantee soundness, got consistency status " + depResult.isConsistentShallowReason() + " for " + depResult.getPersistentPath();
      } finally {
        depResult.write(dep);
      }

      if (regularFinish && depResult.getState() == BuildUnit.State.FAILURE) {
        throw  RequiredBuilderFailed( BuildRequirement<Out>(depResult, buildReq),  Error("Builder failed"));
      }
    }

    return  BuildRequirement<Out>(depResult, buildReq);
  }

  BuildRequirement<Out> yield(BuildRequest<In, Out, B, F> req, B builder, BuildUnit<Out> unit) {
    if (unit.hasFailed()) {
      e =  RequiredBuilderFailed( BuildRequirement<Out>(unit, req), "no rebuild of failing builder");
      report.messageFromBuilder(e.getMessage(), true, builder);
      throw e;
    }
    return  BuildRequirement<>(unit, req);
  }
}
