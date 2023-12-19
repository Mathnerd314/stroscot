Aspects
#######

Aspect-Oriented Programming (AOP) is a programming paradigm that allows separating concerns (aspects) that cut across multiple modules or components. Used properly, AOP increases codebase modularity, maintainability, reusability, readability, consistency, and testability. In particular, developers may isolate business logic, and separately manage cross-cutting aspects which would otherwise be spread across different parts of the code. These aspects can even be dynamically managed, providing flexibility to enable or disable certain behaviors at runtime without altering the core application logic.

Costs and benefits
==================

Aspect-oriented programming (AOP) has shown significant benefits and practical applications in software development. It promotes modularity, separation/centralization of concerns, code reusability, readability, maintainability, flexibility, adaptability, consistency. These lead to higher developer productivity.

However, it has a significant learning curve. First there is the new terminology. Also, AOP introduces a different code organization. This organization is hopefully more maintainable as concerns are placed together and business logic is separated from implementation, but identifying the actual flow of code can be difficult amongst the many "weaved" aspects and advice. A debugger can trace an existing program, but recognizing which parts of a new program are suitable for aspect extraction can be tricky. Similarly error messages and stack traces need to be AOP-aware. There is also a general lack of tooling support for AOP. Specifically testing, unit testing needs to be AOP-aware.

To help developers overcome these challenges, it's important to provide comprehensive resources, documentation, tutorials, and examples that explain AOP concepts clearly. Hands-on practice with small, manageable projects can also help developers gain confidence and proficiency in using AOP. Additionally, offering mentoring or training sessions can accelerate the learning process by addressing developers' questions and concerns directly. Over time, as developers become more familiar with AOP concepts and practices, the learning curve becomes less steep, and the benefits of AOP can become more evident in their development workflows.

AOP can also introduce some performance overhead, although most studies have actually found a performance increase.

Uses
====

AOP has been successfully applied in various domains: detailed logging. authentication and authorization checks, error-handling logic, caching, proper initiation, commit, or rollback procedures for transactions. and performance monitoring.

Terminology
===========

* Join point: A join point is a specific point during the execution of your application. For example, before a method call, after a method call, a field access, an object instantiation. In Stroscot it corresponds to an imperative value (with continuation included). We can also expose non-side-effectful method calls as join points.
* Pointcut: The pointcut or target is a predicate which specifies a set of join points. Generally limited by some scope, like a class, method, or module.
* Advice: Advice is code that is executed at the chosen set of join points. It transforms the join point action to a new join point action. For example Stroscot's destructors are implemented this way, inserting the free call before executing the join point.
* Aspect: an aspect is the combination of some advice and a pointcut. For example, applying a logger before all the method calls in a particular class constitutes an aspect. An aspect may have stateful variables / identifiers shared among aspect invocations.
* Weaving: This is the process of integrating aspect together into the application. It can be done compile-time, by generating weaved code, or at runtime, by modifying dispatch behavior.
* Introduction: introducing new methods, fields, or interfaces in a class at runtime
* Business concern: the straight-line code which does the simple thing you care about
* Cross-cutting concern: aspects that interact with the business concern, such as logging, security, transactions, etc.


