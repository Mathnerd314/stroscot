# Security

Stroscot aims to have built-in security features. This means providing security functionality in the standard library, such as encryption algorithms and communication protocols, but also designing the library and the language so that it is easy to write secure code and hard to write insecure code.

## Best practices

At the moment no programming language has a good track record of being secure. [WhiteSource](https://www.mend.io/blog/is-one-programming-language-more-secure/) concluded in 2019 that Ruby had the lowest number of reported vulnerabilities out of popular web languages, but even Ruby had [5 vulnerabilities](https://www.cvedetails.com/vulnerability-list/vendor_id-7252/product_id-12215/Ruby-lang-Ruby.html) in 2022 in the base runtime, and it easy to write security antipatterns that lead to issues such as SQL injection. To improve in this area, there are various security-related features. Some are implemented in new languages, for example, Rust's borrows checker and Rune's "secrets" mechanism, but most are more obscure and live in academic papers and third-party analysis tools.

As [Hare](https://harelang.org/blog/2022-06-21-safety-features/) writes, "how do we evaluate the trade-offs of a particular safety feature?" Hare proposes to evaluate against the language goals. Stroscot's goals are functionality and minimality in that order. For security, functionality means protecting against all types of security risks, both common and uncommon. This is somewhat daunting as for example [CWE](https://cwe.mitre.org/data/index.html) lists 933 weaknesses. Certainly it would be good to go through all these but I don't have much time. Fortunately, there is the OWASP Application Security Verification Standard. It points out that the CWE is quite duplicative and condenses it down to [286 requirements](https://github.com/OWASP/ASVS/blob/d8fde8b6592af2b8022590ec9d9a1765fe920651/4.0/docs_en/OWASP%20Application%20Security%20Verification%20Standard%204.0.3-en.csv). The [bleeding edge version 5](https://github.com/OWASP/ASVS/tree/d8fde8b6592af2b8022590ec9d9a1765fe920651/5.0/en) further condenses it, removing more duplicate and out of scope requirements. The OWASP ASVS is certainly not comprehensive, as it is web-focused and community-developed, but it is definitely one of the leading standards.

## Collection of security features

So here I went through the CWE, the OWASP ASVS, and other sources, with Perplexity, and gather a voluminous list of security features that can be used to mitigate various security risks. These fall into four main categories: application-level, library-level, tool-level, and language-level. Application-level features are security practices that developers must follow when writing code. Library-level features are secure implementations of cryptographic algorithms, protocols, and parsing libraries. Tool-level features are static analyzers, dynamic testing frameworks, and CI/CD integrations that help detect vulnerabilities. Language-level features are type systems, memory management, and syntax features that are foundational to security. The application-level features are the most numerous, as they cover a wide range of security practices and policies, but do not affect the language or library design. The library-level features are important for the standard library, as it must provide secure implementations of complex algorithms and protocols in order to write secure programs. The tool-level features and language-level features are most relevant for the language design, as they can enforce security properties at compile-time and runtime, and strong integration of the language with tools can help developers write secure code. 

### Language- and Tool-Level Features

* **Integer Overflow Protection:** Compile-time arithmetic verification, unsigned types for sizes, language-enforced bounds checking. Examples: Rust's checked arithmetic by default; Java's BigInteger for overflow-safe operations.
* **Fall-Through Prevention:** Explicit `[[fallthrough]]` annotations (C++17) or exhaustive pattern matching (Rust) prevent implicit fall-through bugs in switch statements.
* **Bounds Checking:** Language features like for-each loops, bounds-checked iterators, no raw pointer arithmetic. Examples: Python's slice notation; Rust's iterator pattern.
* **Restricted Eval:** Elimination of eval() functions on untrusted input removes entire attack vectors. API for sandboxed compilation and execution with restricted environmental access, if dynamic code generation is required.
* **Automatic Memory Management:** Automatic garbage collection or Rust-style ownership prevents use-after-free and double-free vulnerabilities that plague manual memory management languages.
* **Resource Cleanup:** RAII (Resource Acquisition Is Initialization) in C++, defer in Go, or automatic cleanup in managed languages ensures resource release even during exceptions.
* **Secure Deletion APIs:** Language-level support for secure memory zeroization (e.g., Rust's `zeroize` crate) prevents sensitive data leakage after use.
* **Algorithm Deprecation:** Built-in deprecation mechanisms (`@Deprecated` in Java, `DeprecationWarning` in Python) enable phased removal of insecure algorithms.
* **Static Type Checking:** Compile-time type verification, explicit conversions, dependent type support. This prevents entire categories of errors at compilation rather than runtime.
* **Restricted Undefined Behavior:** Languages like Go and Python eliminate undefined behavior entirely, while Rust tracks it explicitly through the unsafe keyword. This prevents compiler-dependent security properties that vary across platforms and optimization levels.
* **Comprehensive Exception Handling:** Languages must support catching specific exception types, preventing silent failures from bare `catch(Exception)` blocks. Rust's Result type and Go's explicit error handling represent modern approaches. Ensure that user-visible exceptions do not contain sensitive security information.
* **Secret Scanning:** Language-level source code analysis rejecting hardcoded credentials patterns prevents accidental secret commits. GitGuardian, TruffleHog scanning for hardcoded credentials. Compiler plugins or linters integrated into CI/CD pipelines. Type system enforcement preventing keys from being serialized, logged, or compared unsafely.
- **Constant-time coding verification** - Verify that functions for cryptographic operations have execution time independent of secret values. Ensure execution paths, instruction access patterns, and memory access patterns don't depend on secrets. Compiler must analyze execution paths, instruction patterns, and memory access patterns to verify timing independence. Examples: RustCrypto's constant-time implementations; libsodium's constant-time functions.
* **Secure Boot Support:** TPM integration, hardware attestation, secure enclave support (ARM TrustZone, Intel SGX).
* **Compiler Warnings:** `-Wall -Wextra -Werror`, `-Wimplicit-fallthrough`, `-fsanitize=address` flags enabling detection at build time.
* **Advanced Static Analysis:** Coverity, SonarQube, Clang Static Analyzer, Infer for deep program analysis detecting null pointer dereferences, buffer overflows.
* **Dead Code Detection:** Meta's SCARF, compiler elimination, coverage analysis identifying unreachable code.
* **Memory Instrumentation:** AddressSanitizer (ASan), MemorySanitizer (MSan), UndefinedBehaviorSanitizer (UBSan), Valgrind detecting memory errors at runtime.
- **Memory Segmentation:** Separation of code, stack, heap, and data segments with non-executable stack/heap (NX bit), ASLR, and DEP preventing code injection attacks. Separate heaps for sensitive data such as cryptographic keys, with strict access controls.
* **Fuzzing:** Coverage-guided fuzzing (AFL, libFuzzer), mutation analysis, symbolic execution (KLEE) discovering crash-inducing inputs.
* **Code Coverage:** Branch and path coverage measurement ensuring test adequacy.
* **Dependency Vulnerability Scanning:** Snyk, Black Duck, Trivy performing Software Composition Analysis (SCA), SBOM generation, license compliance. Version pinning and lockfiles (Cargo.lock, package-lock.json).
* **Dynamic Application Security Testing:** OWASP ZAP, Burp Suite detecting web vulnerabilities, SQL injection, XSS.
* **Binary Analysis:** Ghidra, IDA Pro, Radare2 for reverse engineering detection, debug symbol removal verification, backdoor pattern detection.
* **Cyclomatic Complexity:** SonarQube, Lizard flagging functions exceeding thresholds.
* **Automated Formatting:** Black (Python), Prettier (JavaScript), rustfmt enforcing consistent style.
* **Integrity Monitoring:** Osquery, Wazuh, AIDE detecting tampering with production binaries.
* **Symbolic Execution:** KLEE, SMT solvers exploring all feasible paths, constraint solving for property verification (LTL, CTL, etc.).
* **Theorem Proving:** Coq, Isabelle, TLA+ for mathematical correctness proofs of critical algorithms.
* **Capability-Based Access Control** — Enforces WHAT operations are allowed (CHERI, Capstone, JWT-based tokens)
* **Taint Tracking and Information Flow Control** — Tracks WHERE data came from and prevents unsafe usage
* **Threat Modeling Integration** — Tools like ThreatSpec, Microsoft Threat Modeling Tool integrated into the development lifecycle. Specify roles, threats, mitigations, and risk assessments alongside code - mitigates, accepts, transfers, exposes, connects, tests, review relationships. Produce prioritized threat lists, attack trees, and security requirements. Compiler verifies that conflicting roles cannot be held simultaneously.

### Library-Level Requirements
* **Secure Hash Functions:** SHA-256, SHA-3, BLAKE2 implementations providing collision and pre-image resistance.
* **Key Derivation:** Argon2id for passwords (resistant to GPU/ASIC attacks), HKDF for PRF-based key derivation, PBKDF2 with appropriate iterations.
* **Authenticated Encryption:** AES-GCM, ChaCha20-Poly1305 combining confidentiality and integrity in single operations.
* **Public Key Cryptography:** RSA with OAEP/PSS padding, elliptic curves (ECDH, ECDSA), post-quantum candidates (Kyber, Dilithium). Examples: libsodium, cryptography.io, RustCrypto, OpenSSL
* **Parameterized Queries:** Database adapters enforcing prepared statements with placeholders separating logic from data. Examples: SQLAlchemy, JDBC, Diesel.
* **Output Encoding:** Context-aware encoding libraries (HTML entities, JavaScript escaping, URL encoding) preventing injection attacks.
* **Template Engines:** Auto-escaping template systems (Jinja2, Django, Tera) by default.
* **XML/JSON Parsing:** Safe parsers disabling external DTDs, entity expansion, with schema validation support.
* **TLS/SSL Libraries:** OCSP stapling, CRL checking, certificate transparency integration, Perfect Forward Secrecy support. Examples: OpenSSL, BoringSSL, rustls.
* **Connection Pooling:** HikariCP (Java), asyncpg (Python), sqlx (Rust) managing connection lifecycles, idle timeouts, resource limits.
* **Authentication Protocols:** JOSE libraries, OAuth 2.0 SDKs implementing replay prevention, mutual authentication.
* **Physical Attack Mitigation:** Libraries implementing fault detection, power analysis protections (masking, blinding), side-channel resistance. Example: libsodium's constant-time implementations.
* **Regular Expression Safety:** RE2 and other non-backtracking engines preventing catastrophic backtracking and ReDoS (Regular Expression Denial of Service) attacks.
* **Constant-time Cryptography:** Libraries ensuring execution time independent of secret values. Examples: RustCrypto's constant-time implementations; libsodium's constant-time functions.
* **Logging Frameworks** — Structured audit and logging libraries (Log4j2, Serilog, Zap) with built-in security event types, log levels, and secure storage backends. Detailed enough to identify attacks, including UTC timestamp information. They should avoid sensitive information if possible. Regardless, logs must be considered as assets, as an attacker will often wish to erase their tracks.

### Application-Level Requirements
* **Credential Policies:** Forced password changes on first boot, aging enforcement (90-120 days), reuse prevention (5+ previous generations), complexity requirements, account lockout (5 failures, 15+ minute lock).
* **Multi-Factor Authentication:** TOTP support, hardware security keys, SMS OTP (lower security), out-of-band confirmations.
* **Session Management:** Cryptographically secure random tokens (not predictable IDs), TTL enforcement (15-30 min sensitive operations, 8 hour maximum), httpOnly/Secure cookie flags, logout invalidation, concurrent session limits.
* **Default Account Hardening:** Remove generic accounts, mandatory credential change at first boot, strong random initial passwords.
* **Path Traversal Prevention:** Whitelist-based access, block traversal sequences (`..`, `/`, `./`), symlink resolution validation, mapping tables preventing direct filename use.
* **Injection Attack Prevention:** Parameterized queries, format validation (type, length, pattern), least-privilege database accounts, schema validation.
* **Command Execution Safety:** Avoid system() calls, allowlist permitted commands, parameterized execution without shell parsing.
* **Rate Limiting:** API throttling, brute-force exponential backoff, input size limits, algorithmic complexity bounds, per-operation resource quotas.
* **Error Messages:** Generic user-facing messages while maintaining detailed server-side logging, no stack traces exposed, debug mode disabled in production, centralized exception handling.
* **Security Event Logging:** All login attempts, access control decisions, privilege escalations, configuration changes.
* **Audit Trails:** Log tamper-proofing via digital signatures/HMACs, centralized aggregation, retention (90+ days minimum), immutable storage, real-time alerting.
* **TLS Configuration:** Enforce TLS 1.2+, AEAD-only cipher suites, HSTS headers, certificate domain matching, certificate pinning for critical endpoints.
* **Certificate Validation:** Expiration checking, transparency log verification, revocation status checking, extended validation for high-assurance services.
* **API Security:** Rate limiting, token/API key management, request signing, CORS policies, API gateway filtering.
* **Encryption:** At-rest encryption for sensitive data, TLS for transit, encrypted backups, key rotation policies, secure HSM/vault storage.
* **Sensitive Data Handling:** Data classification, purpose limitation, retention with secure deletion, minimization, de-identification when feasible.
* **Key Management:** Unique salts per user, application-level pepper stored separately, rotation schedules, HSM storage, key splitting for escrow.
* **Random Generation:** CSPRNG re-seeding, random IVs/nonces per encryption, secure random token generation.
* **Hash Operations:** SHA-256 minimum, Argon2id for passwords, HMAC for integrity, salting for collision resistance.
* **Secure Coding Standards:** Naming conventions, code style guides, API documentation with security implications, threat model documentation.
* **Code Review:** Peer review before merge, multiple reviewers for sensitive code, security-focused checklists, documentation verification.
* **Testing:** Unit tests for security-critical functions, integration testing, negative testing (invalid inputs), boundary value testing, coverage targets.
* **Deployment:** Code signing, binary verification, staged rollouts with monitoring, rollback procedures, security release notes.
* **Build System:** Secure build environment, artifact signing, source access controls, dependency pinning, reproducibility verification.
* **Supply Chain:** SBOM maintenance, dependency tracking, vulnerability notifications, internal package registries, build access restrictions.
* **Firmware Updates:** Updateable firmware design, cryptographic verification, OTA capability, end-of-life planning, version deprecation.
* **Architecture:** Layered security controls, least privilege enforcement, zero-trust models, compartmentalization via containers/VMs, fail-secure defaults.
* **Monitoring:** Security event monitoring, anomaly detection, incident response procedures, forensic analysis capability, threat intelligence.
* **User Interface:** Confirmation dialogs for dangerous operations, clear consequence explanation, time-based confirmation, out-of-band confirmation for critical operations.
* **Compliance:** Security audits, vulnerability disclosure policies, incident response plans, data protection agreements, regulatory compliance tracking.
* **Security Training:** Developer awareness, secure coding education, OWASP Top 10 familiarity, cryptographic concepts, threat modeling.
* **Vulnerability Management:** Bug bounty programs, issue disclosure processes, patch development, release coordination, post-incident reviews.
* **Third-Party Risk:** Vendor security assessment, contract security requirements, SLA clauses, audit rights, data handling agreements.
* **Risk Management:** Threat modeling, risk assessment documentation, mitigation prioritization, residual risk acceptance, control effectiveness measurement.

## Cryptography

timing attacks
most of the time our time is very precious and so we want to speed up things and do multiple operations at once
but with crypto the time may reveal information about the secret key
we can do clever tricks like making a fixed length loop
we have this drive for speed but optimization can also break the cryptographic properties

valgrind or address sanitizer for checking for memory problems
it's really helpful to have tools that work on binary so you don't have to worry about getting into the whole compilation process
taint tracking

processors themselves are unpredictable - 3 to 7 cycles for an add on an ARM cortex
micro-implementations for each platform and processor - multiplies surface area

formal methods - how to express properties/proofs succinctly? Coq etc. are a pain to use, most mathematicians don't use them
\- specify program
\- specify mathematical input-output relationship
\- prove that you have that input-output relationship for that software

nevertheless EverCrypt managed to make a formally verified crypto library - the code really has the maximum assurance of any code that we've seen for cryptography
it's written in C and Coq

testing - find a bug, make tests that will catch that bug, add that to your regression test suite, and make sure it never happens again. Then you get e.g. test vectors which make sure no implementation has that bug. fuzzing - test a space rather than just hand-written cases.
but - in Falcon 2, all of the implementations that were released had the same test vectors and the same leaking bug. "this shows that the traditional development methodology (even when being super careful) has failed"
with even the most advanced fuzzing you're not testing all of the possible inputs - millions of security holes consist of the attacker finding some input which nobody thought of testing for. it's just some obscure kind of input where the attacker says, "haha - if I input exactly that length after setting off the following condition, then the following weird thing is going to happen and I can take advantage of it".

so how do you deal with this?
\- it's very low probability, so known answer tests are not going to find this
\- proving correctness is tedious
\- symbolic testing

you build up a computation DAG showing how the arithmetic from these inputs inputs gives you some output through a series of computations and then you analyze this graph and say "yes, this works correctly".

anger - works on binaries, starts from valgrind. it has this cool GUI called anger management
Manticore supposedly can do the same thing from binary and comes with a lot of the same kinds of analyses

but if the SMT solvers aren't smart enough to see that the resulting code works, then you have to build some new tools

compared to all the proving tools, doing symbolic testing/symbolic execution is actually fun
