Security
########

Stroscot aims to have built-in security features. This means providing security functionality in the standard library, such as encryption algorithms and communication protocols, but also designing the library and the language so that it is easy to write secure code and hard to write insecure code.

Best practices
==============

At the moment no programming language has a good track record of being secure. `WhiteSource <https://www.mend.io/blog/is-one-programming-language-more-secure/>`__ concluded in 2019 that Ruby had the lowest number of reported vulnerabilities out of popular web languages, but even Ruby had `5 vulnerabilities <https://www.cvedetails.com/vulnerability-list/vendor_id-7252/product_id-12215/Ruby-lang-Ruby.html>`__ in 2022 in the base runtime, and it easy to write security antipatterns that lead to issues such as SQL injection. To improve in this area, there are various security-related features. Some are implemented in new languages, for example, Rust's borrows checker and Rune's "secrets" mechanism, but most are more obscure and live in academic papers and third-party analysis tools.

As `Hare <https://harelang.org/blog/2022-06-21-safety-features/>`__ writes, "how do we evaluate the trade-offs of a particular safety feature?" Hare proposes to evaluate against the language goals. Stroscot's goals are functionality and minimality in that order. For security, functionality means protecting against all types of security risks, both common and uncommon. This is somewhat daunting as for example `CWE <https://cwe.mitre.org/data/index.html>`__ lists 933 weaknesses. Certainly it would be good to go through all these but I don't have much time. Fortunately, there is the OWASP Application Security Verification Standard. It points out that the CWE is quite duplicative and condenses it down to `286 requirements <https://github.com/OWASP/ASVS/blob/d8fde8b6592af2b8022590ec9d9a1765fe920651/4.0/docs_en/OWASP%20Application%20Security%20Verification%20Standard%204.0.3-en.csv>`__. The `bleeding edge version 5 <https://github.com/OWASP/ASVS/tree/d8fde8b6592af2b8022590ec9d9a1765fe920651/5.0/en>`__ further condenses it, removing more duplicate and out of scope requirements. The OWASP ASVS is certainly not comprehensive, as it is web-focused and community-developed, but it is definitely one of the leading standards. Implementing support for it in Stroscot is certainly a good first step, and goes above and beyond what any currently-popular language is doing. For now, I will focus only on the ASVS v5 unreleased version, and leave the mitigation of other weaknesses to the future.

Threat modeling
===============

Threat modeling consists of three steps: build an abstraction of the system, identify and quantify threats, and address the most prominent. The abstraction of the application is essentially a graph. ASVS calls it a data-flow diagram, but I've looked at 10 DFDs and seen 10 different styles of graph, so this term seems meaningless. Based on this graph, threats are then identified and quantified.

Application model
-----------------

The elements of the model are as follows:

* Network topology graph: nodes for clients, servers, firewalls, and other networked devices and services, and edges for their possible and expected dataflows (with ports). Devices may be identified by their name and description.
* Entry points: this is more detailed information on open ports which accept connections, such as for an HTTP port its list of accessible/valid web URLs.
* Exit points: This is the other direction, ports where data is sent out. XSS and information disclosure vulnerabilities both require an exit point for the attack to complete.
* Assets: things attackers will target or degrade, such as credentials, session cookies, uptime, personal data, business information, execution privileges, and logs. The costs of leaking, losing and replacing, or tampering with each asset should be quantified.
* Protection requirements: All assets should be assigned protection levels and associated protection requirements, such as encryption requirements, integrity requirements, retention, privacy and other confidentiality requirements.
* Security controls: entry points that require a password, cookie, cryptographic secret, or other credential. These credentials should follow key management policies such as NIST SP 800-57 and NIST SP 800-63-3.
* Trust roles: Actors in the system will have different assets at their disposal. The roles specify what credentials an actor may be assumed to have and not have, as well as the assets that each actor should and should not be able to access (is authorized to access). For example, a casual user will have access to one client node and no credentials, and should not be able to access anything, while an authorized sysadmin will have a full set of credentials and should have full access. Although permissions should be allocated using roles, individual security controls should use feature-based access control.
* Attacker types: There is a wide variety of attackers - script kiddies, hacktivists, organized crime / financially motivated, nation-states, advanced persistent threats, insiders, competitors. They have different goals and may be able to assume different trust roles depending on the situation.

This is just a basic outline. The model should be detailed enough to automatically detect and enumerate threats. Basically, for each potential threat, such as enumerated by STRIDE (Spoofing, Tampering, Repudiation, Information disclosure, Denial of service, Elevation of privilege), and each potential weakness, such as enumerated by the CWE list, the model must have sufficient information to identify if that weakness could be exploited to activate that threat and violate the parameters of the trust roles.

`Threatspec <https://github.com/threatspec/threatspec>`__ is one example of how to do modeling, keeping the annotations in the code ensures they are easy to keep in sync. Threats and controls are just identifier strings. Components are identified by a path. There are then various annotations:

* @mitigates (?P<component>.*?) against (?P<threat>.*?) with (?P<control>.*)
* @accepts (?P<threat>.*?) to (?P<component>.*?) with (?P<details>.*)
* @transfers (?P<threat>.*?) from (?P<source_component>.*?) to (?P<destination_component>.*?) with (?P<details>.*)
* @exposes (?P<component>.*?) to (?P<threat>.*?) with (?P<details>.*)
* @connects (?P<source_component>.*?) (?P<direction>with|to) (?P<destination_component>.*?) with (?P<details>.*)
* @tests (?P<control>.*?) for (?P<component>.*)
* @review (?P<component>.*?) (?P<details>.*)

Threat quantification
---------------------

Given the initial list of possible threats, they can be categorized by type and prioritized by risk. The generic definition of risk is the product of two factors: probability that the threat occurs, and cost to the organization. In the DREAD model, probability is split into reproducibility, exploitability and discoverability, while cost is split into damage and affected users/components (extent). The CWE list has statistics on threat incidence. The costs can be entered into and calculated from the model, or simply rated based on vulnerability type as 1-5. Multi-stage attack paths can be aggregated and summarized into attack trees and other reports. Countermeasures that mitigate the threat should also be listed. Based on this prioritized list, the developer can then mark threats as acceptable, or modify the application to eliminate or mitigate the threat.

Logging
-------

Logging is important for security and a common logging approach should be used across the system. The logs should be detailed enough to identify attacks, including UTC timestamp information. They should avoid sensitive information if possible. Regardless, logs must be considered as assets, as an attacker will often wish to erase their tracks.

Cryptography
============

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
- specify program
- specify mathematical input-output relationship
- prove that you have that input-output relationship for that software

nevertheless EverCrypt managed to make a formally verified crypto library - the code really has the maximum assurance of any code that we've seen for cryptography
it's written in C and Coq

testing - find a bug, make tests that will catch that bug, add that to your regression test suite, and make sure it never happens again. Then you get e.g. test vectors which make sure no implementation has that bug. fuzzing - test a space rather than just hand-written cases.
but - in Falcon 2, all of the implementations that were released had the same test vectors and the same leaking bug. "this shows that the traditional development methodology (even when being super careful) has failed"
with even the most advanced fuzzing you're not testing all of the possible inputs - millions of security holes consist of the attacker finding some input which nobody thought of testing for. it's just some obscure kind of input where the attacker says, "haha - if I input exactly that length after setting off the following condition, then the following weird thing is going to happen and I can take advantage of it".

so how do you deal with this?
- it's very low probability, so known answer tests are not going to find this
- proving correctness is tedious
- symbolic testing

you build up a computation DAG showing how the arithmetic from these inputs inputs gives you some output through a series of computations and then you analyze this graph and say "yes, this works correctly".

anger - works on binaries, starts from valgrind. it has this cool GUI called anger management
Manticore supposedly can do the same thing from binary and comes with a lot of the same kinds of analyses

but if the SMT solvers aren't smart enough to see that the resulting code works, then you have to build some new tools

compared to all the proving tools, doing symbolic testing/symbolic execution is actually fun

